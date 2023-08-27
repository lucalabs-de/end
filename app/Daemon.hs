{-# LANGUAGE OverloadedStrings #-}

module Daemon (main) where

import Control.Concurrent (
  forkIO,
  newEmptyMVar,
  newMVar,
  putMVar,
  readMVar,
  swapMVar,
  takeMVar,
  threadDelay,
 )
import Control.Concurrent.AtomicModify (atomicModifyStrict)
import Control.Monad (forever, void, when)
import DBus (Variant)
import DBus.Client (
  Interface (interfaceName),
  autoMethod,
  connectSession,
  defaultInterface,
  export,
  interfaceMethods,
  nameAllowReplacement,
  nameReplaceExisting,
  requestName,
 )
import Data.ByteString.Char8 (split, unpack)
import Data.Int (Int32)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word32)
import Network.Socket (
  Family (AF_UNIX),
  Socket,
  SocketOption (ReuseAddr),
  SocketType (Stream),
  accept,
  bind,
  listen,
  setSocketOption,
  socket,
 )
import Network.Socket.ByteString (recv)
import State
import System.Process (callCommand)

import Config (
  Config (customNotifications, settings),
  CustomNotification (customTimeout, ewwKey, hint, name),
  EwwWindow,
  Settings (ewwDefaultNotificationKey, ewwWindow, timeout),
  Timeout (byUrgency),
  importConfig,
  (//),
 )
import Control.Exception (onException)
import Data.Maybe (fromJust, isJust)
import System.Directory.Internal.Prelude (exitFailure)
import Toml (Value (Bool))

import Data.List (find)
import Util.Builders
import Util.Constants
import Util.DbusNotify
import Util.Helpers

getServerInformation :: IO (Text, Text, Text, Text)
getServerInformation =
  return
    ( "haskell-notification-daemon"
    , "eww"
    , "0.0.1"
    , "1.2"
    )

getCapabilites :: IO [String]
getCapabilites =
  return
    [ "body"
    , "hints"
    , "persistence"
    , "icon-static"
    , "action-icons"
    ]

openEwwWindow :: EwwWindow -> IO ()
openEwwWindow = callCommand . buildWindowOpenCommand

closeEwwWindow :: EwwWindow -> IO ()
closeEwwWindow = callCommand . buildWindowCloseCommand

removeAfterTimeout :: NState -> Maybe EwwWindow -> Word32 -> Word32 -> IO ()
removeAfterTimeout state window id timeout = do
  threadDelay (fromIntegral timeout * 1000 * 1000)
  removeNotification state window id

removeNotification :: NState -> Maybe EwwWindow -> Word32 -> IO ()
removeNotification state window id = do
  l <- atomicModifyStrict state (tuple . updateNotifications (filter (\n -> nId n /= id)))
  displayNotifications window $ notifications l

-- Implements org.freedesktop.Notifications.CloseNotification
closeNotification :: NState -> Word32 -> IO ()
closeNotification state id = do
  s <- readMVar state
  let cfg = config s

  let window = cfg // settings // ewwWindow
  removeNotification state window id

-- Implements org.freedesktop.Notifications.Notify
notify ::
  NState ->
  Text -> -- application name
  Word32 -> -- replaces id
  Text -> -- app icon
  Text -> -- summary
  Text -> -- body
  [Text] -> -- actions
  Hints -> -- hints
  Int32 -> -- timeout (not supported)
  IO Word32
notify state appName replaceId appIcon summary body actions hints timeout = do
  s <- readMVar state
  let cfg = config s

  let customType = do
        customHint <- getStringHint hints hintKeyNotifyType
        find (\s -> hint s == customHint) (customNotifications cfg)

  let notifyFunction = maybe notifyDefault notifyCustom customType
  notifyFunction state appName replaceId appIcon summary body actions hints timeout

notifyDefault ::
  NState ->
  Text -> -- application name
  Word32 -> -- replaces id
  Text -> -- app icon
  Text -> -- summary
  Text -> -- body
  [Text] -> -- actions
  Hints -> -- hints
  Int32 -> -- timeout
  IO Word32
notifyDefault state appName replaceId appIcon summary body actions hints _ = do
  notificationState <- readMVar state
  let cfg = config notificationState

  let currentUrgency = configKeyFromUrgency (getUrgency hints)

  let getLastId = getMax nId 0
  let hintString = buildHintString hints

  let notificationId =
        if replaceId /= 0
          then replaceId
          else 1 + getLastId (notifications notificationState)

  let notification =
        Notification
          { nId = notificationId
          , nTimeout = cfg // settings // timeout // byUrgency // currentUrgency
          , notifyType = Nothing
          , appName = appName
          , appIcon = appIcon
          , summary = summary
          , body = body
          , hintString = hintString
          , widget = cfg // settings // ewwDefaultNotificationKey
          }

  handleNewNotification state notification

notifyCustom ::
  CustomNotification ->
  NState ->
  Text ->
  Word32 ->
  Text ->
  Text ->
  Text ->
  [Text] ->
  Hints ->
  Int32 ->
  IO Word32
notifyCustom custom state appName replaceId appIcon summary body actions hints _ = do
  notificationState <- readMVar state

  let getLastId = getMax nId 0
  let notificationId =
        if replaceId /= 0
          then replaceId
          else 1 + getLastId (notifications notificationState)

  let hintString = buildHintString hints

  let notification =
        Notification
          { nId = notificationId
          , nTimeout = customTimeout custom
          , notifyType = Just (name custom)
          , appName = appName
          , appIcon = appIcon
          , summary = summary
          , body = body
          , hintString = hintString
          , widget = Just (ewwKey custom)
          }

  handleNewNotification state notification

handleNewNotification :: NState -> Notification -> IO Word32
handleNewNotification state notification = do
  notificationState <- readMVar state
  let cfg = config notificationState

  let window = cfg // settings // ewwWindow

  newState <-
    atomicModifyStrict state $
      tuple
        . updateNotifications
          (replaceOrPrepend (\n -> nId n == nId notification) notification)

  when (nTimeout notification /= 0) $
    void . forkIO $
      removeAfterTimeout state window (nId notification) (nTimeout notification)

  displayNotifications window (notifications newState)
  return $ nId notification

displayNotifications :: Maybe EwwWindow -> [Notification] -> IO ()
displayNotifications w l =
  if not $ null l
    then do
      let widgetString = buildWidgetWrapper True $ buildWidgetString l
      putStrLn widgetString
      callCommand $ setEwwValue "end-notifications" widgetString
      mapM_ openEwwWindow w
    else mapM_ closeEwwWindow w

setupIpcSocket :: NState -> Barrier -> IO ()
setupIpcSocket state term = do
  sock <- socket AF_UNIX Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock ipcSocketAddr
  listen sock 2

  socketLoop state sock term

socketLoop :: NState -> Socket -> Barrier -> IO ()
socketLoop state sock barrier = forever $ do
  (client, _) <- accept sock
  msg <- recv client 1024
  let command : args = unpack <$> split ' ' msg
  evalCommand state barrier command args

evalCommand :: NState -> Barrier -> String -> [String] -> IO ()
evalCommand state barrier "kill" params = unlockBarrier barrier
evalCommand state _ "close" params = do
  s <- readMVar state
  let cfg = config s
  removeNotification state (cfg // settings // ewwWindow) (read (head params))

main :: IO ()
main = do
  mConfig <- importConfig
  config <- if isJust mConfig then return mConfig else exitFailure

  client <- connectSession
  _ <-
    requestName
      client
      "org.freedesktop.Notifications"
      [nameAllowReplacement, nameReplaceExisting]

  notifyState <-
    newMVar $
      NotificationState
        { notifications = []
        , config = fromJust config
        }

  export
    client
    "/org/freedesktop/Notifications"
    defaultInterface
      { interfaceName = "org.freedesktop.Notifications"
      , interfaceMethods =
          [ autoMethod "GetServerInformation" getServerInformation
          , autoMethod "GetCapabilites" getCapabilites
          , autoMethod "CloseNotification" (closeNotification notifyState)
          , autoMethod "Notify" (notify notifyState)
          ]
      }

  terminationBarrier <- newEmptyMVar

  forkIO $ setupIpcSocket notifyState terminationBarrier
  waitAtBarrier terminationBarrier
