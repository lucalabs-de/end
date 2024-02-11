{-# LANGUAGE OverloadedStrings #-}

module Daemon (main) where

import Control.Concurrent (
  forkIO,
  newEmptyMVar,
  newMVar,
  readMVar,
  threadDelay,
 )
import Control.Concurrent.AtomicModify (atomicModifyStrict)
import Control.Monad (forever, unless, void, when)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import DBus (
  signal,
  signalBody,
  toVariant,
 )
import DBus.Client (
  Interface (interfaceName),
  autoMethod,
  connectSession,
  defaultInterface,
  emit,
  export,
  interfaceMethods,
  nameAllowReplacement,
  nameReplaceExisting,
  requestName,
 )
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (split, unpack)
import Data.Int (Int32)
import qualified Data.Map as Map
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
  EwwWindow,
  Timeout (byUrgency),
  importConfig,
  (//), Config (..),
 )
import Data.Maybe (fromJust, fromMaybe, isJust)
import System.Directory.Internal.Prelude (exitFailure)

import Data.Bifunctor (second)
import Data.Time.Clock.System (SystemTime (systemSeconds), getSystemTime)

import Util.Builders
import Util.Constants
import Util.DbusNotify
import Util.Helpers
import Util.ImageConversion (wipeImageDirectory, writeImageDataToPng)

getServerInformation :: IO (Text, Text, Text, Text)
getServerInformation =
  return
    ( "haskell-notification-daemon"
    , "eww"
    , "1.2.0"
    , "1.2"
    )

getCapabilities :: IO [String]
getCapabilities =
  return
    [ "body"
    , "hints"
    , "persistence"
    , "icon-static"
    , "action-icons"
    , "actions"
    ]

nextId :: NState -> IO Word32
nextId state = atomicModifyStrict state (second idCounter . tuple . updateIdCounter (+ 1))

openEwwWindow :: EwwWindow -> IO ()
openEwwWindow = callCommand . buildWindowOpenCommand

closeEwwWindow :: EwwWindow -> IO ()
closeEwwWindow = callCommand . buildWindowCloseCommand

removeAfterTimeout :: NState -> Maybe EwwWindow -> Word32 -> Word32 -> IO ()
removeAfterTimeout state window id timeout = do
  threadDelay (fromIntegral timeout * 1000 * 1000)
  removeNotification state window id
  emitNotificationClosed state id Expired

removeNotification :: NState -> Maybe EwwWindow -> Word32 -> IO ()
removeNotification state window id = do
  l <- atomicModifyStrict state (tuple . updateNotifications (filter (\n -> nId n /= id)))
  displayNotifications window $ notifications l
  when (null $ notifications l) wipeImageDirectory

-- Implements org.freedesktop.Notifications.ActionInvoked
invokeAction :: NState -> Word32 -> String -> IO ()
invokeAction state id key = do
  s <- readMVar state

  let signalEmpty =
        signal
          "/org/freedesktop/Notifications"
          "org.freedesktop.Notifications"
          "ActionInvoked"

  let signalWithBody = signalEmpty{signalBody = [toVariant id, toVariant key]}

  emit (client s) signalWithBody

-- Implements org.freedesktop.Notifications.NotificationClosed
emitNotificationClosed :: NState -> Word32 -> NotificationCloseReason -> IO ()
emitNotificationClosed state id reason = do
  s <- readMVar state

  let signalEmpty =
        signal
          "/org/freedesktop/Notifications"
          "org.freedesktop.Notifications"
          "NotificationClosed"
  let reasonId = fromIntegral (fromEnum reason + 1) :: Word32

  let signalWithBody = signalEmpty{signalBody = [toVariant id, toVariant reasonId]}

  emit (client s) signalWithBody

-- Implements org.freedesktop.Notifications.CloseNotification
closeNotification :: NState -> Word32 -> IO ()
closeNotification state id = do
  s <- readMVar state
  let cfg = config s

  let window = cfg // ewwWindow
  removeNotification state window id
  emitNotificationClosed state id CloseNotification

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
notify state appName replaceId appIcon summary body actions hints _ = do
  s <- readMVar state
  let cfg = config s

  -- parse any possible binary image data
  maybeFixedHints <- runMaybeT $ do
    imageData <- liftMaybe $ getImageDataHint hints "image-data"
    imageName <- lift $ show . systemSeconds <$> getSystemTime
    imagePath <- lift $ toVariant <$> writeImageDataToPng imageName imageData
    return $ Map.insert "image-path" imagePath $ Map.delete "image-data" hints

  let sanitizedHints = fromMaybe hints maybeFixedHints
  let currentUrgency = configKeyFromUrgency (getUrgency hints)

  timestamp <- getSystemTime
  notificationId <-
    if replaceId /= 0
      then return replaceId
      else nextId state

  let notification =
        Notification
          { nId = notificationId
          , nTimeout = cfg // timeout // byUrgency // currentUrgency
          , nTimestamp = timestamp
          , notifyType = Nothing
          , appName = appName
          , appIcon = appIcon
          , summary = summary
          , body = body
          , hints = sanitizedHints
          , actions = actions
          , widget = cfg // ewwDefaultNotificationKey
          }

  handleNewNotification state notification

handleNewNotification :: NState -> Notification -> IO Word32
handleNewNotification state notification = do
  notificationState <- readMVar state

  let cfg = config notificationState
  let window = cfg // ewwWindow
  let maxN = cfg // maxNotifications

  let notifyTransform ns = do
        let (r, l) = tryReplace (\n -> nId n == nId notification) notification ns
        if not r
          then
            if maxN > 0 && length l >= fromIntegral maxN
              then do
                let oldest = minWith lifetime l
                replaceOrPrepend (== oldest) notification l
              else notification : l
          else l

  newState <-
    atomicModifyStrict state $
      tuple . updateNotifications notifyTransform

  when (nTimeout notification /= 0) $
    void . forkIO $
      removeAfterTimeout state window (nId notification) (nTimeout notification)

  displayNotifications window (notifications newState)
  return $ nId notification

displayNotifications :: Maybe EwwWindow -> [Notification] -> IO ()
displayNotifications w l =
  if not $ null l
    then do
      let widgetString = buildWidgetWrapper True $ replaceNewlines $ buildWidgetString l
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
  unless (BS.null msg) $ do
    let command : args = unpack <$> split ' ' msg
    evalCommand state barrier command args

evalCommand :: NState -> Barrier -> String -> [String] -> IO ()
evalCommand _ barrier "kill" _ = unlockBarrier barrier
evalCommand state _ "action" params = invokeAction state (read (head params)) (params !! 1)
evalCommand state _ "close" params = do
  s <- readMVar state
  let cfg = config s
  let id = (read (head params))
  removeNotification state (cfg // ewwWindow) id
  emitNotificationClosed state id Dismiss
evalCommand _ _ _ _ = return ()

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
        , idCounter = 0
        , client = client
        }

  export
    client
    "/org/freedesktop/Notifications"
    defaultInterface
      { interfaceName = "org.freedesktop.Notifications"
      , interfaceMethods =
          [ autoMethod "GetServerInformation" getServerInformation
          , autoMethod "GetCapabilities" getCapabilities
          , autoMethod "CloseNotification" (closeNotification notifyState)
          , autoMethod "Notify" (notify notifyState)
          ]
      }

  terminationBarrier <- newEmptyMVar

  _ <- forkIO $ setupIpcSocket notifyState terminationBarrier
  waitAtBarrier terminationBarrier
