{-# LANGUAGE OverloadedStrings #-}

module Daemon (main) where

import Control.Concurrent (
  forkIO,
  newMVar,
  readMVar,
  swapMVar,
  threadDelay,
 )
import Control.Concurrent.AtomicModify (atomicModifyStrict)
import Control.Monad (forever)
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

removeAfterTimeout :: NState -> Word32 -> Int -> IO ()
removeAfterTimeout state id timeout = do
  threadDelay (timeout * 1000 * 1000)
  closeNotification state id

closeNotification :: NState -> Word32 -> IO ()
closeNotification state id = do
  l <- atomicModifyStrict state (tuple . NotificationState . filter (\n -> nId n /= id) . notifications)
  displayNotifications $ notifications l

notify ::
  NState ->
  Text -> -- application name
  Word32 -> -- replaces id
  Text -> -- app icon
  Text -> -- summary
  Text -> -- body
  [Text] -> -- actions
  Map Text Variant -> -- hints
  Int32 -> -- timeout
  IO Word32
notify state appName replaceId appIcon summary body actions hints _ = do
  notificationState <- readMVar state

  let hintString = buildHintString hints
  let getLastId = getMax nId 0

  let notification =
        Notification
          { nId = 1 + getLastId (notifications notificationState)
          , timeout = 10 -- TODO read this from config depending on urgency
          , notifyType = getStringHint hints hintKeyNotifyType
          , appName = appName
          , appIcon = appIcon
          , summary = summary
          , body = body
          , hintString = hintString
          , widget = Nothing
          }

  let notifications' = notification : notifications notificationState
  swapMVar state (NotificationState notifications')

  forkIO $ removeAfterTimeout state (nId notification) (timeout notification)
  displayNotifications notifications'

  return $ nId notification

displayNotifications :: [Notification] -> IO ()
displayNotifications l = do
  let widgetString = buildWidgetWrapper True $ buildWidgetString l
  putStrLn widgetString
  callCommand $ setEwwValue "end-notifications" widgetString

launchEwwWindow :: String -> IO ()
launchEwwWindow = callCommand . buildWindowCommand

setupIpcSocket :: NState -> IO ()
setupIpcSocket state = do
  sock <- socket AF_UNIX Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock ipcSocketAddr
  listen sock 2

  socketLoop state sock

socketLoop :: NState -> Socket -> IO ()
socketLoop state sock = forever $ do
  (client, _) <- accept sock
  msg <- recv client 1024
  let command : args = unpack <$> split ' ' msg
  evalCommand state command args

evalCommand :: NState -> String -> [String] -> IO ()
evalCommand state "close" params = closeNotification state $ read (head params)
evalCommand state "kill" params = undefined -- TODO clean up properly

main :: IO ()
main = do
  launchEwwWindow "notification-frame"

  client <- connectSession
  _ <-
    requestName
      client
      "org.freedesktop.Notifications"
      [nameAllowReplacement, nameReplaceExisting]

  notifyState <- newMVar $ NotificationState []

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

  forkIO $ setupIpcSocket notifyState

  forever $ threadDelay (maxBound - 1)
