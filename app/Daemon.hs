{-# LANGUAGE OverloadedStrings #-}

module Daemon (main) where

import Prelude hiding (id)

import Data.List (sort)
import Data.Text hiding (foldr)

import System.Process

import Control.Concurrent
import Control.Concurrent.AtomicModify
import Control.Monad (forever)
import DBus
import DBus.Client hiding (listen)
import DBus.Internal.Types
import Data.Int (Int32)
import Data.Map hiding (foldr)
import Data.Word (Word32)
-- import GHC.IO.Handle (BufferMode (NoBuffering), hSetBuffering)
-- import GHC.IO.IOMode (IOMode (ReadMode))
import System.IO
import Network.Socket
import State
import Util.Builders
import Util.Constants (ipcSocketAddr)
import Util.DbusNotify (getStringHint, hintKeyNotifyType)
import Util.Helpers (getMax, tuple)
import Data.Maybe (listToMaybe, fromMaybe)

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

removeAfterTimeout :: MVar NotificationState -> Word32 -> Int -> IO ()
removeAfterTimeout state id timeout = do
  threadDelay (timeout * 1000 * 1000)
  l <- atomicModifyStrict state (tuple . NotificationState . Prelude.filter (\n -> nId n /= id) . notifications)
  displayNotifications $ notifications l

closeNotification :: MVar NotificationState -> Word32 -> IO ()
closeNotification state id = do
  undefined 

notify ::
  MVar NotificationState ->
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

-- TODO: add state monad to accomplish the following
-- keep a list of notifications in memory (together with their ids),
-- remove notifications after timeout,
-- update the value of the eww variable end-notifications based on currently
-- displayed notifications,
-- replace notifications if replaceId is 0

setupIpcSocket :: IO ()
setupIpcSocket = do
  sock <- socket AF_UNIX Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock ipcSocketAddr
  listen sock 2

  socketLoop sock

socketLoop :: Socket -> IO ()
socketLoop sock = do
  conn <- accept sock
  hdl <- socketToHandle sock ReadMode
  hSetBuffering hdl NoBuffering

  command <- fmap Prelude.init (hGetLine hdl)
  putStrLn command
  
  hClose hdl

  socketLoop sock

evalCommand :: String -> [String] -> IO a
evalCommand "close" params = do 
  undefined


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

  forkIO setupIpcSocket

  forever $ threadDelay (maxBound - 1)
