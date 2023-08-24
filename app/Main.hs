{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (id)

import Data.List (sort)
import Data.Text hiding (foldr)

import System.Process

import Control.Concurrent (MVar, forkIO, modifyMVar_, newMVar, putMVar, readMVar, swapMVar, threadDelay)
import Control.Concurrent.AtomicModify
import Control.Monad (forever)
import DBus
import DBus.Client
import DBus.Internal.Types
import Data.Int (Int32)
import Data.Map hiding (foldr)
import Data.Word (Word32)
import State
import Util.Builders
import Util.DbusNotify (getStringHint, hintKeyNotifyType)
import Util.Helpers (getMax, tuple)

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

closeNotification :: MVar NotificationState -> IO ()
closeNotification state = return ()

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

  forever $ threadDelay (maxBound - 1)
