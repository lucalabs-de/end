{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (id)

import Data.List (sort)
import Data.Text hiding (foldr)

import System.Process

import Control.Concurrent (MVar, forkIO, newMVar, readMVar, threadDelay)
import Control.Monad (forever)
import DBus
import DBus.Client
import DBus.Internal.Types
import Data.Int (Int32)
import Data.Map hiding (foldr)
import Data.Word (Word32)
import State
import Util.Builders

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
  let notification =
        Notification
          { nId = 1 + getLastId (notifications notificationState)
          , timeout = -1
          , notifyType = Nothing -- TODO parse from hints
          , appName = appName
          , appIcon = appIcon
          , summary = summary
          , body = body
          , hintString = hintString
          }

  let notifications' = notification : notifications notificationState

  let widgetString = buildWidgetString (notifications notificationState)

  callCommand $ setEwwValue "end-notifications" widgetString
  return $ nId notification

getLastId :: [Notification] -> Word32
getLastId = foldr (\n c -> max c (nId n)) 0

-- TODO: add state monad to accomplish the following
-- keep a list of notifications in memory (together with their ids),
-- remove notifications after timeout,
-- update the value of the eww variable end-notifications based on currently
-- displayed notifications,
-- replace notifications if replaceId is 0

main :: IO ()
main = do
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
