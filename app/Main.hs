{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (sort)
import Data.Text

import System.Process

import Control.Concurrent (MVar, forkIO, newMVar, threadDelay)
import Control.Monad (forever)
import DBus
import DBus.Client
import DBus.Internal.Types
import Data.Int (Int32)
import Data.Map
import Data.Word (Word32)
import State

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
notify state appName replaceId appIcon summary body actions hints timeout = do
  putStrLn $ "received notification: " ++ unpack summary

  let hintString = buildHintString hints
  let notification = buildEwwNotification appName appIcon summary body hintString
  putStrLn $ setEwwValue "end-notifications" notification
  callCommand $ setEwwValue "end-notifications" notification
  return 12

buildHintString :: Map Text Variant -> String
buildHintString = Data.Map.foldrWithKey (\k v s -> s ++ showEntry k v) ""
 where
  showEntry k v = "(" ++ unpack k ++ "," ++ show v ++ ")"
  show (Variant x) = showValue True x

buildEwwNotification :: Text -> Text -> Text -> Text -> String -> String
buildEwwNotification appName appIcon summary body hints =
  "(end-notification :end-appname \""
    ++ unpack appName
    ++ "\" :end-appicon \""
    ++ unpack appIcon
    ++ "\" :end-summary \""
    ++ unpack summary
    ++ "\" :end-body \""
    ++ unpack body
    ++ "\" :end-hints \""
    ++ hints
    ++ "\")"

setEwwValue :: String -> String -> String
setEwwValue var val = "eww update " ++ var ++ "='" ++ val ++ "'"

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
  let run act = runStateIO act notifyState

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
