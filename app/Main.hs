{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (sort)
import Data.Text

import System.Process

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import DBus
import DBus.Client
import Data.Int (Int32)
import Data.Word (Word32)
import Data.Map 

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

closeNotification :: IO ()
closeNotification = return ()

notify ::
  Text -> -- application name
  Word32 -> -- replaces id
  Text -> -- app icon
  Text -> -- summary
  Text -> -- body
  [Text] -> -- actions
  Map Text Variant -> -- hints
  Int32 -> -- timeout
  IO Word32
notify appName replaceId appIcon summary body actions hints timeout = do
  putStrLn $ "received notification: " ++ unpack summary
  callCommand $ setEwwValue "end-appname" appName
  callCommand $ setEwwValue "end-appicon" appIcon
  callCommand $ setEwwValue "end-summary" summary
  callCommand $ setEwwValue "end-body" body
  return 12

setEwwValue :: Text -> Text -> String
setEwwValue var val = "eww update " ++ unpack var ++ "='" ++ unpack val ++ "'"

main :: IO ()
main = do
  client <- connectSession
  _ <-
    requestName
      client
      "org.freedesktop.Notifications"
      [nameAllowReplacement, nameReplaceExisting]

  export
    client
    "/org/freedesktop/Notifications"
    defaultInterface
      { interfaceName = "org.freedesktop.Notifications"
      , interfaceMethods =
          [ autoMethod "GetServerInformation" getServerInformation
          , autoMethod "GetCapabilites" getCapabilites
          , autoMethod "CloseNotification" closeNotification
          , autoMethod "Notify" notify
          ]
      }

  forever $ threadDelay (maxBound - 1)
