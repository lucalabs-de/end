{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (sort)
import Data.Text

import DBus
import DBus.Client

getServerInformation :: IO (Text, Text, Text, Text)
getServerInformation = 
  return
    ( "haskell-notification-daemon"
    , "eww"
    , "0.0.1"
    , "1.2" )

getCapabilites :: IO [String]
getCapabilites =
  return 
     [ "body"
      , "hints"
      , "actions"
      , "persistence"
      , "icon-static"
      , "action-icons"
     ]

onCloseNote :: Signal -> IO ()

onNote :: Signal -> IO ()

main :: IO ()
main = do
  client <- connectSession
  _ <- requestName client "org.freedesktop.Notifications"
    [nameAllowReplacement, nameReplaceExisting]

  export client "/org/freedesktop/Notifications" defaultInterface
    { interfaceName = "org.freedesktop.Notifications"
    , interfaceMethods = 
      [ autoMethod "GetServerInformation" getServerInformation
      , autoMethod "GetCapabilites" getCapabilites
      , autoMethod "CloseNotification" onCloseNote
      , autoMethod "Notify" (onNote $ emit client)
      ]
    }

