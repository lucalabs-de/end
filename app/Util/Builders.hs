module Util.Builders where

import DBus.Internal.Types
import Data.Map hiding (foldr)
import Data.Text hiding (foldr)
import State

setEwwValue :: String -> String -> String
setEwwValue var val = "eww update " ++ var ++ "='" ++ val ++ "'"

buildWidgetString :: [Notification] -> String
buildWidgetString =
  foldr
    ( \n s ->
        s
          ++ buildEwwNotification
            (notifyType n)
            (appName n)
            (appIcon n)
            (summary n)
            (body n)
            (hintString n)
    )
    ""

buildHintString :: Map Text Variant -> String
buildHintString = Data.Map.foldrWithKey (\k v s -> s ++ showEntry k v) ""
 where
  showEntry k v = "(" ++ unpack k ++ "," ++ show v ++ ")"
  show (Variant x) = showValue True x

defaultEwwWidget :: String
defaultEwwWidget = "(defwidget end-notification [end-appname end-appicon end-summary end-body end-hints] ${end-summary}"

buildEwwNotification :: -- TODO when the notifyType is Nothing, use the default widget, otherwise read widget name from config 
  Maybe String ->
  Text ->
  Text ->
  Text ->
  Text ->
  String ->
  String
buildEwwNotification notificationType appName appIcon summary body hints =
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
