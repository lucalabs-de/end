module Util.Builders where

import DBus.Internal.Types
import Data.Map hiding (foldr)
import Data.Text hiding (foldr)
import Data.Word (Word32)
import State

setEwwValue :: String -> String -> String
setEwwValue var val = "eww update " ++ var ++ "='" ++ val ++ "'"

buildWindowCommand :: String -> String
buildWindowCommand w = "eww open " ++ w

buildWidgetWrapper :: Bool -> String -> String
buildWidgetWrapper True widgets = "(box :orientation \"vertical\" " ++ widgets ++ ")"
buildWidgetWrapper False widgets = "(box :orientation \"horizontal\" " ++ widgets ++ ")"

buildWidgetString :: [Notification] -> String
buildWidgetString =
  foldr
    ( \n s ->
        s
          ++ buildEwwNotification
            (widget n)
            (nId n)
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

buildEwwNotification ::
  Maybe String ->
  Word32 ->
  Text ->
  Text ->
  Text ->
  Text ->
  String ->
  String
buildEwwNotification Nothing _ _ _ summary _ _ = "(label :text \"" ++ unpack summary ++ "\" :xalign 1 :halign \"end\")"
buildEwwNotification (Just widgetName) nId appName appIcon summary body hints =
  "("
    ++ widgetName
    ++ ":end-id \""
    ++ show nId
    ++ "\""
    ++ ":end-appname \""
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
