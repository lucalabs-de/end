module Util.Builders where

import DBus.Internal.Types
import qualified Data.List as List
import Data.Map (Map)
import Data.Aeson (Array, Object)
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import Data.Word (Word32)

import State
import Util.Helpers (groupTuples)

setEwwValue :: String -> String -> String
setEwwValue var val = "eww update " ++ var ++ "='" ++ val ++ "'"

buildWindowOpenCommand :: String -> String
buildWindowOpenCommand w = "eww open " ++ w

buildWindowCloseCommand :: String -> String
buildWindowCloseCommand w = "eww close " ++ w

buildWidgetWrapper :: Bool -> String -> String
buildWidgetWrapper True widgets = "(box :space-evenly false :orientation \"vertical\" " ++ widgets ++ ")"
buildWidgetWrapper False widgets = "(box :space-evenly false :orientation \"horizontal\" " ++ widgets ++ ")"

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
            (actionString n)
    )
    ""

buildHintString :: Map Text Variant -> String
buildHintString = buildJsonString

buildJsonString :: Map Text Variant -> String
buildJsonString pairs =
  "["
    ++ List.intercalate ", " (Map.foldrWithKey (\k v s -> s ++ showEntry k v) [] pairs)
    ++ "]"
 where
  showEntry k v = ["{ key: \\\"" ++ unpack k ++ "\\\", value: \\\"" ++ show v ++ "\\\" }"]
  show (Variant (ValueAtom (AtomText x))) = unpack x
  show (Variant x) = showValue True x

buildActionString :: [Text] -> String
buildActionString list = buildJsonString actions
 where
  actions = Map.map toVariant (Map.fromList (groupTuples list))

buildEwwNotification ::
  Maybe String -> -- notification widget
  Object -> -- notification data
  String
buildEwwNotification (Just widgetName) notificationData =
  printf
    "(%s :notification %s)"
    widgetName
    (LT.unpack $ encodeToLazyText notificationData)
buildEwwNotification Nothing notificationData =
  printf 
    "(label :text \"%s\" :xalign 1 :halign \"end\" :css \"label { padding-right: 12px; padding-top: 6px }\")"
    summary
 where
  summary = case Object.lookup "summary" notificationData of
    Just (String t) -> unpack t
    _noSummary -> ""
