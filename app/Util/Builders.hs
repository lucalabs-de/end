module Util.Builders where

import DBus.Internal.Types
import qualified Data.List as List
import Data.Map (Map)
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
  Maybe String ->
  Word32 ->
  Text ->
  Text ->
  Text ->
  Text ->
  String ->
  String ->
  String
buildEwwNotification Nothing _ _ _ summary _ _ _ =
  "(label :text \""
    ++ unpack summary
    ++ "\" :xalign 1 :halign \"end\" :css \"label { padding-right: 12px; padding-top: 6px }\")"
buildEwwNotification (Just widgetName) nId appName appIcon summary body hints actions =
  "("
    ++ widgetName
    ++ " :end-id \""
    ++ show nId
    ++ "\" :end-appname \""
    ++ unpack appName
    ++ "\" :end-appicon \""
    ++ unpack appIcon
    ++ "\" :end-summary \""
    ++ unpack summary
    ++ "\" :end-body \""
    ++ unpack body
    ++ "\" :end-hints \""
    ++ hints
    ++ "\" :end-actions \""
    ++ actions
    ++ "\")"
