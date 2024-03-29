{-# LANGUAGE OverloadedStrings #-}

module Util.Builders where

import Data.Aeson (Array, Object, ToJSON (..))
import qualified Data.Aeson as Aeson
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as Object
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (Value (..), (.=))

import DBus (Variant)
import DBus.Internal.Types (
  Atom (..),
  Value (ValueAtom, ValueVariant),
  Variant (..),
 )
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import qualified Data.Text.Lazy as LT

import Data.Vector (fromList)
import State
import Text.Printf (printf)
import Util.DbusNotify (Hints)
import Util.Helpers (asAesonObject, groupTuples)

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
  foldr (\n s -> s ++ notificationString n) ""
 where
  notificationString n =
    buildEwwNotification
      (widget n)
      ( asAesonObject
          [ "id" .= nId n
          , "application" .= appName n
          , "icon" .= appIcon n
          , "summary" .= summary n
          , "body" .= body n
          , "hints" .= buildHintObject (hints n)
          , "actions" .= buildActionArray (actions n)
          ]
      )

buildHintObject :: Hints -> Object
buildHintObject =
  Map.foldrWithKey (\k v a -> Object.insert (fromText k) (fromVariant v) a) Object.empty

buildActionArray :: [Text] -> Array
buildActionArray =
  fromList . map (\(k, v) -> Aeson.object ["key" .= k, "name" .= v]) . groupTuples

buildEwwNotification ::
  Maybe String -> -- notification widget
  Object -> -- notification data
  String
buildEwwNotification Nothing notificationData =
  let
    summary = case Object.lookup "summary" notificationData of
      Just (String t) -> unpack t
      _noSummary -> ""
   in
    printf
      "(label :text \"%s\" :xalign 1 :halign \"end\" :css \"label { padding-right: 12px; padding-top: 6px }\")"
      summary
buildEwwNotification (Just widgetName) notificationData =
  printf
    "(%s :notification %s)"
    widgetName
    (show $ LT.unpack $ encodeToLazyText notificationData)

-- REMARK: Doesn't support all variant types yet, but should be sufficient for
-- hints and actions
fromVariant :: Variant -> Aeson.Value
fromVariant (Variant (ValueVariant v)) = fromVariant v
fromVariant (Variant (ValueAtom a)) = atomToJson a
 where
  atomToJson a = case a of
    AtomBool b -> Bool b
    AtomText t -> String t
    AtomInt16 i -> toJSON i
    AtomInt32 i -> toJSON i
    AtomInt64 i -> toJSON i
    AtomWord8 i -> toJSON i
    AtomWord32 i -> toJSON i
    AtomWord64 i -> toJSON i
    AtomDouble d -> toJSON d
    _notSupported -> Null
fromVariant _notSupported = Null
