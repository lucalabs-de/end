{-# LANGUAGE OverloadedStrings #-}

module Util.DbusNotify where

import Config (TimeoutByUrgency (critical, low, normal))
import DBus
import Data.Int (Int32)
import Data.Map
import Data.Maybe (fromMaybe)
import Data.Text
import Data.Word (Word32)

type Hints = Map Text Variant

hintKeyNotifyType, hintKeyUrgency :: Text
hintKeyNotifyType = "end-type"
hintKeyUrgency = "urgency"

getStringHint :: Hints -> Text -> Maybe String
getStringHint map key = Data.Map.lookup key map >>= fromVariant

getIntHint :: Hints -> Text -> Maybe Int32
getIntHint map key = Data.Map.lookup key map >>= fromVariant

getUrgency :: Hints -> Int32
getUrgency hints = fromMaybe 1 $ getIntHint hints hintKeyUrgency

configKeyFromUrgency :: Int32 -> (TimeoutByUrgency -> Word32)
configKeyFromUrgency 0 = low
configKeyFromUrgency 2 = critical
configKeyFromUrgency _ = normal
