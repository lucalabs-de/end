{-# LANGUAGE OverloadedStrings #-}

module Util.DbusNotify where

import DBus
import Data.Map
import Data.Text
import Data.Int (Int32)
import Data.Maybe (fromMaybe)

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

