{-# LANGUAGE OverloadedStrings #-}

module Util.DbusNotify where

import Config (TimeoutByUrgency (critical, low, normal))
import DBus
import Data.Int (Int32)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text
import Data.Word (Word32, Word8)

type Hints = Map.Map Text Variant
type ImageData = (Int32, Int32, Int32, Bool, Int32, Int32, Array)

hintKeyNotifyType, hintKeyUrgency :: Text
hintKeyNotifyType = "end-type"
hintKeyUrgency = "urgency"

getStringHint :: Hints -> Text -> Maybe String
getStringHint map key = Map.lookup key map >>= fromVariant

getByteHint :: Hints -> Text -> Maybe Word8
getByteHint map key = Map.lookup key map >>= fromVariant

getImageDataHint :: Hints -> Text -> Maybe ImageData
getImageDataHint map key = Map.lookup key map >>= fromVariant

getUrgency :: Hints -> Word8
getUrgency hints = fromMaybe 1 $ getByteHint hints hintKeyUrgency

configKeyFromUrgency :: Word8 -> (TimeoutByUrgency -> Word32)
configKeyFromUrgency 0 = low
configKeyFromUrgency 2 = critical
configKeyFromUrgency _ = normal
