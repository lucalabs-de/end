module Util.ImageConversion where

import DBus (Array)
import Data.Int (Int32)
import Data.Word (Word8)
import Util.DbusNotify (ImageData)

writeImageDataToPng :: ImageData -> IO FilePath
writeImageDataToPng img = do return ""
