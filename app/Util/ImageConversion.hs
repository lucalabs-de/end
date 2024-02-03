module Util.ImageConversion where

import Codec.Picture (PixelRGBA8 (PixelRGBA8), generateImage, writePng)
import DBus (arrayItems, fromVariant)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Util.DbusNotify (ImageData)

writeImageDataToPng :: ImageData -> IO FilePath
writeImageDataToPng img = do
  putStrLn "write data to png"
  let path = "test.png"

  let (widthData, heightData, strideData, hasAlpha, _, _, pixelData) = img
  let width = fromIntegral widthData
  let height = fromIntegral heightData
  let stride = fromIntegral strideData

  let pixels = fromMaybe 0 . fromVariant <$> arrayItems pixelData :: [Word8]

  writePng path $ generateImage (pixelTransformer stride hasAlpha pixels) width height
  putStrLn "done"
  return path

pixelTransformer :: Int -> Bool -> [Word8] -> Int -> Int -> PixelRGBA8
pixelTransformer stride hasAlpha pixels x y =
  if hasAlpha
    then PixelRGBA8 r g b a
    else PixelRGBA8 r g b 255
 where
  pixelSize = if hasAlpha then 4 else 3
  pixelIndex = y * stride + x * pixelSize
  r = pixels !! pixelIndex
  g = pixels !! (pixelIndex + 1)
  b = pixels !! (pixelIndex + 2)
  a = pixels !! (pixelIndex + 3)
