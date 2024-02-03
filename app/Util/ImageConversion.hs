module Util.ImageConversion where

import Codec.Picture (PixelRGBA8 (PixelRGBA8), generateImage, writePng)
import DBus (arrayItems, fromVariant)
import Data.Array (listArray, (!))
import qualified Data.Array as DA
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Util.DbusNotify (ImageData)

type Array v = DA.Array Int v -- int-indexed array

writeImageDataToPng :: ImageData -> IO FilePath
writeImageDataToPng img = do
  putStrLn "write data to png"
  let path = "test.png"

  let (widthData, heightData, strideData, hasAlpha, _, _, pixelData) = img
  let width = fromIntegral widthData
  let height = fromIntegral heightData
  let stride = fromIntegral strideData

  let pixels = fromMaybe 0 . fromVariant <$> arrayItems pixelData :: [Word8]
  
  -- transform list to array to ensure constant indexed access
  let pixelArray = listArray (0, length pixels - 1) pixels

  writePng path $ generateImage (pixelTransformer stride hasAlpha pixelArray) width height
  putStrLn "done"
  return path

pixelTransformer :: Int -> Bool -> Array Word8 -> Int -> Int -> PixelRGBA8
pixelTransformer stride hasAlpha pixels x y
  | length pixels > pixelIndex =
      if hasAlpha
        then PixelRGBA8 r g b a
        else PixelRGBA8 r g b 255
  | otherwise = PixelRGBA8 0 0 0 0
 where
  pixelSize = if hasAlpha then 4 else 3
  pixelIndex = y * stride + x * pixelSize
  r = pixels ! pixelIndex
  g = pixels ! (pixelIndex + 1)
  b = pixels ! (pixelIndex + 2)
  a = pixels ! (pixelIndex + 3)
