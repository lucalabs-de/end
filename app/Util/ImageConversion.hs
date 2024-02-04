module Util.ImageConversion (writeImageDataToPng, wipeImageDirectory) where

import Codec.Picture (Image, PixelRGBA8 (PixelRGBA8), generateImage, writePng)
import Control.Monad (when)
import DBus (arrayItems, fromVariant)
import Data.Array (listArray, (!))
import qualified Data.Array as DA
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath ((<.>))
import System.IO.Temp (emptyTempFile)

import Util.Constants (imageTempDir)
import Util.DbusNotify (ImageData)

type Array v = DA.Array Int v -- int-indexed array

wipeImageDirectory :: IO ()
wipeImageDirectory = do
  exists <- doesDirectoryExist imageTempDir
  when exists (removeDirectoryRecursive imageTempDir)

writeImageDataToPng :: String -> ImageData -> IO FilePath
writeImageDataToPng name img = do
  let png = convertImageDataToPng img

  createDirectoryIfMissing False imageTempDir
  path <- emptyTempFile imageTempDir (name <.> "png")

  writePng path png
  return path

convertImageDataToPng :: ImageData -> Image PixelRGBA8
convertImageDataToPng img = do
  let (widthData, heightData, strideData, hasAlpha, _, _, pixelData) = img
  let width = fromIntegral widthData
  let height = fromIntegral heightData
  let stride = fromIntegral strideData

  let pixels = fromMaybe 0 . fromVariant <$> arrayItems pixelData :: [Word8]

  -- transform list to array to ensure constant indexed access
  let pixelArray = listArray (0, length pixels - 1) pixels

  generateImage (pixelTransformer stride hasAlpha pixelArray) width height

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
