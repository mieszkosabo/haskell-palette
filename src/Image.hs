{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Image where

import qualified Codec.Picture.Repa as CR
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Unsafe as RU
import qualified Model as M
import qualified Data.ByteString.Base64 as B64
import qualified Util as U
import qualified Data.List as L
import qualified Color as C
import qualified Data.Vector.Unboxed as V
import qualified Data.Map as Map

import Data.Array.Repa ((:.), (:.)(..), Z(..))
import Data.Array.Repa.Index (ix2)
import Control.Monad.State
import qualified Data.Map as Map
import Data.Function (on)
import GHC.Word (Word8)

targetImageSize = 256
paletteSize = 6
histogramGridSize = 3

histogram :: M.ComputedImage -> M.Palette
histogram pixels = topColors
  where
    buckets = createBuckets pixels histogramGridSize
    avgColorsAndCounts = map (\bucket -> (C.avarageColor bucket, length bucket)) buckets
    sorted = L.sortBy (\(_, count1) (_, count2) -> compare count2 count1) avgColorsAndCounts
    topColors = take paletteSize $ map (C.rgbToHex . fst) sorted

createBuckets :: M.ComputedImage -> Int -> [[M.RGB]]
createBuckets pixels gridSize = Map.elems $ execState createBuckets' emptyState
  where
    (Z :. nrRow :. nrCol) = R.extent pixels
    imgMatrix = [(row, col) | row <- [0..nrRow - 1], col <- [0..nrCol - 1]]
    emptyState = Map.empty

    levelAt :: (Int, Int) -> M.RGB
    levelAt (row, col) = R.unsafeIndex pixels (Z :. row :. col)

    createBuckets' :: State (Map.Map Int [M.RGB]) ()
    createBuckets' = do
      forM_ imgMatrix (\pos -> do
        let rgb@(r, g, b) = levelAt pos
        let (rIdx, gIdx, bIdx) = U.mapTuple3 (valueToBucketIdx . fromIntegral) rgb
        let idx = turn3DIndexTo1DIndex rIdx gIdx bIdx
        modify (\s -> let appendedArr = rgb : Map.findWithDefault [] idx s in Map.insert idx appendedArr s)
        )
    valueToBucketIdx val = floor (val / (255 / fromIntegral gridSize))
    turn3DIndexTo1DIndex x y z = x + (gridSize * y) + (gridSize * gridSize * z)

decodeImage :: String -> Either String M.RawImage
decodeImage imgBase64 = do
  let bimg = U.strToBStr imgBase64
  dimg <- B64.decode bimg
  rgba <- CR.decodeImageRGBA dimg
  let collapsed = CR.collapseColorChannel rgba
  return $ R.map (U.mapTuple3 (fromIntegral . toInteger :: Word8 -> Int)) collapsed 

resize :: M.RawImage -> Either String M.RawImage
resize = resizeNNSafeToScale targetImageSize

resizeNNSafeToScale :: Int -> M.RawImage -> Either String M.RawImage
resizeNNSafeToScale nd img = do
  let (Z :. h :. w) = R.extent img
  let ratio = nd `divf` (max h w) 
  if (ratio < 1.0) 
    then do
      let scale = \d -> floor (ratio * fromIntegral d)
      resizeNNSafe (scale h) (scale w) img
    else 
      return img

resizeNNSafe :: Int -> Int -> M.RawImage -> Either String M.RawImage
resizeNNSafe nh nw img = do
  when (nh <= 0 || nw <= 0) (Left "invalid size")
  return $ resizeNN nh nw img

resizeNN :: Int -> Int -> M.RawImage -> M.RawImage
resizeNN nh nw img = R.traverse img (\_ -> ix2 nh nw) idxf
  where
    (Z :. h :. w) = R.extent img

    y_ratio = h `divf` nh
    x_ratio = w `divf` nw

    idxf f (Z :. y :. x) = f (ix2 py px)
      where
        py = floor ((fromIntegral y) * y_ratio) :: Int
        px = floor ((fromIntegral x) * x_ratio) :: Int

divf :: (Fractional c, Integral a) => a -> a -> c
divf = (/) `on` fromIntegral
