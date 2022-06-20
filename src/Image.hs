{-# LANGUAGE FlexibleContexts #-}

module Image where

import qualified Codec.Picture.Repa as CR
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Unsafe as RU
import qualified Model as M
import qualified Data.ByteString.Base64 as B64
import qualified Util as U
import qualified Data.List as L
import qualified Color as C

import Data.Array.Repa ((:.), (:.)(..), Z(..))
import Data.Array.Repa.Index (ix3)
import GHC.Word (Word8)
import Control.Monad.State
import qualified Data.Map as Map
import Data.Function (on)

targetImageSize = 256
paletteSize = 6
histogramGridSize = 3

histogram :: M.RawImage -> M.Palette
histogram pixels = topColors
  where
    buckets = createBuckets pixels histogramGridSize
    avgColorsAndCounts = map (\bucket -> (C.avarageColor bucket, length bucket)) buckets
    sorted = L.sortBy (\(_, count1) (_, count2) -> compare count2 count1) avgColorsAndCounts
    topColors = take paletteSize $ map (C.rgbToHex . fst) sorted

createBuckets :: M.RawImage -> Int -> [[C.RGB]]
createBuckets pixels gridSize = Map.elems $ execState createBuckets' emptyState
  where
    (Z :. nrRow :. nrCol :. _) = R.extent pixels
    imgMatrix = [(row, col) | row <- [0..nrRow - 1], col <- [0..nrCol - 1]]
    emptyState = Map.empty

    levelAt :: (Int, Int) -> Int -> Int
    levelAt (row, col) rgb = fromIntegral $ R.unsafeIndex pixels (Z :. row :. col :. rgb)

    createBuckets' :: State (Map.Map Int [C.RGB]) ()
    createBuckets' = do
      forM_ imgMatrix (\pos -> do
        let rgb@[r, g, b] = map (levelAt pos) [0..2]
        let [rIdx, gIdx, bIdx] = map (valueToBucketIdx . fromIntegral) rgb
        let idx = turn3DIndexTo1DIndex rIdx gIdx bIdx
        modify (\s -> let appendedArr = (r, g, b) : Map.findWithDefault [] idx s in Map.insert idx appendedArr s)
        )
    valueToBucketIdx val = floor (val / (255 / fromIntegral gridSize))
    turn3DIndexTo1DIndex x y z = x + (gridSize * y) + (gridSize * gridSize * z)

decodeImage :: String -> Either String M.RawImage
decodeImage imgBase64 = do
  let bimg = U.strToBStr imgBase64
  dimg <- B64.decode bimg
  rgb <- CR.decodeImageRGB dimg
  let raw = CR.imgData rgb 
  resized <- resizeNNSafeToScale targetImageSize raw
  return resized

resizeNNSafeToScale :: Int -> M.RawImage -> Either String M.RawImage
resizeNNSafeToScale nd img = do
  let (Z :. h :. w :. _) = R.extent img
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
resizeNN nh nw img = R.traverse img (\_ -> ix3 nh nw z) idxf
  where
    (Z :. h :. w :. z) = R.extent img

    y_ratio = h `divf` nh
    x_ratio = w `divf` nw

    idxf :: (R.DIM3 -> Word8) -> R.DIM3 -> Word8
    idxf f (Z :. y :. x :. z) = f (ix3 py px z)
      where
        py = floor ((fromIntegral y) * y_ratio) :: Int
        px = floor ((fromIntegral x) * x_ratio) :: Int

divf :: (Fractional c, Integral a) => a -> a -> c
divf = (/) `on` fromIntegral
