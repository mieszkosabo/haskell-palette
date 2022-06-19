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
import GHC.Word (Word8)
import Control.Monad.State
import qualified Data.Map as Map

paletteSize = 6
histogramGridSize = 3

histogram :: M.RawImage -> M.Histogram
histogram arr = hist
  where
    (Z :. nrRow :. nrCol :. _) = R.extent arr

    zero = R.fromFunction (Z :. 256 :. 256 :. 256) (\_ -> 0 :: Word8)

    imgMatrix = [(row, col) | row <- [0..nrRow - 1], col <- [0..nrCol - 1]]

    incElem :: Int -> Int -> Int -> M.Histogram -> M.Histogram
    incElem r g b h = RU.unsafeTraverse h id (\l i -> l i + if i == (Z :. r :. g :. b) then 1 else 0)

    levelAt :: (Int, Int) -> Int -> Int
    levelAt (row, col) rgb = fromIntegral $ R.unsafeIndex arr (Z :. row :. col :. rgb)

    hist = Prelude.foldl (\hist pos ->
                         let r = levelAt pos 0
                             g = levelAt pos 1
                             b = levelAt pos 2
                         in incElem r g b hist)
                         zero
                         imgMatrix

histogram' :: M.RawImage -> M.Palette
histogram' pixels = topColors
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
  return $ CR.imgData rgb