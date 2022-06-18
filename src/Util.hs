module Util where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Text.Read ( readMaybe )
import Data.Maybe ( fromMaybe )
import qualified Codec.Picture.Repa as CR
import qualified Data.Array.Repa as R
import Data.Array.Repa ((:.), (:.)(..), Z(..))
import qualified Data.Array.Repa.Unsafe as RU
import Debug.Trace (trace)
import GHC.Word (Word8)
import System.Environment (lookupEnv)

debug :: Show a => a -> String -> a
debug o name = trace (name ++ " = " ++ show o) o

strToBStr :: String -> B.ByteString
strToBStr = encodeUtf8 . T.pack

envVarString :: String -> String -> IO String 
envVarString name def = do
    value <- lookupEnv name
    return $ fromMaybe def value


envVarInt :: String -> Int -> IO Int
envVarInt name def = do
    value <- lookupEnv name
    return $ fromMaybe def (value >>= readMaybe)

-- | A histogram is a one dimensional array where each element
-- indicates how many pixels held the value represented by the element's
-- index.
type Histogram = R.Array R.D R.DIM1 Word8

-- | Compute the RGB histograms of the image based on JuicyPixels-repa
-- implementation.
histograms :: CR.Img a -> (Histogram, Histogram, Histogram)
histograms (CR.Img arr) =
    let (Z :. nrRow :. nrCol :. _) = R.extent arr
        zero = R.fromFunction (Z :. 256) (\_ -> 0 :: Word8)

        incElem :: Word8 -> R.Array R.D R.DIM1 Word8 -> R.Array R.D R.DIM1 Word8
        incElem idx x = RU.unsafeTraverse x id (\l i -> l i + if i == (Z :. fromIntegral idx) then 1 else 0)

    in Prelude.foldl (\(hR, hG, hB) (row,col) ->
             let r = R.unsafeIndex arr (Z :. row :. col :. 0)
                 g = R.unsafeIndex arr (Z :. row :. col :. 1)
                 b = R.unsafeIndex arr (Z :. row :. col :. 2)
             in (incElem r hR, incElem g hG, incElem b hB))
          (zero, zero, zero)
          [(row, col) | row <- [0..nrRow - 1], col <- [0..nrCol - 1]]

-- having image rgb representation convert to 
-- to counts of pixels type on the image
imgHist :: (R.Array R.D R.DIM3 Word8) -> (R.Array R.U R.DIM3 Word8)
imgHist x = debug (R.computeS x) "test"