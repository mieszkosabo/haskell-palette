module Util where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
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

type Histogram = R.Array R.D R.DIM3 Word8

histograms :: CR.Img a -> Histogram
histograms (CR.Img arr) = hist
  where
    (Z :. nrRow :. nrCol :. _) = R.extent arr

    zero = R.fromFunction (Z :. 256 :. 256 :. 256) (\_ -> 0 :: Word8)

    imgMatrix = [(row, col) | row <- [0..nrRow - 1], col <- [0..nrCol - 1]]

    incElem :: Int -> Int -> Int -> Histogram -> Histogram
    incElem r g b x = RU.unsafeTraverse x id (\l i -> l i + if i == (Z :. r :. g :. b) then 1 else 0)

    valueAt :: (Int, Int) -> Int -> Int
    valueAt (row, col) rgb = fromIntegral $ R.unsafeIndex arr (Z :. row :. col :. rgb)

    hist = Prelude.foldl (\hist pos ->
                         let r = valueAt pos 0
                             g = valueAt pos 1
                             b = valueAt pos 2
                         in (incElem r g b hist))
                         zero imgMatrix
           

-- having image rgb representation convert to 
-- to counts of pixels type on the image
imgHist :: (R.Array R.D R.DIM3 Word8) -> (R.Array R.U R.DIM3 Word8)
imgHist x = debug (R.computeS x) "test"