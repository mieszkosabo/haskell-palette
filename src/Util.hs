module Util where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Codec.Picture.Repa as CR
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Unsafe as RU
import qualified Model as M
import qualified Data.ByteString.Base64 as B64

import Data.Array.Repa ((:.), (:.)(..), Z(..))
import Data.Text.Encoding (encodeUtf8)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
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
                         in (incElem r g b hist))
                         zero
                         imgMatrix
           

decodeImage :: String -> Either String M.RawImage
decodeImage imgBase64 = do
  let bimg = strToBStr imgBase64
  dimg <- B64.decode bimg
  rgb <- CR.decodeImageRGB dimg
  return $ CR.imgData rgb