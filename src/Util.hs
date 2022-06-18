module Util where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Array.Repa as R
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
    case value of
        Just v -> return v
        Nothing -> return def

-- having image rgb representation convert to 
-- to counts of pixels type on the image
imgHist :: (R.Array R.D R.DIM3 Word8) -> (R.Array R.U R.DIM3 Word8)
imgHist x = debug (R.computeS x) "test"