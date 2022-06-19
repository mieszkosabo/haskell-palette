module Util where

import qualified Data.ByteString as B
import qualified Data.Text as T

import Data.Text.Encoding (encodeUtf8)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
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

listToTuple3 :: [a] -> (a, a, a)
listToTuple3 (a:b:c:rest) = (a, b, c)
listToTuple3 _ = error "Argument list must have at least 3 elements"

tupleToList3 :: (a, a, a) -> [a]
tupleToList3 (a, b, c) = [a, b, c]