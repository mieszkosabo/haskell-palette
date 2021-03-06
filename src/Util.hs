module Util where

import qualified Data.ByteString as B
import qualified Data.Text as T

import Data.Text.Encoding (encodeUtf8)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import System.Environment (lookupEnv)
import System.Random (randomR, RandomGen, Random)
import Control.Parallel.Strategies (using, rseq, parList)


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

mapTuple3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTuple3 f (a, b, c) = (f a, f b, f c)

tupleAtPos3 :: Int -> (a, a, a) -> a
tupleAtPos3 pos (a, b, c) = case pos of
    0 -> a
    1 -> b
    2 -> c
    _ -> error "Out of bounds"

splitAtEvery :: (Eq a) => Int -> [a] -> [[a]]
splitAtEvery ith ys
    | zs' == [] = [zs]
    | otherwise = zs : splitAtEvery ith zs'
    where (zs, zs') = splitAt ith ys

frequency :: (Floating w, Ord w, Random w, RandomGen g) => [(w, a)] -> g -> (a, g)
frequency xs g0 = (pick r xs, g1)
    where (r, g1) = randomR (0, tot) g0
          tot = sum (map fst xs)
          pick n ((w, a) : xs) 
             | n <= w = a
             | otherwise = pick (n - w) xs
          pick _ [] = error "invalid frequency state"

randomElem :: RandomGen g => [a] -> g -> (a, g)
randomElem xs g = (xs !! pos, g') where
  n = length xs
  (pos, g') = randomR (0, (n - 1)) g

parMap :: (a -> b) -> [a] -> [b]
parMap f xs = map f xs `using` parList rseq
