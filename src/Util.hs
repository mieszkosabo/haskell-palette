{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Util where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

import Data.Text.Encoding (encodeUtf8)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import System.Environment (lookupEnv)
import System.Random (randomR, RandomGen, Random)
import Control.Parallel.Strategies (using, rseq, parList)


pattern Empty :: VU.Unbox a => VU.Vector a
pattern Empty <- (VU.null -> True) where Empty = VU.empty 

uncons :: VU.Unbox a => VU.Vector a -> Maybe (a, VU.Vector a)
uncons Empty = Nothing
uncons v = Just (VU.unsafeHead v, VU.unsafeTail v)

pattern (:<|) :: VU.Unbox a => a -> VU.Vector a -> VU.Vector a
pattern x :<| xs <- (uncons -> Just (x, xs))

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

splitAtEvery :: (Eq a, VU.Unbox a) => Int -> VU.Vector a -> [VU.Vector a]
splitAtEvery ith ys
    | zs' == VU.empty = [zs]
    | otherwise = zs : splitAtEvery ith zs'
    where (zs, zs') = VU.splitAt ith ys

frequency :: (Floating w, Ord w, Random w, VU.Unbox w, VU.Unbox a, RandomGen g) => VU.Vector (w, a) -> g -> (a, g)
frequency xs g0 = (pick r xs, g1)
    where (r, g1) = randomR (0, tot) g0
          tot = VU.sum (VU.map fst xs)
          pick n ((w, a) :<| xs) 
            | n <= w = a
            | otherwise = pick (n - w) xs
          pick _ Empty = error "invalid frequency state"

randomElem :: (RandomGen g, VU.Unbox a)  => VU.Vector a -> g -> (a, g)
randomElem xs g = (xs VU.! pos, g') where
  n = VU.length xs
  (pos, g') = randomR (0, (n - 1)) g

parMap :: (a -> b) -> [a] -> [b]
parMap f xs = map f xs `using` parList rseq

{-# INLINE groupByVU #-}
groupByVU :: (VU.Unbox a) => (a -> a -> Bool) -> VU.Vector a -> [VU.Vector a]
groupByVU _ v | VU.null v = []
groupByVU f v =
  let h = VU.unsafeHead v
      tl = VU.unsafeTail v
  in case VU.findIndex (not . f h) tl of
      Nothing -> [v]
      Just n -> VU.unsafeTake (n + 1) v : groupByVU f (VU.unsafeDrop (n + 1) v)

