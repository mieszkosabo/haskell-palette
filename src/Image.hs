{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Image where

import qualified Codec.Picture.Repa as CR
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Unsafe as RU
import qualified Model as M
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import qualified Util as U
import qualified Data.List as L
import qualified Color as C
import qualified Data.Vector.Unboxed as V
import qualified Data.Map as Map
import qualified Data.Ord as O
import qualified System.Random as Rnd

import Data.Array.Repa ((:.), (:.)(..), Z(..))
import Data.Array.Repa.Index (ix2)
import Control.Monad.State
import Data.Function (on)
import GHC.Word (Word8)

targetImageSize = 256

histogramGridSize = 3

seed = 42

imagePoints :: M.ComputedImage -> [M.RGB]
imagePoints = V.toList . R.toUnboxed

squaredEuclideanDist :: M.Vect -> M.Vect -> Double
squaredEuclideanDist xs ys = sum $ zipWith (\x y -> (x - y) ^ 2) xs ys

euclideanDist :: M.Vect -> M.Vect -> Double
euclideanDist xs ys = sqrt $ squaredEuclideanDist xs ys

centroid :: [M.PointHolder] -> [Double]
centroid points = U.parMap (flip (/) l . sum) $ L.transpose (map M.normed points)
  where 
    l = fromIntegral $ L.length points

closest :: [M.Vect] -> M.Vect -> M.Vect
closest points point = L.minimumBy (O.comparing $ euclideanDist point) points

assignToCentroids :: [M.Vect] -> [M.PointHolder] -> [[(M.Vect, M.PointHolder)]]
assignToCentroids centroids points = L.groupBy ((==) `on` fst) 
                                   $ L.sort [(closest centroids (M.normed p), p) | p <- points]

reclusterForCentroids :: [M.Vect] -> [M.PointHolder] -> [[M.PointHolder]]
reclusterForCentroids centroids points = U.parMap (map snd) 
                                       $ assignToCentroids centroids points

squaredDistancesToCentroids :: [M.Vect] -> [M.PointHolder] -> [(Double, M.PointHolder)]
squaredDistancesToCentroids centroids points = L.concatMap (map dist)
                                             $ assignToCentroids centroids points
  where 
    dist :: (M.Vect, M.PointHolder) -> (Double, M.PointHolder)
    dist (v, p) = (squaredEuclideanDist v (M.normed p), p)

recluster :: [[M.PointHolder]] -> [[M.PointHolder]]
recluster clusters = reclusterForCentroids centroids $ L.concat clusters
  where 
    centroids = U.parMap centroid clusters

kmeansClustered :: [[M.PointHolder]] -> [[M.PointHolder]]
kmeansClustered clusters
    | clusters == clusters' = clusters
    | otherwise = kmeansClustered clusters'
    where 
      clusters' = recluster clusters

selectCentersByDistr :: M.KMeansInit
selectCentersByDistr k points = go [M.normed rndPoint] k g0
  where
    (rndPoint, g0) = U.randomElem points $ Rnd.mkStdGen seed

    go :: Rnd.RandomGen g => [M.Vect] -> Int -> g -> [[M.PointHolder]]
    go centroids 1 _ = reclusterForCentroids centroids points
    go centroids k g = go (M.normed centroid : centroids) (k - 1) g'
      where 
        distribution = squaredDistancesToCentroids centroids points
        (centroid, g') = U.frequency distribution g

selectCentersNoRandom :: M.KMeansInit
selectCentersNoRandom k points = U.splitAtEvery lth points
  where 
    lth = (length points + k - 1) `div` k

kmeans :: M.KMeansInit -> M.Algorithm
kmeans kinit paletteSize = extractColors . cluster . (map wrapToHolder)
  where
    norm :: M.RGB -> [Double]
    norm (x, y, z) = [x `divf` 255, y `divf` 255, z `divf` 255]

    wrapToHolder :: M.RGB -> M.PointHolder
    wrapToHolder c = M.PointHolder { M.normed = norm c, M.color = c }

    cluster :: [M.PointHolder] -> [[M.PointHolder]]
    cluster points = kmeansClustered $ kinit paletteSize points

    extractColors :: [[M.PointHolder]] -> [M.RGB]
    extractColors = map (C.avarageColor . map M.color)

kmeansStd :: M.Algorithm
kmeansStd = kmeans selectCentersNoRandom

kmeansPP :: M.Algorithm
kmeansPP = kmeans selectCentersByDistr

medianCut :: M.Algorithm
medianCut paletteSize points = map (C.avarageColor . snd) $ go (paletteSize - 1) [(evalRanges points, points)] 
  where
    go :: Int -> [([M.ChannelRange], [M.RGB])] -> [([M.ChannelRange], [M.RGB])]
    go 0 s = s
    go n s = go (n - 1) (oldBuckets ++ newBuckets)
      where
        (idx, (ranges, elems)) = L.maximumBy (compare `on` maxAbsRange . fst . snd) (zip [0..] s)
        (rIdx, range) = L.maximumBy (compare `on` absRange . snd) (zip [0..] ranges)
        median = mid . map (U.tupleAtPos3 rIdx) $ elems
        (elems1, elems2) = L.partition (\pixel -> U.tupleAtPos3 rIdx pixel <= median) elems
        oldBuckets = deleteAt idx s
        newBuckets = map (\els -> (evalRanges els, els)) [elems1, elems2]

    maxAbsRange :: [M.ChannelRange] -> Int
    maxAbsRange ranges = L.maximumBy (compare) (map absRange ranges) 

    absRange :: M.ChannelRange -> Int
    absRange (l, u) = u - l

    evalRanges :: [M.RGB] -> [M.ChannelRange]
    evalRanges = foldl update [(255, 0), (255, 0), (255, 0)]
      where
        update ranges@[r_r, g_r, b_r] rgb@(r, g, b) = [update' r_r r, update' g_r g, update' b_r b]
        update' range@(l, u) v = (min l v, max u v)

    mid :: Integral a => [a] -> a
    mid xs = case mid' xs of
      [x] -> x
      [x,y] -> (x `div` 2) + (y `div` 2)
      where 
        mid' [] = []
        mid' t = mid'' t t
      
        mid'' (x:_) [_] = [x]
        mid'' (x:y:_) [_,_] = [x,y]
        mid'' (_:t) (_:_:u) = mid'' t u  

    deleteAt :: Int -> [a] -> [a]
    deleteAt idx xs = lft ++ rgt
      where (lft, (_:rgt)) = splitAt idx xs

histogram :: M.Algorithm
histogram paletteSize points = topColors
  where
    buckets = createBuckets points histogramGridSize
    avgColorsAndCounts = map (\bucket -> (C.avarageColor bucket, length bucket)) buckets
    sorted = L.sortBy (\(_, count1) (_, count2) -> compare count2 count1) avgColorsAndCounts
    topColors = take paletteSize $ map fst sorted

createBuckets :: [M.RGB] -> Int -> [[M.RGB]]
createBuckets points gridSize = Map.elems $ execState createBuckets' Map.empty
  where
    createBuckets' :: State (Map.Map Int [M.RGB]) ()
    createBuckets' = do
      forM_ points (\rgb -> do
        let rgbIdx = U.mapTuple3 valueToBucketIdx rgb
        let idx = turn3DIndexTo1DIndex rgbIdx
        let addToBuckets s = let appendedArr = rgb : Map.findWithDefault [] idx s in Map.insert idx appendedArr s
        modify addToBuckets
        )
    valueToBucketIdx val = floor (fromIntegral val / (255 / fromIntegral gridSize))
    turn3DIndexTo1DIndex (x, y, z) = x + (gridSize * y) + (gridSize * gridSize * z)

decodeImage :: String -> Either M.ErrorMessage (CR.Img CR.RGBA)
decodeImage imgBase64 = do
  let bimg = U.strToBStr imgBase64
  B64.decode bimg >>= CR.decodeImageRGBA

preprocessImage :: CR.Img CR.RGBA -> Either M.ErrorMessage M.ComputedImage
preprocessImage rgba = do
  let collapsed = CR.collapseColorChannel rgba
  let collapsedImg = R.map (U.mapTuple3 (fromIntegral . toInteger :: Word8 -> Int)) collapsed
  resizedImg <- resize collapsedImg
  R.computeP resizedImg

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
