{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Model where

import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as VU

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import GHC.Word (Word8)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Vector.Unboxed.Deriving (derivingUnbox)

data ImageRequest = ImageRequest { 
  algorithm :: String, 
  image :: String,
  count :: Int
} deriving (Show, Generic)

newtype ColorsResponse = ColorsResponse { 
  colors :: [Color]
} deriving (Show, Generic)

newtype ErrorResponse = ErrorResponse { 
  msg :: String
} deriving (Show, Generic)

instance ToJSON ColorsResponse

instance ToJSON ErrorResponse

instance FromJSON ImageRequest

type Color = String

type Palette = [Color]

type FileName = String

type ErrorMessage = String

type RGB = (Int, Int, Int)

type Algorithm = Int -> VU.Vector RGB -> [RGB]

type ChannelRange = (Int, Int)

type RGBVect = (Double, Double, Double)

type Points = VU.Vector PointHolder

type KMeansInit = Int -> Points -> [Points]

data PointHolder = PointHolder { 
    normed :: RGBVect, 
    color :: RGB
}

derivingUnbox "PointHolder"
    [t| PointHolder -> ((Double, Double, Double), (Int, Int, Int)) |]
    [| \PointHolder {..} -> (normed, color) |]
    [| \(n, c) -> PointHolder { normed = n, color = c } |]

instance Eq PointHolder where
   (==) = (==) `on` normed

instance Ord PointHolder where
    compare = comparing normed

-- 256x256x256 where matrix value represents count
-- of pixels of given RGB color
type Histogram = R.Array R.D R.DIM3 Word8

-- HEIGHTxWIDTHx3 where matrix values are 
-- level values from 0..255
type RawImage = R.Array R.D R.DIM2 RGB

type ComputedImage = VU.Vector RGB

-- (HEIGHT*WIDTH)x3 where matrix values are 
-- level values from 0..255
type PixelSpace = R.Array R.D R.DIM2 Word8