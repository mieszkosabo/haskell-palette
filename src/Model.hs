{-# LANGUAGE TemplateHaskell #-}

module Model where

import qualified Data.Array.Repa as R
import Data.Aeson.TH (deriveJSON, defaultOptions)
import GHC.Generics
import GHC.Word (Word8)
import Data.Function (on)
import Data.Ord (comparing)

type Color = String

data ImageRequest = ImageRequest { 
  algorithm :: String, 
  image :: String,
  count :: Int
} deriving (Show)

newtype ColorsResponse = ColorsResponse { 
  colors :: [Color]
} deriving (Show)

newtype ErrorResponse = ErrorResponse { 
  msg :: String
} deriving (Show)

deriveJSON defaultOptions ''ImageRequest

deriveJSON defaultOptions ''ColorsResponse

deriveJSON defaultOptions ''ErrorResponse

type Palette = [Color]

type FileName = String

type ErrorMessage = String

type RGB = (Int, Int, Int)

type Algorithm = Int -> [RGB] -> [RGB]

type ChannelRange = (Int, Int)

type Vect = [Double]

type KMeansInit = Int -> [PointHolder] -> [[PointHolder]]

data PointHolder = PointHolder { 
    normed :: Vect, 
    color :: RGB
}

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

type ComputedImage = R.Array R.U R.DIM2 RGB

-- (HEIGHT*WIDTH)x3 where matrix values are 
-- level values from 0..255
type PixelSpace = R.Array R.D R.DIM2 Word8