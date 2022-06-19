{-# LANGUAGE DeriveGeneric #-}

module Model where

import qualified Data.Array.Repa as R
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import GHC.Word (Word8)

data ImageRequest = ImageRequest { 
  algorithm :: String, 
  image :: String
 } deriving (Show, Generic)

data ColorsResponse = ColorsResponse { 
  colors :: [Color]
  } deriving (Show, Generic)

instance ToJSON ColorsResponse

instance FromJSON ImageRequest

type Color = String

type FileName = String

type ErrorMessage = String

-- 256x256x256 where matrix value is represents count
-- of pixels of given RGB color
type Histogram = R.Array R.D R.DIM3 Word8

-- HEIGHTxWIDTHx3 where matrix values are 
-- level values from 0..255
type RawImage = R.Array R.D R.DIM3 Word8