{-# LANGUAGE DeriveGeneric #-}

module Model where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

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
