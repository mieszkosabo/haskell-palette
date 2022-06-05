{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified MyLib (someFunc)

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import qualified Data.Text.Lazy as TL

import Data.Monoid (mconcat)

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  get "/:test" $ do
    test <- param "test"
    text $ mconcat ["You're at ", test, " page!"]

  post "/upload/image" $ do
    formContent <- (param "image[data]") `rescue` (\msg -> return msg)
    text $ "received: " `TL.append` formContent