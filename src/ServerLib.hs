{-# LANGUAGE OverloadedStrings #-}

module ServerLib (server) where

import Network.Wai.Middleware.RequestLogger
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import qualified Web.Scotty as Scot
import qualified Control.Monad.IO.Class as Cm
import qualified Data.Digest.Pure.SHA as Dps

import Data.Monoid (mconcat)

htmlSourceDir :: String
htmlSourceDir = "/haskell-palette/"

tempDataDir :: String
tempDataDir = "/tmp/"

server :: IO ()
server = Scot.scotty 3000 $ do
  Scot.middleware logStdoutDev

  Scot.get "/" $ do
    Scot.file $ htmlSourceDir ++ "index.html"

  Scot.post "/upload" $ do
    contents <- Scot.body
    let fileName = Dps.showDigest $ Dps.sha256 contents
    Cm.liftIO $ BL.writeFile (tempDataDir ++ fileName) contents
    Scot.text "File uploaded"

someFunc :: IO ()
someFunc = putStrLn "someFunc"
