{-# LANGUAGE OverloadedStrings #-}

module Server (server) where

import Network.Wai.Middleware.RequestLogger
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import qualified Web.Scotty as Scot
import qualified Control.Monad.IO.Class as Cm
import qualified Data.Digest.Pure.SHA as Dps
import qualified Model as M

import Data.Monoid (mconcat)

htmlSourceDir :: String
htmlSourceDir = "/workspaces/haskell-palette/frontend/"

samplePalette :: [String]
samplePalette = [
                "#8c2703",
                "#973c1c",
                "#a35235",
                "#ae674e",
                "#ba7d67"
                ]

generatePalette :: M.ImageRequest -> IO M.ColorsResponse
generatePalette request = do 
  -- just sample action done on request algorithm field,
  -- let's work on image field that is a string represenation of image
  (print . M.algorithm) request 
  return $ M.ColorsResponse { M.colors = samplePalette }

server :: IO ()
server = Scot.scotty 3000 $ do
  Scot.middleware logStdoutDev

  Scot.get "/" $ do
    Scot.file $ htmlSourceDir ++ "index.html"

  Scot.post "/upload" $ do
    request <- Scot.jsonData :: Scot.ActionM M.ImageRequest
    palette <- (Cm.liftIO . generatePalette) request
    Scot.json palette