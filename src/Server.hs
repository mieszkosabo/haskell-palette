{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Server (server) where

import qualified Data.Text.Lazy as TL
import qualified Model as M
import qualified Color as C
import qualified Util as U
import qualified Image as I
import qualified Web.Scotty as Scot
import qualified Data.Array.Repa as R
import qualified Data.List as L

import Network.Wai.Middleware.RequestLogger


htmlSourceDir :: IO String
htmlSourceDir = U.envVarString "SOURCE_PATH" "/workspaces/haskell-palette/frontend/"

port :: IO Int
port = U.envVarInt "PORT" 3000

algorithmImplementation :: M.ImageRequest -> Either M.ErrorMessage M.Algorithm
algorithmImplementation request = case M.algorithm request of
  "histogram" -> Right I.histogram
  "median_cut" -> Right I.medianCut
  "k_means" -> Right I.kmeans
  _ -> Left "Algorithm not implemented"

generatePalette :: M.ImageRequest -> Either M.ErrorMessage M.ColorsResponse
generatePalette request = do 
  algorithm <- algorithmImplementation request
  let count = M.count request
  let img = M.image request
  resizedImg <- I.decodeImage img >>= I.resize
  computedImg <- R.computeP resizedImg :: Either M.ErrorMessage M.ComputedImage
  let points = I.imagePoints computedImg
  let colors = L.sort $ algorithm count points
  let palette = L.map C.rgbToHex colors
  return M.ColorsResponse { M.colors = palette }

server :: IO ()
server = do
  htmlSourceDir' <- htmlSourceDir
  port' <- port

  Scot.scotty port' $ do
    Scot.middleware logStdoutDev

    Scot.get "/" $ do
      Scot.file $ htmlSourceDir' ++ "index.html"

    Scot.post "/upload" $ do
      request <- Scot.jsonData :: Scot.ActionM M.ImageRequest
      case generatePalette request of
        Right response -> Scot.json response
        Left msg -> Scot.text $ TL.pack msg
