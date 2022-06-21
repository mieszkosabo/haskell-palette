{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Server (server) where

import qualified Data.Text.Lazy as TL
import qualified Model as M
import qualified Util as U
import qualified Image as I
import qualified Web.Scotty as Scot
import qualified Data.Array.Repa as R
import Network.Wai.Middleware.RequestLogger

htmlSourceDir :: IO String
htmlSourceDir = U.envVarString "SOURCE_PATH" "/workspaces/haskell-palette/frontend/"

port :: IO Int
port = U.envVarInt "PORT" 3000

samplePalette :: [String]
samplePalette = [
                "#8c2703",
                "#973c1c",
                "#a35235",
                "#ae674e",
                "#ba7d67"
                ]

generatePalette :: M.ImageRequest -> Either String M.ColorsResponse
generatePalette request = do 
  let img = M.image request
  img <- I.decodeImage img >>= I.resize
  computed <- R.computeP img :: Either String M.ComputedImage
  let res = I.medianCut computed
  return M.ColorsResponse { M.colors = res }

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
