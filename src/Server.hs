{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Server (server) where

import Network.Wai.Middleware.RequestLogger
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLU
import qualified Web.Scotty as Scot
import qualified Control.Monad.IO.Class as Cm
import qualified Data.Digest.Pure.SHA as Dps
import qualified Model as M
import qualified Codec.Picture.Repa as CR
import qualified Data.Array.Repa as R
import qualified Data.ByteString.Base64 as B64
import Util (strToBStr, imgHist, envVarString)
import Data.Monoid (mconcat)
import GHC.Word (Word8)

htmlSourceDir :: IO String
htmlSourceDir = envVarString "SOURCE_PATH" "/workspaces/haskell-palette/frontend/"

samplePalette :: [String]
samplePalette = [
                "#8c2703",
                "#973c1c",
                "#a35235",
                "#ae674e",
                "#ba7d67"
                ]

decodeImage :: String -> Either String (R.Array R.D R.DIM3 Word8)
decodeImage imgBase64 = do
  let bimg = strToBStr imgBase64
  dimg <- B64.decode bimg
  rgb <- CR.decodeImageRGB dimg
  return $ CR.imgData rgb

generatePalette :: M.ImageRequest -> Either String M.ColorsResponse
generatePalette request = do 
  let img = M.image request
  pixels <- decodeImage img
  let !hist = imgHist pixels
  return M.ColorsResponse { M.colors = samplePalette }

server :: IO ()
server = do
  sourceDir <- htmlSourceDir

  Scot.scotty 3000 $ do
    Scot.middleware logStdoutDev

    Scot.get "/" $ do
      Scot.file $ sourceDir ++ "index.html"

    Scot.post "/upload" $ do
      request <- Scot.jsonData :: Scot.ActionM M.ImageRequest
      case generatePalette request of
        Right response -> Scot.json response
        Left msg -> Scot.text $ TL.pack msg
