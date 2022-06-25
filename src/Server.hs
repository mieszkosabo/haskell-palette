{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Server (server, testPar) where

import qualified Model as M
import qualified Color as C
import qualified Util as U
import qualified Image as I
import qualified Web.Scotty as Scot
import qualified Data.Array.Repa as R
import qualified Data.List as L
import qualified Codec.Picture.Repa as CR

import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import GHC.Word (Word8)


htmlSourceDir :: IO String
htmlSourceDir = U.envVarString "SOURCE_PATH" "/workspaces/haskell-palette/frontend/"

port :: IO Int
port = U.envVarInt "PORT" 3000

algorithmImplementation :: M.ImageRequest -> Either M.ErrorMessage M.Algorithm
algorithmImplementation request = case M.algorithm request of
  "histogram" -> Right I.histogram
  "median_cut" -> Right I.medianCut
  "k_means" -> Right I.kmeansStd
  "k_means_pp" -> Right I.kmeansPP
  _ -> Left "Algorithm not implemented"

validateCount :: Int -> Either M.ErrorMessage Int
validateCount x | x < 1 = Left "Too small clusters number"
                | otherwise = Right x

runAlgorithm :: M.Algorithm -> Int -> CR.Img CR.RGBA -> Either M.ErrorMessage [M.Color]
runAlgorithm algorithm k dimg = do
  preprocessedImg <- I.preprocessImage dimg
  let points = I.imagePoints preprocessedImg
  let colors = L.sort $ algorithm k points
  return $ L.map C.rgbToHex colors

generatePalette :: M.ImageRequest -> Either M.ErrorMessage M.ColorsResponse
generatePalette request = do 
  algorithm <- algorithmImplementation request
  count <- validateCount $ M.count request
  let img = M.image request
  decodedImg <- I.decodeImage img
  palette <- runAlgorithm algorithm count decodedImg
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
        Left msg -> Scot.json $ M.ErrorResponse msg

testPar :: IO ()
testPar = do
  img <- CR.readImageRGBA "image.jpg"
  case img of
    Right dimg -> case runAlgorithm I.kmeansStd 8 dimg of
                    Right palette -> print palette
                    Left msg -> putStrLn msg
    Left msg -> putStrLn msg
