{-# LANGUAGE RecordWildCards #-}

module Image (loadImage, FileName, ErrorMessage, Image) where

import qualified Data.Either.Combinators as E
import qualified Codec.Picture as P
import Data.Array.Repa as R hiding ((++))
import CommonTypes

-- alias for JuicyImage type for brevity
type Image = P.Image P.PixelRGB8

loadImage :: FileName -> IO (Either ErrorMessage (P.Image P.PixelRGB8))
loadImage fileName = do
    io <- P.readImage fileName
    return $ E.mapBoth ((++) "Error: Unable to load the image: ") P.convertRGB8 io

type RGB8 = (P.Pixel8, P.Pixel8, P.Pixel8)

fromImage :: P.Image P.PixelRGB8 -> Array D DIM2 RGB8
fromImage img@P.Image {..} =
  R.fromFunction
    (Z :. imageWidth :. imageHeight)
    (\(Z :. x :. y) ->
       let (P.PixelRGB8 r g b) = P.pixelAt img x y
       in (r, g, b))