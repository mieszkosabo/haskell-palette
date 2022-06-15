module Image (loadImage, FileName, ErrorMessage, Image) where

import qualified Data.Either.Combinators as E
import qualified Codec.Picture as P
import CommonTypes

-- alias for JuicyImage type for brevity
type Image = P.Image P.PixelRGBA8

loadImage :: FileName -> IO (Either ErrorMessage (P.Image P.PixelRGBA8))
loadImage fileName = do
    io <- P.readImage fileName
    return $ E.mapBoth ((++) "Error: Unable to load the image: ") P.convertRGBA8 io
