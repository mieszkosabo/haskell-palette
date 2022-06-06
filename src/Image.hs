module Image (loadImage, FileName, ErrorMessage, Image) where
import qualified Codec.Picture as P

type FileName = String
type ErrorMessage = String

-- alias for JuicyImage type for brevity
type Image = P.Image P.PixelRGBA8

loadImage :: FileName -> IO (Either ErrorMessage (P.Image P.PixelRGBA8))
loadImage fileName = do
    io <- P.readImage fileName

    case io of
        Left err -> return $ Left "Error: Unable to load the image:"
        Right dynamicImage -> return $ Right $ P.convertRGBA8 dynamicImage

