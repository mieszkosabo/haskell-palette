module Color where

import qualified Util as U
import qualified Model as M

import Numeric

-- we are averaging the squares of the colors because it gives better results:
-- https://sighack.com/post/averaging-rgb-colors-the-right-way
avarageColor :: [M.RGB] -> M.RGB
avarageColor colors = U.listToTuple3 squared
  where
    numOfColors = length colors
    squared = map (\x -> round $ sqrt (fromIntegral x / fromIntegral numOfColors)) (U.tupleToList3 multiplied)
    multiplied = foldr (\(r, g, b) (accR, accG, accB) -> (accR + r * r, accG + g * g, accB + b * b)) (0, 0, 0) colors

rgbToHex :: M.RGB -> String
rgbToHex (r, g, b) = '#' : concatMap convertToPaddedHex [r, g, b]
  where convertToPaddedHex x 
          = let hex = showHex x "" in 
            if length hex == 1 
              then '0' : hex 
              else hex