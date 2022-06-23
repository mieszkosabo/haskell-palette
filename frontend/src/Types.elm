module Types exposing (..)

import Http
import File exposing (File)

type alias Color = String
type alias ImageString = String
type Algorithm = Histogram | MedianCut | Kmeans | KmeansPP

availableAlgorithms : List Algorithm
availableAlgorithms = [Histogram, MedianCut, Kmeans, KmeansPP]

type State = 
    NoImage Algorithm Int
    | Loading Algorithm Int ImageString
    | ShowingPalette Algorithm Int ImageString (List Color)
    | Error String

type alias Palette = List Color

type Msg 
    = ChooseFileRequest 
    | FileSelected File 
    | FileLoaded String
    | GotPalette (Result Http.Error (List Color))
    | Reset
    | ChangeAlgorithm Algorithm
    | ChangeCount Int