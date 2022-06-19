module Types exposing (..)

import Http
import File exposing (File)

type alias Color = String
type alias ImageString = String
type Algorithm = MedianCut | Kmeans | KmeansPP
type LocalState = Initial | Loading | ShowingPalette | Error

-- TODO: refactor into union of possible states (eg. we know that image
-- is Nothing in Error localState, and Just image in ShowingPalette, etc...)
type alias State = {
    state: LocalState,
    algorithm: Maybe Algorithm,
    image: Maybe ImageString,
    colors : List Color,
    errorMessage : Maybe String
    }


type alias Palette = List Color

type Msg 
    = ChooseFileRequest 
    | FileSelected File 
    | FileLoaded String
    | GotPalette (Result Http.Error (List Color))