module Main exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Css exposing (..)

import Http
import File
import File.Select as Select
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)
import Browser
import Task

import Components exposing (..)
import Utils exposing (..)
import Types exposing (..)

endpointUrl : String
endpointUrl = "http://localhost:3000/upload"

apiRequestEncoder : Algorithm -> ImageString -> Encode.Value
apiRequestEncoder algorithm image = Encode.object [
    ("algorithm", Encode.string (algorithmToString algorithm)),
    ("image", Encode.string image)
    ]

paletteDecoder : Decode.Decoder (List Types.Color)
paletteDecoder = Decode.list Decode.string


getPaletteFromImage : Algorithm -> ImageString -> Cmd Msg
getPaletteFromImage algorithm image = Http.post {
    url = endpointUrl,
    body = Http.jsonBody (apiRequestEncoder algorithm image),
    expect = Http.expectJson GotPalette paletteDecoder
    }

initialState : State
initialState = {
    algorithm = Nothing,
    image = Nothing,
    colors = [],
    errorMessage = Nothing
    }

imageView : Maybe ImageString -> Html a
imageView image = case image of
                        Nothing -> emptyNode
                        Just content -> styledImg [ src content ] []

view : State -> Html Msg
view model =
    styledContainerOutside [] [
        styledContainerInside [] [
            styledH1 [] [ text "Haskell Palette Demo" ],
            uploadImageButton [ onClick ChooseFileRequest ] [ text "upload image" ],
            colorsPalette model.colors,
            imageView model.image            
        ]
    ]

update : Msg -> State -> (State, Cmd Msg)
update msg state =
    case msg of
        ChooseFileRequest -> (state, Select.file ["image/jpeg", "image/png"] FileSelected)
        FileSelected file -> (state, Task.perform FileLoaded (File.toUrl file))
        FileLoaded content -> (
            { state | image = Just content },
            getPaletteFromImage 
                (Maybe.withDefault KmeansPP state.algorithm) 
                (imageUrlToPureBase64 content)
            )
        GotPalette (Ok palette) -> ({ state | colors = palette }, Cmd.none)
        GotPalette (Err error) -> (
            { state | errorMessage = Just (buildErrorMessage error) },
            Cmd.none
            )

main : Program () State Msg
main = Browser.element { 
    init = const (initialState, Cmd.none),
    view = view >> toUnstyled,
    update = update,
    subscriptions = const Sub.none
    }
