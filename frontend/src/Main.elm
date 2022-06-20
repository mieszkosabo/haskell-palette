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
endpointUrl = "/upload"

apiRequestEncoder : Algorithm -> ImageString -> Encode.Value
apiRequestEncoder algorithm image = Encode.object [
    ("algorithm", Encode.string (algorithmToJSONString algorithm)),
    ("image", Encode.string image)
    ]

paletteDecoder : Decode.Decoder (List Types.Color)
paletteDecoder = Decode.field "colors" (Decode.list Decode.string)


getPaletteFromImage : Algorithm -> ImageString -> Cmd Msg
getPaletteFromImage algorithm image = Http.post {
    url = endpointUrl,
    body = Http.jsonBody (apiRequestEncoder algorithm image),
    expect = Http.expectJson GotPalette paletteDecoder
    }

initialState : State
initialState = NoImage Histogram

imageView : ImageString -> Html a
imageView image = styledImg [ src image ] []

view : State -> Html Msg
view state =
    styledContainerOutside [] [
        styledContainerInside [] (
            styledH1 [] [ text "Haskell Palette Demo" ] ::
            case state of
                NoImage algorithm -> ([
                    algorithmPicker algorithm,
                    uploadImageButton [ onClick ChooseFileRequest ] [ text "Upload image!" ]
                    ])
                Loading _ image -> ([
                    imageView image,
                    colorsSkeleton,
                    skeletonAnimation
                    ])
                ShowingPalette algorithm image colors -> ([
                    algorithmPicker algorithm,
                    imageView image,
                    colorsPalette colors,
                    uploadImageButton [ onClick Reset ] [ text "Try again!" ]
                    ])
                Error errorMessage -> ([
                    Html.Styled.h2 [] [ text ("Unexpected error occured: " ++ errorMessage) ],
                    uploadImageButton [ onClick Reset ] [ text "Try again!" ]
                    ])
            )
    ]

update : Msg -> State -> (State, Cmd Msg)
update msg state =
        case state of 
            NoImage algorithm ->
                case msg of
                    ChooseFileRequest -> (state, Select.file ["image/jpeg", "image/png", "image/bitmap", "image/gif", "image/tiff"] FileSelected)
                    FileSelected file -> (state, Task.perform FileLoaded (File.toUrl file))
                    FileLoaded image -> (
                        Loading algorithm image,
                        getPaletteFromImage 
                            algorithm
                            (imageUrlToPureBase64 image)
                        )
                    ChangeAlgorithm algo -> (NoImage algo, Cmd.none)
                    _ -> (state, Cmd.none)
            Loading algorithm image ->
                case msg of
                    GotPalette (Ok palette) -> (
                        ShowingPalette algorithm image palette,
                        Cmd.none
                        )
                    GotPalette (Err error) -> (
                        Error (buildErrorMessage error),
                        Cmd.none
                        )
                    _ -> (state, Cmd.none)
            ShowingPalette algorithm image _ -> 
                case msg of
                    ChangeAlgorithm newAlgorithm -> (
                        Loading algorithm image,
                        getPaletteFromImage newAlgorithm (imageUrlToPureBase64 image)
                        )
                    Reset -> (NoImage Histogram, Cmd.none)
                    _ -> (state, Cmd.none)
            Error _ -> 
                case msg of
                    Reset -> (NoImage Histogram, Cmd.none)
                    _ -> (state, Cmd.none)

main : Program () State Msg
main = Browser.element { 
    init = const (initialState, Cmd.none),
    view = view >> toUnstyled,
    update = update,
    subscriptions = const Sub.none
    }
