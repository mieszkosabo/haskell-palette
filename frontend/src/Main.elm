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

apiRequestEncoder : Algorithm -> Int -> ImageString -> Encode.Value
apiRequestEncoder algorithm count image = Encode.object [
    ("algorithm", Encode.string (algorithmToJSONString algorithm)),
    ("image", Encode.string image),
    ("count", Encode.int count)
    ]

paletteDecoder : Decode.Decoder (List Types.Color)
paletteDecoder = Decode.field "colors" (Decode.list Decode.string)

getPaletteFromImage : Algorithm -> Int -> ImageString -> Cmd Msg
getPaletteFromImage algorithm count image = Http.post {
    url = endpointUrl,
    body = Http.jsonBody (apiRequestEncoder algorithm count image),
    expect = Http.expectJson GotPalette paletteDecoder
    }

initialState : State
initialState = NoImage Histogram defaultCount

imageView : ImageString -> Html a
imageView image = styledImg [ src image ] []

view : State -> Html Msg
view state =
    styledContainerOutside [] [
        styledContainerInside [] (
            styledH1 [] [ text "Haskell Palette Demo" ] ::
            case state of
                NoImage algorithm count -> ([
                    settingsPicker algorithm count,
                    uploadImageButton [ onClick ChooseFileRequest ] [ text "Upload image!" ]
                    ])
                Loading _ _ image -> ([
                    imageView image,
                    colorsSkeleton,
                    skeletonAnimation
                    ])
                ShowingPalette algorithm count image colors -> ([
                    settingsPicker algorithm count,
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
            NoImage algorithm count ->
                case msg of
                    ChooseFileRequest -> (state, Select.file ["image/jpeg", "image/png", "image/bitmap", "image/gif", "image/tiff"] FileSelected)
                    FileSelected file -> (state, Task.perform FileLoaded (File.toUrl file))
                    FileLoaded image -> (
                        Loading algorithm count image,
                        getPaletteFromImage algorithm count (imageUrlToPureBase64 image)
                        )
                    ChangeAlgorithm newAlgorithm -> (NoImage newAlgorithm count, Cmd.none)
                    ChangeCount newCount -> (NoImage algorithm newCount, Cmd.none)
                    _ -> (state, Cmd.none)
            Loading algorithm count image ->
                case msg of
                    GotPalette (Ok palette) -> (
                        ShowingPalette algorithm count image palette,
                        Cmd.none
                        )
                    GotPalette (Err error) -> (
                        Error (buildErrorMessage error),
                        Cmd.none
                        )
                    _ -> (state, Cmd.none)
            ShowingPalette algorithm count image _ -> 
                case msg of
                    ChangeAlgorithm newAlgorithm -> (
                        Loading newAlgorithm count image,
                        getPaletteFromImage newAlgorithm count (imageUrlToPureBase64 image)
                        )
                    ChangeCount newCount -> (
                        Loading algorithm newCount image,
                        getPaletteFromImage algorithm newCount (imageUrlToPureBase64 image)
                        )
                    Reset -> (initialState, Cmd.none)
                    _ -> (state, Cmd.none)
            Error _ -> 
                case msg of
                    Reset -> (initialState, Cmd.none)
                    _ -> (state, Cmd.none)

main : Program () State Msg
main = Browser.element { 
    init = const (initialState, Cmd.none),
    view = view >> toUnstyled,
    update = update,
    subscriptions = const Sub.none
    }
