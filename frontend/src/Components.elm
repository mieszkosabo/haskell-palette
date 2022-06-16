module Components exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Css exposing (..)
import Types exposing (..)

type alias StyledComponent a = List (Attribute a) -> List (Html a) -> Html a

styledH1 : StyledComponent a
styledH1 = styled Html.Styled.h1 [ fontSize (px 48), color (hex "#8C2703") ]

styledContainerOutside : StyledComponent a
styledContainerOutside = styled Html.Styled.div [
    Css.minHeight (Css.vh 100),
    Css.fontFamily Css.monospace,
    Css.backgroundColor (Css.hex "#F2F2F2")
    ]

styledContainerInside : StyledComponent a
styledContainerInside = styled Html.Styled.div [
    Css.displayFlex, 
    Css.flexDirection Css.column,
    Css.alignItems Css.center,
    Css.padding (px 64)
    ]

styledImg : StyledComponent a
styledImg = styled Html.Styled.img [
    Css.maxHeight (Css.px 320),
    Css.padding (Css.px 64)
    ]

uploadImageButton : StyledComponent a
uploadImageButton = styled Html.Styled.button [
    Css.width (Css.pct 75),
    Css.padding2 (Css.px 8) (Css.px 16),
    Css.fontSize (Css.px 24),
    color (hex "#8C2703"),
    Css.fontFamily Css.monospace
    ]

emptyNode : Html msg
emptyNode = Html.Styled.text ""

pickedColor : Types.Color -> Html a
pickedColor c = styled Html.Styled.div [
    Css.width (Css.pt 64),
    Css.height (Css.pt 64),
    Css.backgroundColor (hex c),
    Css.margin (Css.pt 4)
    ] [] [ emptyNode ]

describedColor : Types.Color -> Html a
describedColor c = styled Html.Styled.div [
    Css.paddingBottom (Css.pt 4),
    Css.textTransform Css.uppercase,
    Css.textAlign Css.center
    ] [] [ 
        pickedColor c,
        Html.Styled.text c
        ]

colorsPalette : List Types.Color -> Html a
colorsPalette colors = styled Html.Styled.div ([
    Css.width (Css.pct 100),
    Css.displayFlex,
    Css.flexDirection Css.row,
    Css.flexWrap Css.wrap,
    Css.justifyContent Css.center
    ]) [] (List.map describedColor colors)
