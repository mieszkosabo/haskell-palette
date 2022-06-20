module Components exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Css exposing (..)
import Css.Global exposing (..)
import Css.Transitions exposing (transition)
import Types exposing (..)
import Utils exposing (const, algorithmToString)

type alias StyledComponent a = List (Attribute a) -> List (Html a) -> Html a

styledH1 : StyledComponent a
styledH1 = styled Html.Styled.h1 [ 
    fontSize (px 48), 
    color (hex "#1E2126"),
    Css.marginBottom (Css.px 48) 
    ]

styledContainerOutside : StyledComponent a
styledContainerOutside = styled Html.Styled.div [
    Css.minHeight (Css.vh 100),
    Css.fontFamily Css.monospace,
    Css.backgroundColor (Css.hex "#f5f5f5")
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
    Css.marginBottom (Css.px 32)
    ]

uploadImageButton : List (Attribute a) -> List (Html a) -> Html a
uploadImageButton attrs children = styled Html.Styled.button [
    Css.width (Css.px 256),
    Css.padding (Css.px 0),
    Css.fontSize (Css.px 24),
    Css.borderRadius (Css.px  4),
    Css.cursor (Css.pointer),
    Css.backgroundColor (hex "#000000"),
    Css.outlineOffset (Css.px 4),
    Css.borderStyle (Css.none),
    Css.fontFamily Css.monospace,
    Css.active [
        descendants [ 
            Css.Global.typeSelector "span" [ 
                    Css.transform (Css.translateY (Css.px -2))
                ]
            ]
    ]
    ] attrs [
        styled Html.Styled.span [
            Css.display (Css.block),
            Css.padding2 (Css.px 8) (Css.px 16),
            Css.borderRadius (Css.px  4),
            color (hex "#f5f5f5"),
            Css.backgroundColor (hex "#8C2703"),
            Css.transform (Css.translateY (Css.px -6)),
            transition [Css.Transitions.transform 250],
            Css.hover [
               Css.transform (Css.translateY (Css.px -8)) 
            ]
        ] [] children
    ]

emptyNode : Html msg
emptyNode = Html.Styled.text ""

colorSquareSize = 64

pickedColor : Types.Color -> Html a
pickedColor c = styled Html.Styled.div [
    Css.width (Css.pt colorSquareSize),
    Css.height (Css.pt colorSquareSize),
    Css.backgroundColor (hex c),
    Css.margin (Css.pt 4)
    ] [] []

describedColor : Types.Color -> Html a
describedColor c = styled Html.Styled.div [
    Css.paddingBottom (Css.pt 4),
    Css.textTransform Css.uppercase,
    Css.textAlign Css.center,
    Css.marginBottom (Css.px 32)
    ] [] [ 
        pickedColor c,
        Html.Styled.text c
        ]

colorsPalette : List Types.Color -> Html a
colorsPalette colors = styled Html.Styled.div [
    Css.width (Css.pct 100),
    Css.displayFlex,
    Css.flexDirection Css.row,
    Css.flexWrap Css.wrap,
    Css.justifyContent Css.center
    ] [] (List.map describedColor colors)

colorSkeleton : Html a
colorSkeleton = styled Html.Styled.div [
    Css.width (Css.pt colorSquareSize),
    Css.height (Css.pt colorSquareSize),
    Css.backgroundColor (hex "#D3D3D3"),
    Css.property "animation" "glow 1.5s ease-in-out infinite",
    Css.margin (Css.pt 4)
    ] [] []

colorsSkeleton : Html a
colorsSkeleton = styled Html.Styled.div [
    Css.width (Css.pct 100),
    Css.displayFlex,
    Css.flexDirection Css.row,
    Css.flexWrap Css.wrap,
    Css.paddingBottom (Css.px 32),
    Css.justifyContent Css.center
    ] [] (List.repeat 6 colorSkeleton)

skeletonAnimation = Css.Global.global [Css.Global.selector "@keyframes glow"
    [ Css.property "0%, 100% { opacity" "1; } 50% { opacity: 0.5; }" ]]

radio : String -> Bool -> a -> Html a
radio value isChecked msg =
    styled Html.Styled.label 
    [ Css.displayFlex,
      Css.padding2 (Css.px 0) (Css.px 4),
      Css.alignItems (Css.center),
      Css.cursor (Css.pointer)
    ] []
    [
        styled Html.Styled.input 
        [ Css.margin (Css.px 6), 
          Css.cursor (Css.pointer) 
        ]
        [ 
            type_ "radio", 
            name "algorithm", 
            onInput (const msg), 
            Html.Styled.Attributes.checked isChecked 
            ] []
    , text value
    ]


algorithmPicker : Algorithm -> Html Msg
algorithmPicker algorithm  = styled Html.Styled.div [  
    Css.marginBottom (Css.px 32),
    Css.width (Css.pct 100),
    Css.displayFlex,
    Css.alignItems (Css.center),
    Css.justifyContent (Css.center)
    ] [] (
        styled Html.Styled.p 
        [ fontWeight bold, 
          Css.paddingRight (Css.px 16) 
        ]  [] [ text "Algorithm:" ]
    :: (List.map 
        (\algo -> radio (algorithmToString algo) (algorithm == algo) (ChangeAlgorithm algo)) 
        availableAlgorithms)
        )