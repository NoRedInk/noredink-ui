module Nri.Ui.Balloon.V2 exposing
    ( balloon
    , Attribute
    , green, purple, orange, white, navy
    , onBottom, onLeft, onRight, onTop
    , widthPx, widthPct
    , paddingPx
    )

{-| Adding a tooltip? Use `Nri.Ui.Tooltip`, not Balloon.
Balloon is really just a container: it is non-interactive and isn't semantically meaningful.


# Changelog

Changes from V1:

  - allow for far more customization:
      - background color
      - HTML attributes
  - change the API to be more similar to other NRI components

@docs balloon


## Customizations for Balloon

Use these if you don't want the standard green balloon

@docs Attribute
@docs green, purple, orange, white, navy
@docs onBottom, onLeft, onRight, onTop
@docs widthPx, widthPct
@docs paddingPx

    balloon [ green, onTop ] (text "hello")

-}

import Css exposing (..)
import Html.Styled as Html exposing (Html, div, styled)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Shadows.V1 as Shadows



-- TOOLTIP VIEWS


{-| Green balloon without an arrow by default.

     __________
    |         |
    |_________|

-}
balloon : List Attribute -> Html msg -> Html msg
balloon customizations content =
    custom (customizationsToConfig customizations) content


{-| Balloon's attributes.
-}
type Attribute
    = Theme Theme
    | Position Position
    | WidthPx Float
    | WidthPct Float
    | PaddingPx Float


{-| Balloon with the arrow on the bottom.

     __________
    |         |
    |___  ____|
        \/

-}
onTop : Attribute
onTop =
    Position OnTop


{-| Balloon with the arrow on the left.

      __________
     |         |
    <          |
     |_________|

-}
onRight : Attribute
onRight =
    Position OnRight


{-| Balloon with the arrow on the top.

     ___/\_____
    |         |
    |_________|

-}
onBottom : Attribute
onBottom =
    Position OnBottom


{-| Balloon with the arrow on the right.

      __________
     |         |
     |          >
     |_________|

-}
onLeft : Attribute
onLeft =
    Position OnLeft


{-| Green theme (This is the default theme.)
-}
green : Attribute
green =
    Theme Green


{-| Orange theme
-}
orange : Attribute
orange =
    Theme Orange


{-| Purple theme
-}
purple : Attribute
purple =
    Theme Purple


{-| White theme
-}
white : Attribute
white =
    Theme White


{-| Navy theme
-}
navy : Attribute
navy =
    Theme Navy


{-| Width of the balloon in pixels.
-}
widthPx : Float -> Attribute
widthPx =
    WidthPx


{-| Warning: using a percentage-based width may change the positioning of the element
in unexpected ways.
-}
widthPct : Float -> Attribute
widthPct =
    WidthPct


{-| Padding of the balloon in pixels.
-}
paddingPx : Float -> Attribute
paddingPx =
    PaddingPx



-- INTERNALS


type alias Config =
    { position : Position
    , theme : Theme
    , css : List Css.Style
    , arrowSize : ArrowSize
    }


{-| Default configuration
-}
defaultConfig : Config
defaultConfig =
    { position = NoArrow
    , theme = Green
    , css = [ Css.padding (Css.px 20) ]
    , arrowSize = Medium
    }


{-| The Arrow may be positioned on any edge of the balloon, facing outward.
-}
type Position
    = OnTop
    | OnRight
    | OnBottom
    | OnLeft
    | NoArrow


{-| NOTE: Double check with the design team if the spec calls for something else.
-}
type Theme
    = Orange
    | Green
    | Purple
    | White
    | Navy


type ArrowSize
    = Medium


custom : Config -> Html msg -> Html msg
custom config content =
    container config.position
        [ viewBalloon config.theme config.css [ content ]
        , case config.position of
            NoArrow ->
                Html.text ""

            _ ->
                viewArrow config.position config.theme <|
                    case config.arrowSize of
                        Medium ->
                            8 * 2
        ]


container : Position -> List (Html msg) -> Html msg
container position =
    styled div
        (case position of
            OnTop ->
                [ Css.display Css.inlineFlex
                , Css.flexDirection Css.column
                , Css.alignItems Css.center
                ]

            OnBottom ->
                [ Css.display Css.inlineFlex
                , Css.flexDirection Css.columnReverse
                , Css.alignItems Css.center
                ]

            OnLeft ->
                [ Css.display Css.inlineFlex
                , Css.flexDirection Css.row
                , Css.alignItems Css.center
                ]

            OnRight ->
                [ Css.display Css.inlineFlex
                , Css.flexDirection Css.rowReverse
                , Css.alignItems Css.center
                ]

            NoArrow ->
                []
        )
        []


viewBalloon : Theme -> List Css.Style -> List (Html msg) -> Html msg
viewBalloon theme_ styles contents =
    styled div
        [ display inlineBlock
        , lineHeight (num 1.4)
        , textAlign left
        , position relative
        , Css.borderRadius (px 8)
        , Shadows.high
        , balloonTheme theme_
        , Css.batch styles
        ]
        []
        contents


balloonTheme : Theme -> Css.Style
balloonTheme theme =
    case theme of
        Orange ->
            batch
                [ backgroundColor Colors.sunshine
                , border3 (px 1) solid Colors.sunshine
                , Fonts.baseFont
                , fontSize (px 15)
                , color Colors.gray20
                ]

        Purple ->
            batch
                [ backgroundColor Colors.purple
                , border3 (px 1) solid Colors.purple
                , Fonts.baseFont
                , fontSize (px 15)
                , color Colors.white
                ]

        White ->
            batch
                [ backgroundColor Colors.white
                , border3 (px 1) solid Colors.white
                , Fonts.baseFont
                , fontSize (px 15)
                , color Colors.gray20
                ]

        Green ->
            batch
                [ backgroundColor Colors.greenDarkest
                , border3 (px 1) solid Colors.greenDarkest
                , Fonts.baseFont
                , fontSize (px 15)
                , color Colors.white
                ]

        Navy ->
            batch
                [ backgroundColor Colors.navy
                , border3 (px 1) solid Colors.navy
                , Fonts.baseFont
                , fontSize (px 15)
                , color Colors.white
                ]


viewArrow : Position -> Theme -> Float -> Html msg
viewArrow position theme diagonal =
    let
        arrowSideWidth =
            sqrt ((diagonal ^ 2) / 2)
    in
    styled div
        [ arrowPosition position
        , arrowTheme theme
        , Css.height (px arrowSideWidth)
        , Css.width (px arrowSideWidth)
        ]
        []
        []


arrowPosition : Position -> Css.Style
arrowPosition position =
    case position of
        OnTop ->
            batch
                [ Css.property "transform" "translateY(-50%) rotate(45deg)"
                , Css.property "transform-origin" "center"
                , Css.borderTop Css.zero
                , Css.borderLeft Css.zero
                , Css.borderTopLeftRadius (pct 100)
                , Css.flexShrink (Css.num 0)
                ]

        OnBottom ->
            batch
                [ Css.property "transform" "translateY(50%) rotate(45deg)"
                , Css.property "transform-origin" "center"
                , Css.borderRight Css.zero
                , Css.borderBottom Css.zero
                , Css.borderBottomRightRadius (pct 100)
                , Css.flexShrink (Css.num 0)
                ]

        OnLeft ->
            batch
                [ Css.property "transform" "translateX(-50%) rotate(45deg)"
                , Css.property "transform-origin" "center"
                , Css.borderBottom Css.zero
                , Css.borderLeft Css.zero
                , Css.borderBottomLeftRadius (pct 100)
                , Css.flexShrink (Css.num 0)
                ]

        OnRight ->
            batch
                [ Css.property "transform" "translateX(50%) rotate(45deg)"
                , Css.property "transform-origin" "center"
                , Css.borderRight Css.zero
                , Css.borderTop Css.zero
                , Css.borderTopRightRadius (pct 100)
                , Css.flexShrink (Css.num 0)
                ]

        NoArrow ->
            batch
                []


arrowTheme : Theme -> Css.Style
arrowTheme theme =
    case theme of
        Orange ->
            batch
                [ backgroundColor Colors.sunshine
                , border3 (px 1) solid Colors.sunshine
                , Fonts.baseFont
                , fontSize (px 15)
                , color Colors.gray20
                ]

        Purple ->
            batch
                [ backgroundColor Colors.purple
                , border3 (px 1) solid Colors.purple
                , Fonts.baseFont
                , fontSize (px 15)
                , color Colors.white
                ]

        White ->
            batch
                [ backgroundColor Colors.white
                , border3 (px 1) solid Colors.white
                , Fonts.baseFont
                , fontSize (px 15)
                , color Colors.gray20
                ]

        Green ->
            batch
                [ backgroundColor Colors.greenDarkest
                , border3 (px 1) solid Colors.greenDarkest
                , Fonts.baseFont
                , fontSize (px 15)
                , color Colors.white
                ]

        Navy ->
            batch
                [ backgroundColor Colors.navy
                , border3 (px 1) solid Colors.navy
                , Fonts.baseFont
                , fontSize (px 15)
                , color Colors.white
                ]


customizationsToConfig : List Attribute -> Config
customizationsToConfig customizations =
    List.foldl customize defaultConfig customizations


customize : Attribute -> Config -> Config
customize customization config =
    case customization of
        Position position ->
            { config | position = position }

        Theme theme ->
            { config | theme = theme }

        WidthPx width_ ->
            { config | css = Css.width (Css.px width_) :: config.css }

        WidthPct width_ ->
            { config | css = Css.width (Css.pct width_) :: config.css }

        PaddingPx length ->
            { config | css = Css.padding (Css.px length) :: config.css }
