module Nri.Ui.Balloon.V2 exposing
    ( balloon
    , Attribute
    , green, purple, orange, white, navy
    , onBottom, onLeft, onRight, onTop
    , width
    , custom, id, nriDescription, testId
    , paddingPx
    )

{-| Adding a tooltip? Use `Nri.Ui.Tooltip`, not Balloon.
Balloon is really just a container: it is non-interactive and isn't semantically meaningful.


# Changelog

Changes from V1:

  - allow for far more customization:
      - background color
      - HTML attributes
  - change the API to be more similar to other NRI components (including adding the standard attributes)

@docs balloon


## Customizations for Balloon

Use these if you don't want the standard green balloon

@docs Attribute
@docs green, purple, orange, white, navy
@docs onBottom, onLeft, onRight, onTop
@docs width
@docs custom, id, nriDescription, testId

    balloon [ green, onTop ] (text "hello")

-}

import Css exposing (..)
import Html.Styled as Html exposing (Html, div, styled)
import Html.Styled.Attributes as Attributes
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.Shadows.V1 as Shadows



-- TOOLTIP VIEWS


{-| Green balloon without an arrow by default.

     __________
    |         |
    |_________|

-}
balloon : List Attribute -> Html msg -> Html msg
balloon customizations content =
    view (customizationsToConfig customizations) content


{-| Balloon's attributes.
-}
type Attribute
    = Attribute (Config -> Config)


setPosition : Position -> Attribute
setPosition position =
    Attribute (\config -> { config | position = position })


setTheme : Theme -> Attribute
setTheme theme =
    Attribute (\config -> { config | theme = theme })


{-| Balloon with the arrow on the bottom.

     __________
    |         |
    |___  ____|
        \/

-}
onTop : Attribute
onTop =
    setPosition OnTop


{-| Balloon with the arrow on the left.

      __________
     |         |
    <          |
     |_________|

-}
onRight : Attribute
onRight =
    setPosition OnRight


{-| Balloon with the arrow on the top.

     ___/\_____
    |         |
    |_________|

-}
onBottom : Attribute
onBottom =
    setPosition OnBottom


{-| Balloon with the arrow on the right.

      __________
     |         |
     |          >
     |_________|

-}
onLeft : Attribute
onLeft =
    setPosition OnLeft


{-| Green theme (This is the default theme.)
-}
green : Attribute
green =
    setTheme Green


{-| Orange theme
-}
orange : Attribute
orange =
    setTheme Orange


{-| Purple theme
-}
purple : Attribute
purple =
    setTheme Purple


{-| White theme
-}
white : Attribute
white =
    setTheme White


{-| Navy theme
-}
navy : Attribute
navy =
    setTheme Navy


{-| Width of the balloon.

Warning: using a percentage-based width may change the positioning of the element
in unexpected ways.

-}
width : Css.LengthOrAuto compatible -> Attribute
width width_ =
    Attribute (\config -> { config | css = Css.width width_ :: config.css })


paddingPx : Float -> Attribute
paddingPx length =
    Attribute (\config -> { config | css = Css.padding (Css.px length) :: config.css })


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way you want/expect if underlying Balloon styles change.
Instead, please use the `css` helper.

-}
custom : List (Html.Attribute Never) -> Attribute
custom attributes =
    Attribute
        (\config ->
            { config
                | customAttributes = List.append config.customAttributes attributes
            }
        )


{-| -}
nriDescription : String -> Attribute
nriDescription description =
    custom [ ExtraAttributes.nriDescription description ]


{-| -}
testId : String -> Attribute
testId id_ =
    custom [ ExtraAttributes.testId id_ ]


{-| -}
id : String -> Attribute
id id_ =
    custom [ Attributes.id id_ ]



-- INTERNALS


type alias Config =
    { position : Position
    , theme : Theme
    , css : List Css.Style
    , customAttributes : List (Html.Attribute Never)
    }


{-| Default configuration
-}
defaultConfig : Config
defaultConfig =
    { position = NoArrow
    , theme = Green
    , css = [ Css.padding (Css.px 20) ]
    , customAttributes = []
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


view : Config -> Html msg -> Html msg
view config content =
    container config.position
        config.customAttributes
        [ viewBalloon config.theme config.css [ content ]
        , case config.position of
            NoArrow ->
                Html.text ""

            _ ->
                viewArrow config.position config.theme 16
        ]


container : Position -> List (Html.Attribute Never) -> List (Html msg) -> Html msg
container position attributes =
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
        (List.map (Attributes.map never) attributes)


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
    List.foldl (\(Attribute f) a -> f a) defaultConfig customizations
