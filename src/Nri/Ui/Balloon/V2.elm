module Nri.Ui.Balloon.V2 exposing
    ( view, Attribute
    , plaintext, markdown, html
    , green, purple, orange, white, navy, customTheme
    , onBottom, onLeft, onRight, onTop
    , custom, id, nriDescription, testId
    , containerCss
    , css, notMobileCss, mobileCss, quizEngineMobileCss
    )

{-| Adding a tooltip? Use `Nri.Ui.Tooltip`, not Balloon.
Balloon is really just a container: it is non-interactive and isn't semantically meaningful.

    Balloon.view
        [ Balloon.plaintext "Hello!"
        , Balloon.onTop
        , Balloon.navy
        ]


## Changelog

Changes from V1:

  - allow for far more customization:
      - background color
      - HTML attributes
  - change the API to be more similar to other NRI components
      - add the standard attributes, like nriDescription and css
      - `balloon` -> `view`
      - content -> `markdown`, `plaintext`, and `html`


## API

@docs view, Attribute


### Content

@docs plaintext, markdown, html


### Customizations for Balloon

@docs green, purple, orange, white, navy, customTheme
@docs onBottom, onLeft, onRight, onTop
@docs custom, id, nriDescription, testId


### CSS

@docs containerCss
@docs css, notMobileCss, mobileCss, quizEngineMobileCss

-}

import Content
import Css exposing (..)
import Css.Media exposing (MediaQuery)
import Html.Styled as Html exposing (Html, div, styled)
import Html.Styled.Attributes as Attributes
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Shadows.V1 as Shadows



-- TOOLTIP VIEWS


{-| Green balloon without an arrow by default.

     __________
    |         |
    |_________|

-}
view : List (Attribute msg) -> Html msg
view customizations =
    view_ (customizationsToConfig customizations)


{-| Balloon's attributes.
-}
type Attribute msg
    = Attribute (Config msg -> Config msg)


setPosition : Position -> Attribute msg
setPosition position =
    Attribute (\config -> { config | position = position })


setTheme : Theme -> Attribute msg
setTheme theme =
    Attribute (\config -> { config | theme = theme })


{-| Balloon with the arrow on the bottom.

     __________
    |         |
    |___  ____|
        \/

-}
onTop : Attribute msg
onTop =
    setPosition OnTop


{-| Balloon with the arrow on the left.

      __________
     |         |
    <          |
     |_________|

-}
onRight : Attribute msg
onRight =
    setPosition OnRight


{-| Balloon with the arrow on the top.

     ___/\_____
    |         |
    |_________|

-}
onBottom : Attribute msg
onBottom =
    setPosition OnBottom


{-| Balloon with the arrow on the right.

      __________
     |         |
     |          >
     |_________|

-}
onLeft : Attribute msg
onLeft =
    setPosition OnLeft


{-| Green theme (This is the default theme.)
-}
green : Attribute msg
green =
    setTheme defaultGreenTheme


{-| Orange theme
-}
orange : Attribute msg
orange =
    setTheme { backgroundColor = Colors.sunshine, color = Colors.gray20 }


{-| Purple theme
-}
purple : Attribute msg
purple =
    setTheme { backgroundColor = Colors.purple, color = Colors.white }


{-| White theme
-}
white : Attribute msg
white =
    setTheme { backgroundColor = Colors.white, color = Colors.gray20 }


{-| Navy theme
-}
navy : Attribute msg
navy =
    setTheme { backgroundColor = Colors.navy, color = Colors.white }


{-| Custom theme: set the background & text color.
-}
customTheme : { backgroundColor : Css.Color, color : Css.Color } -> Attribute msg
customTheme =
    setTheme


{-| -}
containerCss : List Style -> Attribute msg
containerCss styles =
    Attribute (\config -> { config | containerCss = List.append config.containerCss styles })


{-| -}
css : List Style -> Attribute msg
css styles =
    Attribute (\config -> { config | css = List.append config.css styles })


{-| Equivalent to:

    ClickableText.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.notMobile ] styles ]

-}
notMobileCss : List Style -> Attribute msg
notMobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.notMobile ] styles ]


{-| Equivalent to:

    ClickableText.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.mobile ] styles ]

-}
mobileCss : List Style -> Attribute msg
mobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.mobile ] styles ]


{-| Equivalent to:

    ClickableText.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.quizEngineMobile ] styles ]

-}
quizEngineMobileCss : List Style -> Attribute msg
quizEngineMobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.quizEngineMobile ] styles ]


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way you want/expect if underlying Balloon styles change.
Instead, please use the `css` helper.

-}
custom : List (Html.Attribute msg) -> Attribute msg
custom attributes =
    Attribute
        (\config ->
            { config
                | customAttributes = List.append config.customAttributes attributes
            }
        )


{-| -}
nriDescription : String -> Attribute msg
nriDescription description =
    custom [ ExtraAttributes.nriDescription description ]


{-| -}
testId : String -> Attribute msg
testId id_ =
    custom [ ExtraAttributes.testId id_ ]


{-| -}
id : String -> Attribute msg
id id_ =
    custom [ Attributes.id id_ ]


{-| Provide a plain-text string.
-}
plaintext : String -> Attribute msg
plaintext =
    Attribute << Content.plaintext


{-| Provide a string that will be rendered as markdown.
-}
markdown : String -> Attribute msg
markdown =
    Attribute << Content.markdown


{-| Provide a list of custom HTML.
-}
html : List (Html msg) -> Attribute msg
html =
    Attribute << Content.html



-- INTERNALS


type alias Config msg =
    { position : Position
    , theme : Theme
    , containerCss : List Css.Style
    , css : List Css.Style
    , customAttributes : List (Html.Attribute msg)
    , content : List (Html msg)
    }


{-| Default configuration
-}
defaultConfig : Config msg
defaultConfig =
    { position = NoArrow
    , theme = defaultGreenTheme
    , containerCss = []
    , css = [ Css.padding (Css.px 20) ]
    , customAttributes = []
    , content = []
    }


{-| The Arrow may be positioned on any edge of the balloon, facing outward.
-}
type Position
    = OnTop
    | OnRight
    | OnBottom
    | OnLeft
    | NoArrow


type alias Theme =
    { backgroundColor : Css.Color
    , color : Css.Color
    }


defaultGreenTheme : Theme
defaultGreenTheme =
    { backgroundColor = Colors.greenDarkest
    , color = Colors.white
    }


view_ : Config msg -> Html msg
view_ config =
    container config.position
        (Attributes.css config.containerCss :: config.customAttributes)
        [ viewBalloon config.theme config.css config.content
        , case config.position of
            NoArrow ->
                Html.text ""

            _ ->
                viewArrow config.position config.theme 16
        ]


container : Position -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
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
        attributes


viewBalloon : Theme -> List Css.Style -> List (Html msg) -> Html msg
viewBalloon palette styles contents =
    styled div
        [ display inlineBlock
        , lineHeight (num 1.4)
        , textAlign left
        , position relative
        , Css.borderRadius (px 8)
        , Shadows.high
        , backgroundColor palette.backgroundColor
        , border3 (px 1) solid palette.backgroundColor
        , color palette.color
        , Fonts.baseFont
        , fontSize (px 15)
        , Css.batch styles
        ]
        []
        contents


viewArrow : Position -> Theme -> Float -> Html msg
viewArrow position palette diagonal =
    let
        arrowSideWidth =
            sqrt ((diagonal ^ 2) / 2)
    in
    styled div
        [ arrowPosition position
        , backgroundColor palette.backgroundColor
        , border3 (px 1) solid palette.backgroundColor
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


customizationsToConfig : List (Attribute msg) -> Config msg
customizationsToConfig customizations =
    List.foldl (\(Attribute f) a -> f a) defaultConfig customizations
