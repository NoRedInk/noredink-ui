module Nri.Ui.Balloon.V2 exposing
    ( view, Attribute
    , paragraph, plaintext, markdown, html
    , green, purple, orange, white, navy, customTheme
    , highContrastModeTheme
    , onBottom, onLeft, onRight, onTop
    , alignArrowStart, alignArrowMiddle, alignArrowEnd
    , arrowHeight
    , custom, id, contentId, nriDescription, testId
    , containerCss
    , css, notMobileCss, mobileCss, quizEngineMobileCss
    )

{-| Adding a tooltip? Use `Nri.Ui.Tooltip`, not Balloon.
Balloon is really just a container: it is non-interactive and isn't semantically meaningful.

    Balloon.view
        [ Balloon.paragraph "Hello, world! I'm a balloon!"
        , Balloon.onTop
        , Balloon.navy
        ]


## Changelog

Patch changes:

  - adds paragraph

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

@docs paragraph, plaintext, markdown, html


### Customizations for Balloon

@docs green, purple, orange, white, navy, customTheme
@docs highContrastModeTheme
@docs onBottom, onLeft, onRight, onTop
@docs alignArrowStart, alignArrowMiddle, alignArrowEnd
@docs arrowHeight
@docs custom, id, contentId, nriDescription, testId


### CSS

@docs containerCss
@docs css, notMobileCss, mobileCss, quizEngineMobileCss

-}

import Content
import Css exposing (..)
import Css.Media
import Html.Styled as Html exposing (Html, div, styled)
import Html.Styled.Attributes as Attributes
import MarkdownStyles
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


{-| Where should the arrow be positioned relative to the balloon?
-}
type ArrowAlignment
    = Start
    | Middle
    | End


withArrowAlignment : ArrowAlignment -> Attribute msg
withArrowAlignment alignment =
    Attribute (\config -> { config | arrowAlignment = alignment })


{-| Put the arrow at the "start" of the ballon.
For onTop & onBottom ballons, this means "left".
For onLeft & onRight ballon, this means "top".

     __________
    |_  ______|
      \/

-}
alignArrowStart : Attribute msg
alignArrowStart =
    withArrowAlignment Start


{-| Put the arrow at the "middle" of the ballon. This is the default behavior.

     __________
    |___  ____|
        \/

-}
alignArrowMiddle : Attribute msg
alignArrowMiddle =
    withArrowAlignment Middle


{-| Put the arrow at the "end" of the ballon.
For onTop & onBottom ballons, this means "right".
For onLeft & onRight ballon, this means "bottom".

     __________
    |______  _|
           \/

-}
alignArrowEnd : Attribute msg
alignArrowEnd =
    withArrowAlignment End


{-| Set how tall you want the arrow to be. The default is 8px.
-}
arrowHeight : Float -> Attribute msg
arrowHeight height =
    Attribute (\config -> { config | arrowHeight = height })


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


{-| Typically, you will want to use [system colors](https://developer.mozilla.org/en-US/docs/Web/CSS/system-color) for these colors (which is why the API uses strings):
-}
highContrastModeTheme : { backgroundColor : String, color : String } -> Attribute msg
highContrastModeTheme theme =
    Attribute (\config -> { config | highContrastModeTheme = Just theme })


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


{-| -}
contentId : String -> Attribute msg
contentId id_ =
    Attribute (\config -> { config | contentId = Just id_ })


{-| Provide a plain-text string.
-}
plaintext : String -> Attribute msg
plaintext =
    Attribute << Content.plaintext


{-| Provide a plain-text string that will be put into a paragraph tag, with the default margin removed.
-}
paragraph : String -> Attribute msg
paragraph =
    Attribute << Content.paragraph


{-| Provide a string that will be rendered as markdown.
-}
markdown : String -> Attribute msg
markdown content =
    Attribute <|
        \config ->
            { config
                | content = Content.markdownContent content
                , css = MarkdownStyles.anchorAndButton ++ config.css
            }


{-| Provide a list of custom HTML.
-}
html : List (Html msg) -> Attribute msg
html =
    Attribute << Content.html



-- INTERNALS


type alias Config msg =
    { position : Position
    , arrowAlignment : ArrowAlignment
    , arrowHeight : Float
    , theme : Theme
    , highContrastModeTheme : Maybe HighContrastModeTheme
    , containerCss : List Css.Style
    , contentId : Maybe String
    , css : List Css.Style
    , customAttributes : List (Html.Attribute msg)
    , content : List (Html msg)
    }


{-| Default configuration
-}
defaultConfig : Config msg
defaultConfig =
    { position = NoArrow
    , arrowAlignment = Middle
    , arrowHeight = 8
    , theme = defaultGreenTheme
    , highContrastModeTheme = Nothing
    , containerCss = []
    , contentId = Nothing
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


type alias HighContrastModeTheme =
    { backgroundColor : String
    , color : String
    }


applyHighContrastModeTheme : Maybe HighContrastModeTheme -> Css.Style
applyHighContrastModeTheme maybeHighContrastModeTheme =
    case maybeHighContrastModeTheme of
        Just highContrastPalette ->
            MediaQuery.highContrastMode
                [ Css.property "background-color" highContrastPalette.backgroundColor
                , Css.property "color" highContrastPalette.color
                , Css.property "border-color" highContrastPalette.backgroundColor
                , Css.property "forced-color-adjust" "none"
                ]

        Nothing ->
            Css.batch []


view_ : Config msg -> Html msg
view_ config =
    container config.position
        (Attributes.css config.containerCss :: config.customAttributes)
        [ viewBalloon config
        , case config.position of
            NoArrow ->
                Html.text ""

            _ ->
                viewArrow config
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


viewBalloon :
    { config
        | theme : Theme
        , highContrastModeTheme : Maybe HighContrastModeTheme
        , contentId : Maybe String
        , css : List Css.Style
        , content : List (Html msg)
    }
    -> Html msg
viewBalloon config =
    styled div
        [ display inlineBlock
        , lineHeight (num 1.4)
        , textAlign left
        , position relative
        , Css.borderRadius (px borderRounding)
        , Shadows.high
        , backgroundColor config.theme.backgroundColor
        , border3 (px 1) solid config.theme.backgroundColor
        , color config.theme.color
        , Fonts.baseFont
        , fontSize (px 15)
        , applyHighContrastModeTheme config.highContrastModeTheme
        , Css.batch config.css
        ]
        (case config.contentId of
            Nothing ->
                []

            Just id_ ->
                [ Attributes.id id_ ]
        )
        config.content


borderRounding : Float
borderRounding =
    8


arrowWidth : Float
arrowWidth =
    8


viewArrow : Config msg -> Html msg
viewArrow config =
    styled div
        [ arrowPosition config
        , borderStyle solid
        , applyHighContrastModeTheme config.highContrastModeTheme
        , Css.height zero
        , Css.width zero
        , Css.flexShrink (Css.num 0)
        ]
        []
        []


arrowPosition : Config msg -> Css.Style
arrowPosition ({ position, arrowAlignment, theme } as config) =
    let
        arrowHeight_ =
            config.arrowHeight

        color =
            theme.backgroundColor

        highContrastColor =
            Maybe.withDefault "CanvasText"
                (Maybe.map .backgroundColor config.highContrastModeTheme)

        offset =
            String.fromFloat (borderRounding + 8) ++ "px"

        translate direction =
            Css.batch <|
                case arrowAlignment of
                    Start ->
                        [ Css.alignSelf Css.flexStart
                        , Css.property "transform" ("translate" ++ direction ++ "(" ++ offset ++ ")")
                        ]

                    Middle ->
                        [ Css.property "transform" "scale(1)" ]

                    End ->
                        [ Css.alignSelf Css.flexEnd
                        , Css.property "transform" ("translate" ++ direction ++ "(-" ++ offset ++ ")")
                        ]
    in
    case position of
        OnTop ->
            batch
                [ translate "X"
                , Css.property "transform-origin" "center"
                , Css.borderTop (Css.px arrowHeight_)
                , Css.borderRight (Css.px arrowWidth)
                , Css.borderBottom Css.zero
                , Css.borderLeft (Css.px arrowWidth)

                -- Colors:
                , Css.borderTopColor color
                , Css.borderRightColor transparent
                , Css.borderBottomColor transparent
                , Css.borderLeftColor transparent
                , MediaQuery.highContrastMode
                    [ Css.property "forced-color-adjust" "none"
                    , Css.property "border-top-color" highContrastColor
                    , Css.property "border-right-color" "Canvas"
                    , Css.property "border-bottom-color" "Canvas"
                    , Css.property "border-left-color" "Canvas"
                    ]
                ]

        OnBottom ->
            batch
                [ translate "X"
                , Css.property "transform-origin" "center"
                , Css.borderTop Css.zero
                , Css.borderRight (Css.px arrowWidth)
                , Css.borderBottom (Css.px arrowHeight_)
                , Css.borderLeft (Css.px arrowWidth)

                -- Colors:
                , Css.borderTopColor transparent
                , Css.borderRightColor transparent
                , Css.borderBottomColor color
                , Css.borderLeftColor transparent
                , MediaQuery.highContrastMode
                    [ Css.property "forced-color-adjust" "none"
                    , Css.property "border-top-color" "Canvas"
                    , Css.property "border-right-color" "Canvas"
                    , Css.property "border-bottom-color" highContrastColor
                    , Css.property "border-left-color" "Canvas"
                    ]
                ]

        OnLeft ->
            batch
                [ translate "Y"
                , Css.property "transform-origin" "center"
                , Css.borderTop (Css.px arrowWidth)
                , Css.borderRight Css.zero
                , Css.borderBottom (Css.px arrowWidth)
                , Css.borderLeft (Css.px arrowHeight_)

                -- Colors:
                , Css.borderTopColor transparent
                , Css.borderRightColor transparent
                , Css.borderBottomColor transparent
                , Css.borderLeftColor color
                , MediaQuery.highContrastMode
                    [ Css.property "forced-color-adjust" "none"
                    , Css.property "border-top-color" "Canvas"
                    , Css.property "border-right-color" "Canvas"
                    , Css.property "border-bottom-color" "Canvas"
                    , Css.property "border-left-color" highContrastColor
                    ]
                ]

        OnRight ->
            batch
                [ translate "Y"
                , Css.property "transform-origin" "center"
                , Css.borderTop (Css.px arrowWidth)
                , Css.borderRight (Css.px arrowHeight_)
                , Css.borderBottom (Css.px arrowWidth)
                , Css.borderLeft Css.zero

                -- Colors:
                , Css.borderTopColor transparent
                , Css.borderRightColor color
                , Css.borderBottomColor transparent
                , Css.borderLeftColor transparent
                , MediaQuery.highContrastMode
                    [ Css.property "forced-color-adjust" "none"
                    , Css.property "border-top-color" "Canvas"
                    , Css.property "border-right-color" highContrastColor
                    , Css.property "border-bottom-color" "Canvas"
                    , Css.property "border-left-color" "Canvas"
                    ]
                ]

        NoArrow ->
            display none


customizationsToConfig : List (Attribute msg) -> Config msg
customizationsToConfig customizations =
    List.foldl (\(Attribute f) a -> f a) defaultConfig customizations
