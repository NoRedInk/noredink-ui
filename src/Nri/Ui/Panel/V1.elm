module Nri.Ui.Panel.V1 exposing
    ( view, Attribute
    , header
    , paragraph, plaintext, markdown, html
    , containerCss, headerCss, css
    , primary, secondary
    )

{-| Create panels (AKA wells.)


## Changelog


### Patch changes

  - use internal `Content` module
  - adds paragraph

@docs view, Attribute


## Content

@docs header
@docs paragraph, plaintext, markdown, html
@docs containerCss, headerCss, css


## Theme

@docs primary, secondary

-}

import Content
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes
import MarkdownStyles
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts


type alias Config msg =
    { header : String
    , content : List (Html msg)
    , theme : Theme
    , css : List Style
    , headerCss : List Style
    , containerCss : List Style
    }


defaultConfig : Config msg
defaultConfig =
    { header = ""
    , content = []
    , theme = Primary
    , css = []
    , headerCss = []
    , containerCss = []
    }


{-| -}
type Attribute msg
    = Attribute (Config msg -> Config msg)


type Theme
    = Primary
    | Secondary


{-| use the primary color theme (this is the default)
-}
primary : Attribute msg
primary =
    Attribute (\soFar -> { soFar | theme = Primary })


{-| use the secondary color theme
-}
secondary : Attribute msg
secondary =
    Attribute (\soFar -> { soFar | theme = Secondary })


{-| The main text for the panel's header
-}
header : String -> Attribute msg
header header_ =
    Attribute (\soFar -> { soFar | header = header_ })


{-| Render panel content.
-}
html : List (Html msg) -> Attribute msg
html =
    Attribute << Content.html


{-| Use a plain-text string for the panel content.
-}
plaintext : String -> Attribute msg
plaintext =
    Attribute << Content.plaintext


{-| Provide a plain-text string that will be put into a paragraph tag, with the default margin removed.
-}
paragraph : String -> Attribute msg
paragraph =
    Attribute << Content.paragraph


{-| Use a markdown string for the panel content.
-}
markdown : String -> Attribute msg
markdown content =
    Attribute <|
        \config ->
            { config
                | content = Content.markdownContent content
                , css = config.css ++ MarkdownStyles.anchorAndButton
            }


{-| -}
containerCss : List Style -> Attribute msg
containerCss styles =
    Attribute <| \config -> { config | containerCss = styles }


{-| -}
headerCss : List Style -> Attribute msg
headerCss styles =
    Attribute <| \config -> { config | headerCss = styles }


{-| -}
css : List Style -> Attribute msg
css styles =
    Attribute <| \config -> { config | css = styles }



-- views


{-| create a panel, given the options you specify
-}
view : List (Attribute msg) -> Html msg
view customizations =
    let
        panel =
            List.foldl (\(Attribute f) -> f) defaultConfig customizations
    in
    section [ Attributes.css panel.containerCss ]
        [ Html.Styled.header
            [ Attributes.css
                [ case panel.theme of
                    Primary ->
                        Css.backgroundColor Colors.navy

                    Secondary ->
                        Css.backgroundColor Colors.gray45
                , Css.padding2 (Css.px 8) (Css.px 15)
                , Fonts.baseFont
                , Css.fontSize (Css.px 16)
                , Css.color Colors.white
                , Css.borderTopLeftRadius (Css.px 8)
                , Css.borderTopRightRadius (Css.px 8)
                , Css.displayFlex
                , Css.alignItems Css.center
                , Css.flexWrap Css.wrap
                , Css.batch panel.headerCss
                ]
            ]
            [ text panel.header ]
        , article
            [ Attributes.css
                [ Css.padding2 (Css.px 8) (Css.px 15)
                , Fonts.baseFont
                , Css.fontSize (Css.px 16)
                , Css.color Colors.gray20
                , Css.backgroundColor Colors.white
                , Css.borderBottomLeftRadius (Css.px 8)
                , Css.borderBottomRightRadius (Css.px 8)
                , Css.overflowWrap Css.breakWord
                , Css.property "word-wrap" "break-word"
                , Css.batch panel.css
                ]
            ]
            panel.content
        ]
