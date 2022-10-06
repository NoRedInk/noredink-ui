module Nri.Ui.Panel.V1 exposing
    ( view
    , Customization, primaryTheme, secondaryTheme
    )

{-| Create panels (AKA wells.)


## View

`view` works mostly like the functions from `Html`, but you can only customize
it with things that make sense for panels. To get a really basic panel:

    view []
        "Header Text"
        [ text "Body Content" ]

@docs view


## Customizations

    view
        [ secondaryTheme ]
        "Header text"
        [ text "Body Content" ]

@docs Customization, primaryTheme, secondaryTheme

You can also add extra stuff to the right side of the panel header. This is a
customization, despite being content. See the examples in the style guide for
how this works out.

-}

import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts


type alias Panel msg =
    { header : String
    , headerExtras : List (Html msg)
    , contents : List (Html msg)
    , theme : Theme
    }


init : String -> List (Html msg) -> Panel msg
init header contents =
    { header = header
    , headerExtras = []
    , contents = contents
    , theme = Primary
    }


{-| -}
type Customization
    = WithTheme Theme


type Theme
    = Primary
    | Secondary


{-| use the primary color theme (this is the default)
-}
primaryTheme : Customization
primaryTheme =
    WithTheme Primary


{-| use the secondary color theme
-}
secondaryTheme : Customization
secondaryTheme =
    WithTheme Secondary


customize : Customization -> Panel msg -> Panel msg
customize customization panel =
    case customization of
        WithTheme theme ->
            { panel | theme = theme }



-- views


{-| create a panel, given the options you specify
-}
view : List Customization -> String -> List (Html msg) -> Html msg
view customizations header contents =
    let
        panel =
            List.foldl customize (init header contents) customizations
    in
    section []
        [ Html.Styled.header
            [ css
                [ case panel.theme of
                    Primary ->
                        Css.backgroundColor Colors.navy

                    Secondary ->
                        Css.backgroundColor Colors.gray75
                , Css.padding2 (Css.px 8) (Css.px 15)
                , Fonts.baseFont
                , Css.fontSize (Css.px 16)
                , Css.color Colors.white
                , Css.borderTopLeftRadius (Css.px 8)
                , Css.borderTopRightRadius (Css.px 8)
                , Css.displayFlex
                , Css.alignItems Css.center
                , Css.flexWrap Css.wrap
                ]
            ]
            (text panel.header :: panel.headerExtras)
        , article
            [ css
                [ Css.padding2 (Css.px 8) (Css.px 15)
                , Fonts.baseFont
                , Css.fontSize (Css.px 16)
                , Css.color Colors.gray20
                , Css.backgroundColor Colors.white
                , Css.borderBottomLeftRadius (Css.px 8)
                , Css.borderBottomRightRadius (Css.px 8)
                , Css.overflowWrap Css.breakWord
                , Css.property "word-wrap" "break-word"
                ]
            ]
            panel.contents
        ]
