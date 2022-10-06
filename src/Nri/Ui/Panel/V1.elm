module Nri.Ui.Panel.V1 exposing
    ( view, Attribute
    , header, headerExtras
    , contents
    , primaryTheme, secondaryTheme
    )

{-| Create panels (AKA wells.)

@docs view, Attribute


## Content

@docs header, headerExtras
@docs contents


## Theme

@docs primaryTheme, secondaryTheme

-}

import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts


type alias Config msg =
    { header : String
    , headerExtras : List (Html msg)
    , contents : List (Html msg)
    , theme : Theme
    }


defaultConfig : Config msg
defaultConfig =
    { header = ""
    , headerExtras = []
    , contents = []
    , theme = Primary
    }


{-| -}
type Attribute msg
    = Attribute (Config msg -> Config msg)


type Theme
    = Primary
    | Secondary


{-| use the primary color theme (this is the default)
-}
primaryTheme : Attribute msg
primaryTheme =
    Attribute (\soFar -> { soFar | theme = Primary })


{-| use the secondary color theme
-}
secondaryTheme : Attribute msg
secondaryTheme =
    Attribute (\soFar -> { soFar | theme = Secondary })


{-| The main text for the panel's header
-}
header : String -> Attribute msg
header header_ =
    Attribute (\soFar -> { soFar | header = header_ })


{-| Use this attribute can also add extra stuff to the right side of the panel header.
-}
headerExtras : List (Html msg) -> Attribute msg
headerExtras headerExtras_ =
    Attribute (\soFar -> { soFar | headerExtras = headerExtras_ })


contents : List (Html msg) -> Attribute msg
contents contents_ =
    Attribute (\soFar -> { soFar | contents = contents_ })



-- views


{-| create a panel, given the options you specify
-}
view : List (Attribute msg) -> Html msg
view customizations =
    let
        panel =
            List.foldl (\(Attribute f) -> f) defaultConfig customizations
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
