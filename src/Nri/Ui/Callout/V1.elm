module Nri.Ui.Callout.V1 exposing
    ( Attribute, callout
    , sideText
    , containerCss, contentCss
    , custom
    )

{-|

@docs Attribute, callout

@docs sideText

@docs containerCss, contentCss

@docs custom

-}

import Css
import Css.Global
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts


type Attribute msg
    = SideText (Maybe (Html msg))
    | ContentCss (List Css.Style)
    | ContainerCss (List Css.Style)
    | Custom (Html.Attribute msg)


sideText : Html msg -> Attribute msg
sideText =
    SideText << Just


containerCss : List Css.Style -> Attribute msg
containerCss =
    ContainerCss


contentCss : List Css.Style -> Attribute msg
contentCss =
    ContentCss


custom : Html.Attribute msg -> Attribute msg
custom =
    Custom


type alias Attrs msg =
    { sideText : Maybe (Html msg)
    , containerCss : List Css.Style
    , contentCss : List Css.Style
    , customAttrs : List (Html.Attribute msg)
    }


defaultAttrs : Attrs msg
defaultAttrs =
    { sideText = Nothing
    , containerCss = []
    , contentCss = []
    , customAttrs = []
    }


customize : Attribute msg -> Attrs msg -> Attrs msg
customize attr attrs =
    case attr of
        SideText text ->
            { attrs | sideText = text }

        ContentCss css ->
            { attrs | contentCss = css }

        ContainerCss css ->
            { attrs | containerCss = css }

        Custom custom_ ->
            { attrs | customAttrs = custom_ :: attrs.customAttrs }


callout : List (Attribute msg) -> List (Html msg) -> Html msg
callout attrs children =
    let
        finalAttrs =
            List.foldl customize defaultAttrs attrs
    in
    Html.aside
        (css
            ([ Css.boxSizing Css.borderBox
             , Css.backgroundColor Colors.sunshine
             , Css.displayFlex
             , Css.flexDirection Css.row
             , Css.minHeight (Css.px 42)
             , Css.alignItems Css.stretch
             , Css.border3 (Css.px 1) Css.solid Colors.highlightYellow
             , Css.borderRadius (Css.px 4)
             ]
                ++ finalAttrs.containerCss
            )
            :: finalAttrs.customAttrs
        )
        [ case finalAttrs.sideText of
            Just text ->
                Html.div
                    [ css
                        [ -- position
                          Css.backgroundColor Colors.highlightYellow
                        , Css.color Colors.highlightYellowDark
                        , Css.padding2 Css.zero (Css.px 20)
                        , Css.displayFlex
                        , Css.alignItems Css.center

                        -- text
                        , Fonts.baseFont
                        , Css.fontWeight Css.bold
                        , Css.fontSize (Css.px 12)
                        ]
                    ]
                    [ text ]

            Nothing ->
                Html.text ""
        , Html.div
            [ css
                ([ -- position
                   Css.padding2 (Css.px 6) (Css.px 14)
                 , Css.displayFlex
                 , Css.alignItems Css.center

                 -- text
                 , Css.fontSize (Css.px 12)
                 , Css.color Colors.gray20
                 , Fonts.baseFont

                 -- children
                 , Css.Global.descendants
                    [ Css.Global.a
                        [ Css.color Colors.azure
                        , Css.textDecoration Css.none
                        ]
                    ]
                 ]
                    ++ finalAttrs.contentCss
                )
            ]
            [ Html.p [ css [ Css.margin Css.zero ] ] children ]
        ]
