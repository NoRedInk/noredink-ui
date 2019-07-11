module Nri.Ui.Heading.V1 exposing
    ( Heading, heading
    , VisualLevel(..), withVisualLevel
    , DocumentLevel(..), withDocumentLevel
    , view
    )

{-| Headings such as you'd find in Nri.Ui.Text.V3, but customization options for
accessibility.

@docs Heading, heading

@docs VisualLevel, withVisualLevel

@docs DocumentLevel, withDocumentLevel

@docs view

-}

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Fonts.V1 as Fonts


{-| a custom heading
-}
type Heading msg
    = Heading (List (Html msg)) VisualLevel DocumentLevel


heading : List (Html msg) -> Heading msg
heading content =
    Heading content Top H1



-- VISUAL LEVEL


{-| Customize the heading level for visual reasons.
-}
type VisualLevel
    = Top
    | Tagline
    | Subhead
    | Small


withVisualLevel : VisualLevel -> Heading msg -> Heading msg
withVisualLevel visualLevel (Heading content _ documentLevel) =
    Heading content visualLevel documentLevel


getStyles : VisualLevel -> Style
getStyles visualLevel =
    case visualLevel of
        Top ->
            headingStyles
                { font = Fonts.baseFont
                , color = navy
                , size = 30
                , lineHeight = 38
                , weight = 700
                }

        Tagline ->
            headingStyles
                { font = Fonts.baseFont
                , color = gray45
                , size = 20
                , lineHeight = 30
                , weight = 400
                }

        Subhead ->
            headingStyles
                { font = Fonts.baseFont
                , color = navy
                , size = 20
                , lineHeight = 26
                , weight = 700
                }

        Small ->
            Css.batch
                [ headingStyles
                    { font = Fonts.baseFont
                    , color = gray20
                    , size = 16
                    , lineHeight = 21
                    , weight = 700
                    }
                , letterSpacing (px -0.13)
                ]


headingStyles :
    { color : Color
    , font : Style
    , lineHeight : Float
    , size : Float
    , weight : Int
    }
    -> Style
headingStyles config =
    Css.batch
        [ config.font
        , fontSize (px config.size)
        , color config.color
        , lineHeight (px config.lineHeight)
        , fontWeight (int config.weight)
        , padding zero
        , textAlign left
        , margin zero
        ]



-- DOCUMENT LEVEL


{-| Customize the heading level for accessibility reasons.
-}
type DocumentLevel
    = H1
    | H2
    | H3
    | H4
    | H5
    | H6


withDocumentLevel : DocumentLevel -> Heading msg -> Heading msg
withDocumentLevel documentLevel (Heading content visualLevel _) =
    Heading content visualLevel documentLevel


getTag : DocumentLevel -> (List (Attribute msg) -> List (Html msg) -> Html msg)
getTag documentLevel =
    case documentLevel of
        H1 ->
            h1

        H2 ->
            h2

        H3 ->
            h3

        H4 ->
            h4

        H5 ->
            h5

        H6 ->
            h6



-- VIEW


view : Heading msg -> Html msg
view (Heading content visualLevel documentLevel) =
    getTag documentLevel
        [ css [ getStyles visualLevel ] ]
        content
