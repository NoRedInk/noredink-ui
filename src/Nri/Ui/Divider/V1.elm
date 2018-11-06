module Nri.Ui.Divider.V1 exposing (styles, view)

{-| <https://staging.noredink.com/style_guide#ui/src/Nri/Divider.elm>

@docs styles, view

-}

import Css exposing (..)
import Css.Foreign exposing (Snippet, children, descendants, everything, selector)
import DEPRECATED.Css.File exposing (Stylesheet, compile, stylesheet)
import DEPRECATED.Nri.Ui.Styles.V1
import Html exposing (..)
import Nri.Ui.Colors.V1 as Colors


type alias Config =
    { lineColor : Css.Color
    , textColor : Css.Color
    }


{-| -}
view : String -> Html msg
view text =
    div [ styles.class [ Container ] ]
        [ div [ styles.class [ LeftLine ] ] []
        , div [ styles.class [ Title ] ] [ Html.text text ]
        , div [ styles.class [ RightLine ] ] []
        ]


type CssClasses
    = LeftLine
    | RightLine
    | Title
    | Container


{-| -}
styles : DEPRECATED.Nri.Ui.Styles.V1.Styles Never CssClasses msg
styles =
    DEPRECATED.Nri.Ui.Styles.V1.styles "Nri-Ui-Divider-"
        [ Css.Foreign.class Container
            [ Css.width (pct 100)
            , Css.displayFlex
            , Css.alignItems Css.center
            ]
        , Css.Foreign.class LeftLine
            [ Css.width (px 10)
            , Css.height (px 1)
            , backgroundColor Colors.gray75
            , marginTop (px 2)
            ]
        , Css.Foreign.class RightLine
            [ Css.flexGrow (Css.int 1)
            , backgroundColor Colors.gray75
            , Css.height (px 1)
            , marginTop (px 2)
            ]
        , Css.Foreign.class Title
            [ margin2 zero (px 5)
            , fontSize (px 12)
            , color Colors.gray45
            ]
        ]
