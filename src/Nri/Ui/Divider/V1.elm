module Nri.Ui.Divider.V1 exposing (styles, view)

{-| <https://staging.noredink.com/style_guide#ui/src/Nri/Divider.elm>

@docs styles, view

-}

import Css exposing (..)
import Html exposing (..)
import Nri.Colors as Colors
import Nri.Ui.Styles.V1


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
styles : Nri.Ui.Styles.V1.Styles Never CssClasses msg
styles =
    Nri.Ui.Styles.V1.styles "Nri-Ui-Divider-"
        [ Css.class Container
            [ Css.width (pct 100)
            , Css.displayFlex
            , Css.alignItems Css.center
            ]
        , Css.class LeftLine
            [ Css.width (px 10)
            , Css.height (px 1)
            , backgroundColor Colors.gray75
            , marginTop (px 2)
            ]
        , Css.class RightLine
            [ Css.flexGrow (Css.int 1)
            , backgroundColor Colors.gray75
            , Css.height (px 1)
            , marginTop (px 2)
            ]
        , Css.class Title
            [ margin2 zero (px 5)
            , fontSize (px 12)
            , color Colors.gray45
            ]
        ]
