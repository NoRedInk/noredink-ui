module Nri.Ui.SortableTable.V1 exposing (Direction(..), arrow, arrowActive, arrows, downArrow, sortActive, sortButton, sortHeader, sortInactive, upArrow)

import Css exposing (..)
import Css.File exposing (Stylesheet, UniqueClass, stylesheet, uniqueClass)
import Css.Global exposing (Snippet, adjacentSiblings, children, class, descendants, each, everything, media, selector, withClass)
import Nri.Ui.Colors.V1
import Nri.Ui.CssVendorPrefix.V1 as CssVendorPrefix


sortHeader : UniqueClass
sortHeader =
    uniqueClass
        [ Css.displayFlex
        , Css.alignItems Css.center
        , Css.justifyContent Css.spaceBetween
        , cursor pointer
        , CssVendorPrefix.property "user-select" "none"
        ]


sortButton : UniqueClass
sortButton =
    uniqueClass
        [ padding (px 2)
        , color Nri.Ui.Colors.V1.gray75
        ]


sortInactive : UniqueClass
sortInactive =
    uniqueClass
        [ fontWeight normal ]


sortActive : UniqueClass
sortActive =
    uniqueClass
        [ fontWeight bold ]


arrows : UniqueClass
arrows =
    uniqueClass
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.alignItems Css.center
        , Css.justifyContent Css.center
        ]


type Direction
    = Up
    | Down


arrow : Direction -> UniqueClass
arrow direction =
    let
        result =
            case direction of
                Up ->
                    []

                Down ->
                    [ transform <| rotate (deg 180) ]
    in
    uniqueClass
        [ width (px 8)
        , height (px 6)
        , position relative
        , margin2 (px 1) zero
        , children
            [ selector "svg"
                ([ position absolute
                 , top zero
                 , left zero
                 ]
                    ++ result
                )
            ]
        ]


upArrow : UniqueClass
upArrow =
    arrow Up


downArrow : UniqueClass
downArrow =
    arrow Down


arrowActive : UniqueClass
arrowActive =
    uniqueClass
        [ color Nri.Ui.Colors.V1.azure
        ]
