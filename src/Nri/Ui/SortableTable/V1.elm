module Nri.Ui.SortableTable.V1 exposing (Direction(..), arrow, arrowActive, arrows, downArrow, sortActive, sortButton, sortHeader, sortInactive, upArrow)

import Css exposing (..)
import Css.File exposing (Stylesheet, UniqueClass, stylesheet, uniqueClass)
import Css.Global exposing (Snippet, adjacentSiblings, children, class, descendants, each, everything, media, selector, withClass)
import Css.VendorPrefixed
import FlexBoxWithVendorPrefix as FlexBox
import Nri.Ui.Colors.V1


sortHeader : UniqueClass
sortHeader =
    uniqueClass
        [ FlexBox.displayFlex
        , FlexBox.alignItems FlexBox.center
        , FlexBox.justifyContent FlexBox.spaceBetween
        , cursor pointer
        , Css.VendorPrefixed.property "user-select" "none"
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
        [ FlexBox.displayFlex
        , FlexBox.flexDirection FlexBox.column
        , FlexBox.alignItems FlexBox.center
        , FlexBox.justifyContent FlexBox.center
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
