module Nri.Outline.Styles exposing (..)

import Css exposing (..)
import Nri.Colors as Colors
import Nri.Css.Extra
import Nri.Fonts
import Nri.Palette as Palette exposing (Palette)
import Nri.Styles


type CssClasses
    = Container
    | Row
    | ChildRow
    | RowGray
    | RowDarkGray
    | RowBlue
    | RowDarkBlue
    | RowPurple
    | RowTurquoise
    | RowRed
    | RowGreen
    | RowWhite
    | RowCornflower
    | RowAqua
    | RowPanelTitle
    | RowPanelTitleGray
    | RowPanelTitleDarkGray
    | RowPanelTitleBlue
    | RowPanelTitleDarkBlue
    | RowPanelTitlePurple
    | RowPanelTitleTurquoise
    | RowPanelTitleRed
    | RowPanelTitleGreen
    | RowPanelTitleWhite
    | RowPanelTitleCornflower
    | RowPanelTitleAqua
    | RowPanelContent
    | RowPanelContentGray
    | RowPanelContentCornflower
    | RowPanelContentDarkGray
    | RowPanelContentBlue
    | RowPanelContentDarkBlue
    | RowPanelContentPurple
    | RowPanelContentTurquoise
    | RowPanelContentRed
    | RowPanelContentGreen
    | RowPanelContentWhite
    | RowPanelContentAqua
    | RowPanelContentDashed
    | ChildRowContent
    | ChildRowContentCornflower
    | ChildRowContentGray
    | ChildRowContentDarkGray
    | ChildRowContentBlue
    | ChildRowContentDarkBlue
    | ChildRowContentPurple
    | ChildRowContentTurquoise
    | ChildRowContentRed
    | ChildRowContentGreen
    | ChildRowContentWhite
    | ChildRowContentAqua
    | RowContent
    | RowChildren


styles : Nri.Styles.Styles Never CssClasses msg
styles =
    Nri.Styles.styles "Nri-Outline-"
        [ Css.class Container
            [ listStyle none
            , margin zero
            , padding zero
            ]

        -- rows
        , Css.class Row
            [ paddingBottom (px 16)
            , position relative
            , lastChild [ paddingBottom zero ]
            ]
        , Css.class ChildRow
            [ after
                [ property "content" "''"
                , position absolute
                , top (px -16)
                , left (px -14)
                , width (px 14)
                , borderLeft3 (px 1) solid (Palette.gray |> .border)
                , property "height" "calc(100% + 16px)"
                ]
            , lastChild
                [ after [ borderLeftWidth zero ] ]
            , onlyChild
                {- Fixes border styles if there is only one child row -}
                [ after
                    [ property "content" "''"
                    , position absolute
                    , top (px -16)
                    , left (px -14)
                    , width (px 14)
                    , borderLeftWidth (px 1)
                    , borderLeftStyle solid
                    , property "height" "16px"
                    ]
                ]
            ]
        , Css.class RowGray
            [ rowTheme Palette.gray ]
        , Css.class RowDarkGray
            [ rowTheme Palette.darkGray ]
        , Css.class RowBlue
            [ rowTheme Palette.blue ]
        , Css.class RowDarkBlue
            [ rowTheme Palette.darkBlue ]
        , Css.class RowPurple
            [ rowTheme Palette.purple ]
        , Css.class RowTurquoise
            [ rowTheme Palette.turquoise ]
        , Css.class RowRed
            [ rowTheme Palette.red ]
        , Css.class RowGreen
            [ rowTheme Palette.green ]
        , Css.class RowWhite
            [ rowTheme Palette.white ]
        , Css.class RowCornflower
            [ rowTheme Palette.cornflower ]
        , Css.class RowAqua
            [ rowTheme Palette.aqua ]

        -- row panel titles
        , Css.class RowPanelTitle
            [ Nri.Fonts.baseFont
            , borderRadius (px 16)
            , color Colors.white
            , display inlineBlock
            , fontSize (px 12)
            , height (px 16)
            , left (px -4)
            , lineHeight (px 16)
            , padding2 zero (px 7)
            , position absolute
            , top (px -8)
            , property "z-index" "2"
            ]
        , Css.class RowPanelTitleGray
            [ rowPanelTitleTheme Palette.gray ]
        , Css.class RowPanelTitleDarkGray
            [ rowPanelTitleTheme Palette.darkGray ]
        , Css.class RowPanelTitleBlue
            [ rowPanelTitleTheme Palette.blue ]
        , Css.class RowPanelTitleDarkBlue
            [ rowPanelTitleTheme Palette.darkBlue ]
        , Css.class RowPanelTitlePurple
            [ rowPanelTitleTheme Palette.purple ]
        , Css.class RowPanelTitleTurquoise
            [ rowPanelTitleTheme Palette.turquoise ]
        , Css.class RowPanelTitleRed
            [ rowPanelTitleTheme Palette.red ]
        , Css.class RowPanelTitleGreen
            [ rowPanelTitleTheme Palette.green ]
        , Css.class RowPanelTitleWhite
            [ rowPanelTitleTheme Palette.white ]
        , Css.class RowPanelTitleCornflower
            [ rowPanelTitleTheme Palette.cornflower ]
        , Css.class RowPanelTitleAqua
            [ rowPanelTitleTheme Palette.aqua ]
        , Css.class RowPanelContent
            [ borderRadius (px 8)
            , borderWidth (px 1)
            , borderColor (Palette.gray |> .border)
            , borderStyle solid
            , backgroundColor (Palette.gray |> .background)
            , color Colors.gray20
            , fontSize (px 18)
            , Nri.Fonts.quizFont
            , padding (px 13)
            , Nri.Css.Extra.lineHeightNum 1.2
            ]
        , Css.class RowPanelContentGray
            [ rowPanelContentTheme Palette.gray ]
        , Css.class RowPanelContentCornflower
            [ rowPanelContentTheme Palette.cornflower ]
        , Css.class RowPanelContentDarkGray
            [ rowPanelContentTheme Palette.darkGray ]
        , Css.class RowPanelContentBlue
            [ rowPanelContentTheme Palette.blue ]
        , Css.class RowPanelContentDarkBlue
            [ rowPanelContentTheme Palette.darkBlue ]
        , Css.class RowPanelContentPurple
            [ rowPanelContentTheme Palette.purple ]
        , Css.class RowPanelContentTurquoise
            [ rowPanelContentTheme Palette.turquoise ]
        , Css.class RowPanelContentRed
            [ rowPanelContentTheme Palette.red ]
        , Css.class RowPanelContentGreen
            [ rowPanelContentTheme Palette.green ]
        , Css.class RowPanelContentWhite
            [ rowPanelContentTheme Palette.white ]
        , Css.class RowPanelContentAqua
            [ rowPanelContentTheme Palette.aqua ]
        , Css.class RowPanelContentDashed
            [ borderStyle dashed ]
        , Css.class ChildRowContent
            [ after
                [ property "content" "''"
                , height (pct 50)
                , width (px 14)
                , borderBottom3 (px 1) solid (Palette.gray |> .border)
                , borderLeft3 (px 1) solid (Palette.gray |> .border)
                , left (px -14)
                , top zero
                , position absolute
                , maxHeight (px 50)
                ]
            ]
        , Css.class ChildRowContentCornflower
            [ childRowContentTheme Palette.cornflower ]
        , Css.class ChildRowContentGray
            [ childRowContentTheme Palette.gray ]
        , Css.class ChildRowContentDarkGray
            [ childRowContentTheme Palette.darkGray ]
        , Css.class ChildRowContentBlue
            [ childRowContentTheme Palette.blue ]
        , Css.class ChildRowContentDarkBlue
            [ childRowContentTheme Palette.darkBlue ]
        , Css.class ChildRowContentPurple
            [ childRowContentTheme Palette.purple ]
        , Css.class ChildRowContentTurquoise
            [ childRowContentTheme Palette.turquoise ]
        , Css.class ChildRowContentRed
            [ childRowContentTheme Palette.red ]
        , Css.class ChildRowContentGreen
            [ childRowContentTheme Palette.green ]
        , Css.class ChildRowContentWhite
            [ childRowContentTheme Palette.white ]
        , Css.class ChildRowContentAqua
            [ childRowContentTheme Palette.aqua ]
        , Css.class RowContent
            [ position relative ]
        , Css.class RowChildren
            [ paddingLeft (px 29)
            , paddingTop (px 16)
            , listStyleType none
            ]
        ]


rowTheme : Palette -> Style
rowTheme palette =
    batch [ after [ borderColor palette.border ] ]


rowPanelTitleTheme : Palette -> Style
rowPanelTitleTheme palette =
    Css.batch [ backgroundColor palette.border ]


childRowContentTheme : Palette -> Style
childRowContentTheme palette =
    Css.batch [ after [ borderColor palette.border ] ]


rowPanelContentTheme : Palette -> Style
rowPanelContentTheme palette =
    Css.batch
        [ backgroundColor palette.background
        , borderColor palette.border
        , after [ borderColor palette.border ]
        ]
