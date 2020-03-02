module Nri.Ui.CssFlexBoxWithVendorPrefix exposing
    ( displayFlex, displayInlineFlex, flexDirection, justifyContent, alignItems, alignSelf, flexBasis
    , flexGrow, flexShrink, row, rowReverse, column, columnReverse, flexStart, flexEnd, baseline, stretch, center, spaceBetween, spaceAround, flexWrap, nowrap, wrap, wrapReverse
    )

{-|

@docs displayFlex, displayInlineFlex, flexDirection, justifyContent, alignItems, alignSelf, flexBasis
@docs flexGrow, flexShrink, row, rowReverse, column, columnReverse, flexStart, flexEnd, baseline, stretch, center, spaceBetween, spaceAround, flexWrap, nowrap, wrap, wrapReverse

-}

import Css exposing (Style, batch, property)


{-| -}
displayFlex : Style
displayFlex =
    batch
        [ property "display" "-webkit-box" -- OLD - iOS 6-, Safari 3.1-6
        , property "display" "-moz-box" -- OLD - Firefox 19- (buggy but mostly works)
        , property "display" "-ms-flexbox" -- TWEENER - IE 10
        , property "display" "-webkit-flex" -- NEW - Chrome
        , property "display" "flex" -- NEW, Spec - Opera 12.1, Firefox 20+
        ]


{-| -}
displayInlineFlex : Style
displayInlineFlex =
    batch
        [ property "display" "-webkit-inline-box" -- OLD - iOS 6-, Safari 3.1-6
        , property "display" "-moz-inline-box" -- OLD - Firefox 19- (buggy but mostly works)
        , property "display" "-ms-inline-flexbox" -- TWEENER - IE 10
        , property "display" "-webkit-inline-flex" -- NEW - Chrome
        , property "display" "inline-flex" -- NEW, Spec - Opera 12.1, Firefox 20+
        ]


{-| -}
flexDirection : Direction -> Style
flexDirection direction =
    addPrefix "flex-direction" <|
        case direction of
            Row ->
                "row"

            RowReverse ->
                "row-reverse"

            Column ->
                "column"

            ColumnReverse ->
                "column-reverse"


type Direction
    = Row
    | RowReverse
    | Column
    | ColumnReverse


{-| Direction row.
-}
row : Direction
row =
    Row


{-| Direction rowReverse.
-}
rowReverse : Direction
rowReverse =
    RowReverse


{-| Direction column.
-}
column : Direction
column =
    Column


{-| Direction columnReverse.
-}
columnReverse : Direction
columnReverse =
    ColumnReverse


{-| -}
justifyContent : Alignment JustifyContent a -> Style
justifyContent =
    addPrefix "justify-content" << alignmentToString


{-| -}
alignItems : Alignment a AlignItems -> Style
alignItems =
    addPrefix "align-items" << alignmentToString


{-| -}
alignSelf : Alignment a AlignItems -> Style
alignSelf =
    addPrefix "align-self" << alignmentToString


{-| -}
flexBasis : Css.Length compatible units -> Style
flexBasis =
    addPrefix "flex-basis" << .value


{-| -}
flexGrow : Float -> Style
flexGrow value =
    addPrefix "flex-grow" (toString value)


{-| -}
flexShrink : Float -> Style
flexShrink value =
    addPrefix "flex-shrink" (toString value)


{-| -}
flexWrap : Wrap -> Style
flexWrap value =
    addPrefix "flex-wrap" <|
        case value of
            Nowrap ->
                "nowrap"

            Wrap ->
                "wrap"

            WrapReverse ->
                "wrap-reverse"


type Wrap
    = Nowrap
    | Wrap
    | WrapReverse


{-| flex-wrap nowrap
-}
nowrap : Wrap
nowrap =
    Nowrap


{-| flex-wrap wrap
-}
wrap : Wrap
wrap =
    Wrap


{-| flex-wrap wrapReverse
-}
wrapReverse : Wrap
wrapReverse =
    WrapReverse


type Alignment justify align
    = FlexStart justify align
    | FlexEnd justify align
    | Center justify align
    | SpaceBetween justify
    | SpaceAround justify
    | Baseline align
    | Stretch align


alignmentToString : Alignment a b -> String
alignmentToString value =
    case value of
        FlexStart _ _ ->
            "flex-start"

        FlexEnd _ _ ->
            "flex-end"

        Center _ _ ->
            "center"

        SpaceBetween _ ->
            "space-between"

        SpaceAround _ ->
            "space-around"

        Baseline _ ->
            "baseline"

        Stretch _ ->
            "stretch"


type JustifyContent
    = JustifyContent


type AlignItems
    = AlignItems


{-| align-items/justify-content flexStart
-}
flexStart : Alignment JustifyContent AlignItems
flexStart =
    FlexStart JustifyContent AlignItems


{-| align-items/justify-content flexEnd
-}
flexEnd : Alignment JustifyContent AlignItems
flexEnd =
    FlexEnd JustifyContent AlignItems


{-| align-items/justify-content center
-}
center : Alignment JustifyContent AlignItems
center =
    Center JustifyContent AlignItems


{-| justify-content spaceBetween
-}
spaceBetween : Alignment JustifyContent Never
spaceBetween =
    SpaceBetween JustifyContent


{-| justify-content spaceAround
-}
spaceAround : Alignment JustifyContent Never
spaceAround =
    SpaceAround JustifyContent


{-| align-items baseline
-}
baseline : Alignment Never AlignItems
baseline =
    Baseline AlignItems


{-| align-items stretch
-}
stretch : Alignment Never AlignItems
stretch =
    Stretch AlignItems


addPrefix : String -> String -> Style
addPrefix propertyName value =
    batch
        [ property ("-webkit-" ++ propertyName) value
        , property propertyName value
        , property ("-ms-" ++ propertyName) value
        ]
