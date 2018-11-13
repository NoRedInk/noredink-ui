module Nri.Ui.Table.V4 exposing
    ( Column, custom, string
    , view, viewWithoutHeader
    , viewLoading, viewLoadingWithoutHeader
    )

{-| Upgrading from V1:

  - All the `width` fields in column configurations now take an elm-css length
    value rather than an Integer. Change `width = 100` to `width = px 100` to get
    the same widths as before.
  - Tables now by default take the full width of the container they are placed in.
    If this is not what you want, wrap the table in an element with a fixed width.
  - The table module now makes use of `Html.Styled` and no longer exposes a
    separate `styles` value.
    Check out the [elm-css](http://package.elm-lang.org/packages/rtfeldman/elm-css/14.0.0/Html-Styled)
    documentation on Html.Styled to see how to work with it.
  - The default cell padding has been removed and content is not vertically
    centered in its cell. If you need to overwrite this, wrap your cells in
    elements providing custom styling to the cell.

@docs Column, custom, string

@docs view, viewWithoutHeader

@docs viewLoading, viewLoadingWithoutHeader

-}

import Css exposing (..)
import Css.Animations
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Fonts.V1 exposing (baseFont)


{-| Closed representation of how to render the header and cells of a column
in the table
-}
type Column data msg
    = Column (Html msg) (data -> Html msg) Style


{-| A column that renders some aspect of a value as text
-}
string :
    { header : String
    , value : data -> String
    , width : LengthOrAuto compatible
    }
    -> Column data msg
string { header, value, width } =
    Column (Html.text header) (value >> Html.text) (Css.width width)


{-| A column that renders however you want it to
-}
custom :
    { header : Html msg
    , view : data -> Html msg
    , width : LengthOrAuto compatible
    }
    -> Column data msg
custom { header, view, width } =
    Column header view (Css.width width)



-- VIEW


{-| Displays a table of data without a header row
-}
viewWithoutHeader : List (Column data msg) -> List data -> Html msg
viewWithoutHeader columns =
    tableWithoutHeader [] columns (viewRow columns)


{-| Displays a table of data based on the provided column definitions
-}
view : List (Column data msg) -> List data -> Html msg
view columns =
    tableWithHeader [] columns (viewRow columns)


viewRow : List (Column data msg) -> data -> Html msg
viewRow columns data =
    tr
        [ css rowStyles ]
        (List.map (viewColumn data) columns)


viewColumn : data -> Column data msg -> Html msg
viewColumn data (Column _ renderer width) =
    td
        [ css (width :: cellStyles)
        ]
        [ renderer data ]



-- VIEW LOADING


{-| Display a table with the given columns but instead of data, show blocked
out text with an interesting animation. This view lets the user know that
data is on its way and what it will look like when it arrives.
-}
viewLoading : List (Column data msg) -> Html msg
viewLoading columns =
    tableWithHeader loadingTableStyles columns (viewLoadingRow columns) (List.range 0 8)


{-| Display the loading table without a header row
-}
viewLoadingWithoutHeader : List (Column data msg) -> Html msg
viewLoadingWithoutHeader columns =
    tableWithoutHeader loadingTableStyles columns (viewLoadingRow columns) (List.range 0 8)


viewLoadingRow : List (Column data msg) -> Int -> Html msg
viewLoadingRow columns index =
    tr
        [ css rowStyles ]
        (List.indexedMap (viewLoadingColumn index) columns)


viewLoadingColumn : Int -> Int -> Column data msg -> Html msg
viewLoadingColumn rowIndex colIndex (Column _ _ width) =
    td
        [ css (stylesLoadingColumn rowIndex colIndex width ++ cellStyles ++ loadingCellStyles)
        ]
        [ span [ css loadingContentStyles ] [] ]


stylesLoadingColumn : Int -> Int -> Style -> List Style
stylesLoadingColumn rowIndex colIndex width =
    [ width
    , property "animation-delay" (toString (toFloat (rowIndex + colIndex) * 0.1) ++ "s")
    ]



-- HELP


tableWithoutHeader : List Style -> List (Column data msg) -> (a -> Html msg) -> List a -> Html msg
tableWithoutHeader styles columns toRow data =
    table styles
        [ tableBody toRow data
        ]


tableWithHeader : List Style -> List (Column data msg) -> (a -> Html msg) -> List a -> Html msg
tableWithHeader styles columns toRow data =
    table styles
        [ tableHeader columns
        , tableBody toRow data
        ]


table : List Style -> List (Html msg) -> Html msg
table styles =
    Html.table [ css (styles ++ tableStyles) ]


tableHeader : List (Column data msg) -> Html msg
tableHeader columns =
    thead []
        [ tr [ css headersStyles ]
            (List.map tableRowHeader columns)
        ]


tableRowHeader : Column data msg -> Html msg
tableRowHeader (Column header _ width) =
    th
        [ css (width :: headerStyles)
        ]
        [ header ]


tableBody : (a -> Html msg) -> List a -> Html msg
tableBody toRow items =
    tbody [] (List.map toRow items)



-- STYLES


headersStyles : List Style
headersStyles =
    [ borderBottom3 (px 3) solid gray75
    , height (px 45)
    , fontSize (px 15)
    ]


headerStyles : List Style
headerStyles =
    [ padding4 (px 15) (px 12) (px 11) (px 12)
    , textAlign left
    , fontWeight bold
    ]


rowStyles : List Style
rowStyles =
    [ height (px 45)
    , fontSize (px 14)
    , color gray20
    , pseudoClass "nth-child(odd)"
        [ backgroundColor gray96 ]
    ]


cellStyles : List Style
cellStyles =
    [ verticalAlign middle
    ]


loadingContentStyles : List Style
loadingContentStyles =
    [ width (pct 100)
    , display inlineBlock
    , height (Css.em 1)
    , borderRadius (Css.em 1)
    , backgroundColor gray75
    ]


loadingCellStyles : List Style
loadingCellStyles =
    [ batch flashAnimation
    , padding2 (px 14) (px 10)
    ]


loadingTableStyles : List Style
loadingTableStyles =
    fadeInAnimation


tableStyles : List Style
tableStyles =
    [ borderCollapse collapse
    , baseFont
    , Css.width (Css.pct 100)
    ]


flash : Css.Animations.Keyframes {}
flash =
    Css.Animations.keyframes
        [ ( 0, [ Css.Animations.opacity (Css.num 0.6) ] )
        , ( 50, [ Css.Animations.opacity (Css.num 0.2) ] )
        , ( 100, [ Css.Animations.opacity (Css.num 0.6) ] )
        ]


fadeIn : Css.Animations.Keyframes {}
fadeIn =
    Css.Animations.keyframes
        [ ( 0, [ Css.Animations.opacity (Css.num 0) ] )
        , ( 100, [ Css.Animations.opacity (Css.num 1) ] )
        ]


flashAnimation : List Css.Style
flashAnimation =
    [ animationName flash
    , property "animation-duration" "2s"
    , property "animation-iteration-count" "infinite"
    , opacity (num 0.6)
    ]


fadeInAnimation : List Css.Style
fadeInAnimation =
    [ animationName fadeIn
    , property "animation-duration" "0.4s"
    , property "animation-delay" "0.2s"
    , property "animation-fill-mode" "forwards"
    , animationIterationCount (int 1)
    , opacity (num 0)
    ]
