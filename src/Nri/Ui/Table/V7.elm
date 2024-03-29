module Nri.Ui.Table.V7 exposing
    ( Column, SortDirection(..), custom, string, rowHeader
    , view, viewWithoutHeader
    , viewLoading, viewLoadingWithoutHeader
    )

{-| Upgrading from V6:

  - If you don't have extra styles for the table, add an empty list to calls
    to `view`.
  - Consider moving to `SortableTable`, as in V4 a non-interactive table renders
    just the same but has additional flexibility in the API and will make future
    upgrades easier.

@docs Column, SortDirection, custom, string, rowHeader

@docs view, viewWithoutHeader

@docs viewLoading, viewLoadingWithoutHeader

-}

import Accessibility.Styled.Style as Style
import Css exposing (..)
import Css.Animations
import Css.Global
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Fonts.V1 exposing (baseFont)


{-| Closed representation of how to render the header and cells of a column
in the table
-}
type Column data msg
    = Column (Html msg) (data -> Html msg) Style (data -> List Style) (Maybe SortDirection) CellType


{-| Which direction is a table column sorted? Only set these on columns that
actually have an explicit sort!
-}
type SortDirection
    = Ascending
    | Descending


{-| Is this cell a data cell or header cell?
-}
type CellType
    = RowHeaderCell
    | DataCell


cell : CellType -> List (Attribute msg) -> List (Html msg) -> Html msg
cell cellType attrs =
    case cellType of
        RowHeaderCell ->
            th (Attributes.scope "row" :: attrs)

        DataCell ->
            td attrs


{-| A column that renders some aspect of a value as text
-}
string :
    { header : String
    , value : data -> String
    , width : LengthOrAuto compatible
    , cellStyles : data -> List Style
    , sort : Maybe SortDirection
    }
    -> Column data msg
string { header, value, width, cellStyles, sort } =
    Column (Html.text header) (value >> Html.text) (Css.width width) cellStyles sort DataCell


{-| A column that renders however you want it to
-}
custom :
    { header : Html msg
    , view : data -> Html msg
    , width : LengthOrAuto compatible
    , cellStyles : data -> List Style
    , sort : Maybe SortDirection
    }
    -> Column data msg
custom options =
    Column options.header options.view (Css.width options.width) options.cellStyles options.sort DataCell


{-| A column whose cells are row headers
-}
rowHeader :
    { header : Html msg
    , view : data -> Html msg
    , width : LengthOrAuto compatible
    , cellStyles : data -> List Style
    , sort : Maybe SortDirection
    }
    -> Column data msg
rowHeader options =
    Column options.header options.view (Css.width options.width) options.cellStyles options.sort RowHeaderCell



-- VIEW


{-| Displays a table of data without a header row
-}
viewWithoutHeader : List Style -> List (Column data msg) -> List data -> Html msg
viewWithoutHeader additionalStyles columns =
    tableWithoutHeader additionalStyles columns (viewRow columns)


{-| Displays a table of data based on the provided column definitions
-}
view : List Style -> List (Column data msg) -> List data -> Html msg
view additionalStyles columns =
    tableWithHeader additionalStyles columns (viewRow columns)


viewRow : List (Column data msg) -> data -> Html msg
viewRow columns data =
    tr
        [ css rowStyles ]
        (List.map (viewColumn data) columns)


viewColumn : data -> Column data msg -> Html msg
viewColumn data (Column _ renderer width cellStyles _ cellType) =
    cell cellType
        [ css ([ width, verticalAlign middle ] ++ cellStyles data)
        ]
        [ renderer data ]



-- VIEW LOADING


{-| Display a table with the given columns but instead of data, show blocked
out text with an interesting animation. This view lets the user know that
data is on its way and what it will look like when it arrives.
-}
viewLoading : List Style -> List (Column data msg) -> Html msg
viewLoading additionalStyles columns =
    tableWithHeader (loadingTableStyles ++ additionalStyles) columns (viewLoadingRow columns) (List.range 0 8)


{-| Display the loading table without a header row
-}
viewLoadingWithoutHeader : List Style -> List (Column data msg) -> Html msg
viewLoadingWithoutHeader additionalStyles columns =
    tableWithoutHeader (loadingTableStyles ++ additionalStyles) columns (viewLoadingRow columns) (List.range 0 8)


viewLoadingRow : List (Column data msg) -> Int -> Html msg
viewLoadingRow columns index =
    tr
        [ css rowStyles ]
        (List.indexedMap (viewLoadingColumn index) columns)


viewLoadingColumn : Int -> Int -> Column data msg -> Html msg
viewLoadingColumn rowIndex colIndex (Column _ _ width _ _ cellType) =
    cell cellType
        [ css (stylesLoadingColumn rowIndex colIndex width ++ [ verticalAlign middle ] ++ loadingCellStyles)
        ]
        [ span [ css loadingContentStyles ] [] ]


stylesLoadingColumn : Int -> Int -> Style -> List Style
stylesLoadingColumn rowIndex colIndex width =
    [ width
    , property "animation-delay" (String.fromFloat (toFloat (rowIndex + colIndex) * 0.1) ++ "s")
    ]



-- HELP


tableWithoutHeader : List Style -> List (Column data msg) -> (a -> Html msg) -> List a -> Html msg
tableWithoutHeader styles columns toRow data =
    table styles
        [ thead [] [ tr Style.invisible (List.map tableColHeader columns) ]
        , tableBody toRow data
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
            (List.map tableColHeader columns)
        ]


tableColHeader : Column data msg -> Html msg
tableColHeader (Column header _ width _ sort _) =
    th
        [ Attributes.scope "col"
        , css (width :: headerStyles)
        , Attributes.attribute "aria-sort" <|
            case sort of
                Nothing ->
                    "none"

                Just Ascending ->
                    "ascending"

                Just Descending ->
                    "descending"
        ]
        [ header ]


tableBody : (a -> Html msg) -> List a -> Html msg
tableBody toRow items =
    tbody [] (List.map toRow items)



-- STYLES


headersStyles : List Style
headersStyles =
    [ -- We use a inset box shadow for a bottom border instead of an actual
      -- border because with our use of `border-collapse: collapse`, the bottom
      -- gray border sticks to the table instead of traveling with the header
      -- when the header has `position: sticky` applied.
      --
      -- We add the style on the children instead of on the parent because
      -- `<tr>` tags do not display box shadows in Safari (although by the time
      -- you read this, they may have fixed it. Feel free to try again!)
      Css.Global.children [ Css.Global.th [ boxShadow4 inset (px 0) (px -3) gray75 ] ]
    , height (px 45)
    , fontSize (px 15)
    ]


headerStyles : List Style
headerStyles =
    [ padding4 (px 11) (px 12) (px 14) (px 12)
    , textAlign left
    , fontWeight bold
    , color gray20
    ]


rowStyles : List Style
rowStyles =
    [ height (px 45)
    , fontSize (px 14)
    , color gray20
    , pseudoClass "nth-child(odd)"
        [ backgroundColor gray96 ]
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
