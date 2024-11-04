module Nri.Ui.Table.V8 exposing
    ( Column, SortDirection(..), custom, string, rowHeader, placeholderColumn
    , disableAlternatingRowColors, css, Attr
    , view, viewWithoutHeader
    , viewLoading, viewLoadingWithoutHeader
    )

{-| Upgrading from V7:

  - New attribute added to define `alternatingRowColors`, set it to true to get default behavior.
  - Consider moving to `SortableTable`, as in V4 a non-interactive table renders
    just the same but has additional flexibility in the API and will make future
    upgrades easier.

@docs Column, SortDirection, custom, string, rowHeader, placeholderColumn
@docs disableAlternatingRowColors, css, Attr

@docs view, viewWithoutHeader

@docs viewLoading, viewLoadingWithoutHeader

-}

import Accessibility.Styled.Style as Style
import Css exposing (..)
import Css.Animations
import Css.Global
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Fonts.V1 exposing (baseFont)


{-| Closed representation of how to render the header and cells of a column
in the table
-}
type Column data msg
    = Column (Html msg) (data -> Html msg) Style (data -> List Style) (Maybe SortDirection) CellType
    | PlaceholderColumn Style


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


cell : CellType -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
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


{-| Creates a placeholder column which will reserve the space for the column,
this can be used when multiple tables have similar data and we want to keep
the size consistent.
-}
placeholderColumn :
    { width : LengthOrAuto compatible
    }
    -> Column data msg
placeholderColumn options =
    PlaceholderColumn (Css.width options.width)


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



-- Model


type alias Config =
    { css : List Style, alternatingRowColors : Bool }


{-| Attribute to configure the table
-}
type Attr
    = Attr (Config -> Config)


defaultConfig : Config
defaultConfig =
    { css = [], alternatingRowColors = True }


{-| Add a CSS style to the table
-}
css : List Style -> Attr
css styles =
    Attr <|
        \config ->
            { config | css = styles ++ config.css }


{-| disable alternating row colors, default behavior alternates row colors
-}
disableAlternatingRowColors : Attr
disableAlternatingRowColors =
    Attr <|
        \config ->
            { config | alternatingRowColors = False }



-- VIEW


{-| Displays a table of data without a header row
-}
viewWithoutHeader : List Attr -> List (Column data msg) -> List data -> Html msg
viewWithoutHeader attrs columns =
    let
        config =
            List.foldl
                (\attr soFar ->
                    case attr of
                        Attr f ->
                            f soFar
                )
                defaultConfig
                attrs
    in
    tableWithoutHeader config.css columns (viewRow columns config.alternatingRowColors)


{-| Displays a table of data based on the provided column definitions
-}
view : List Attr -> List (Column data msg) -> List data -> Html msg
view attrs columns =
    let
        config =
            List.foldl
                (\attr soFar ->
                    case attr of
                        Attr f ->
                            f soFar
                )
                defaultConfig
                attrs
    in
    tableWithHeader config.css columns (viewRow columns config.alternatingRowColors)


viewRow : List (Column data msg) -> Bool -> data -> Html msg
viewRow columns alternatingRowColors data =
    tr
        [ Attributes.css (rowStyles alternatingRowColors) ]
        (List.map (viewColumn data) columns)


viewColumn : data -> Column data msg -> Html msg
viewColumn data column =
    case column of
        Column _ renderer width cellStyles _ cellType ->
            cell cellType
                [ Attributes.css ([ width, verticalAlign middle, Css.paddingLeft (Css.px 12), Css.paddingRight (Css.px 12), textAlign left ] ++ cellStyles data)
                ]
                [ renderer data ]

        PlaceholderColumn width ->
            cell DataCell
                [ Attributes.css [ width, verticalAlign middle ] ]
                []



-- VIEW LOADING


{-| Display a table with the given columns but instead of data, show blocked
out text with an interesting animation. This view lets the user know that
data is on its way and what it will look like when it arrives.
-}
viewLoading : List Attr -> List (Column data msg) -> Html msg
viewLoading attrs columns =
    let
        config =
            List.foldl
                (\attr soFar ->
                    case attr of
                        Attr f ->
                            f soFar
                )
                defaultConfig
                attrs
    in
    tableWithHeader (loadingTableStyles ++ config.css) columns (viewLoadingRow columns config.alternatingRowColors) (List.range 0 8)


{-| Display the loading table without a header row
-}
viewLoadingWithoutHeader : List Attr -> List (Column data msg) -> Html msg
viewLoadingWithoutHeader attrs columns =
    let
        config =
            List.foldl
                (\attr soFar ->
                    case attr of
                        Attr f ->
                            f soFar
                )
                defaultConfig
                attrs
    in
    tableWithoutHeader (loadingTableStyles ++ config.css) columns (viewLoadingRow columns config.alternatingRowColors) (List.range 0 8)


viewLoadingRow : List (Column data msg) -> Bool -> Int -> Html msg
viewLoadingRow columns alternatingRowColors index =
    tr
        [ Attributes.css (rowStyles alternatingRowColors) ]
        (List.indexedMap (viewLoadingColumn index) columns)


viewLoadingColumn : Int -> Int -> Column data msg -> Html msg
viewLoadingColumn rowIndex colIndex column =
    case column of
        Column _ _ width _ _ cellType ->
            cell cellType
                [ Attributes.css (stylesLoadingColumn rowIndex colIndex width ++ [ verticalAlign middle ] ++ loadingCellStyles)
                ]
                [ span [ Attributes.css loadingContentStyles ] [] ]

        PlaceholderColumn width ->
            cell DataCell
                [ Attributes.css (stylesLoadingColumn rowIndex colIndex width ++ [ verticalAlign middle ] ++ loadingCellStyles) ]
                [ span [ Attributes.css loadingContentStyles ] [] ]


stylesLoadingColumn : Int -> Int -> Style -> List Style
stylesLoadingColumn rowIndex colIndex width =
    [ width
    , property "animation-delay" (String.fromFloat (toFloat (rowIndex + colIndex) * 0.1) ++ "s")
    ]



-- HELP


tableWithoutHeader : List Style -> List (Column data msg) -> (a -> Html msg) -> List a -> Html msg
tableWithoutHeader styles columns toRow data =
    table styles
        columns
        [ thead [] [ tr Style.invisible (List.map tableColHeader columns) ]
        , tableBody toRow data
        ]


tableWithHeader : List Style -> List (Column data msg) -> (a -> Html msg) -> List a -> Html msg
tableWithHeader styles columns toRow data =
    table styles
        columns
        [ tableHeader columns
        , tableBody toRow data
        ]


table : List Style -> List (Column data msg) -> List (Html msg) -> Html msg
table styles columns =
    Html.table [ Attributes.css (styles ++ tableStyles columns) ]


tableHeader : List (Column data msg) -> Html msg
tableHeader columns =
    thead []
        [ tr [ Attributes.css headersStyles ]
            (List.map tableColHeader columns)
        ]


tableColHeader : Column data msg -> Html msg
tableColHeader column =
    case column of
        Column header _ width _ sort _ ->
            th
                [ Attributes.scope "col"
                , Attributes.css (width :: headerStyles)
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

        PlaceholderColumn width ->
            th [ Attributes.css [ width ] ] []


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


rowStyles : Bool -> List Style
rowStyles alternatingRowColors =
    [ height (px 45)
    , fontSize (px 14)
    , color gray20
    , if alternatingRowColors then
        pseudoClass "nth-child(odd)"
            [ backgroundColor gray96 ]

      else
        Css.batch [ Css.borderBottom3 (Css.px 1) Css.solid gray92 ]
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


tableStyles : List (Column data msg) -> List Style
tableStyles columns =
    let
        hasPlaceholderColumns =
            List.any
                (\column ->
                    case column of
                        Column _ _ _ _ _ _ ->
                            False

                        PlaceholderColumn _ ->
                            True
                )
                columns
    in
    [ borderCollapse collapse
    , baseFont
    , Css.width (Css.pct 100)
    , if hasPlaceholderColumns then
        Css.tableLayout Css.fixed

      else
        Css.batch []
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
