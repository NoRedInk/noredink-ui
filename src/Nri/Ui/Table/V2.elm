module Nri.Ui.Table.V2
    exposing
        ( Column
        , custom
        , keyframeStyles
        , keyframes
        , string
        , view
        , viewLoading
        , viewLoadingWithoutHeader
        , viewWithoutHeader
        )

{-|

@docs Column, custom, string

@docs view, viewWithoutHeader

@docs viewLoading, viewLoadingWithoutHeader

@docs keyframes, keyframeStyles

-}

import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Fonts.V1 exposing (baseFont)
import Nri.Ui.Styles.V1


{-| Closed representation of how to render the header and cells of a column
in the table
-}
type Column data msg
    = Column (Html msg) (data -> Html msg) Style


type alias WidthStyle =
    List ( String, String )


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
viewWithoutHeader columns data =
    table [] <|
        List.map (viewRow columns) data


{-| Displays a table of data based on the provided column definitions
-}
view : List (Column data msg) -> List data -> Html msg
view columns data =
    tableWithHeader [] columns <|
        List.map (viewRow columns) data


viewHeaders : List (Column data msg) -> Html msg
viewHeaders columns =
    tr
        [ css headersStyles ]
        (List.map viewRowHeader columns)


viewRowHeader : Column data msg -> Html msg
viewRowHeader (Column header _ width) =
    th
        [ css (width :: headerStyles)
        ]
        [ header ]


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
    tableWithHeader loadingTableStyles columns <|
        List.map (viewLoadingRow columns) (List.range 0 8)


{-| Display the loading table without a header row
-}
viewLoadingWithoutHeader : List (Column data msg) -> Html msg
viewLoadingWithoutHeader columns =
    table loadingTableStyles <|
        List.map (viewLoadingRow columns) (List.range 0 8)


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


table : List Style -> List (Html msg) -> Html msg
table styles =
    Html.table [ css (styles ++ tableStyles) ]


tableWithHeader : List Style -> List (Column data msg) -> List (Html msg) -> Html msg
tableWithHeader styles columns rows =
    table styles (viewHeaders columns :: rows)



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
    , color gray45
    , pseudoClass "nth-child(odd)"
        [ backgroundColor gray96 ]
    ]


cellStyles : List Style
cellStyles =
    [ padding2 (px 14) (px 10)
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
    flashAnimation


loadingTableStyles : List Style
loadingTableStyles =
    fadeInAnimation


tableStyles : List Style
tableStyles =
    [ borderCollapse collapse
    , baseFont
    , Css.width (Css.pct 100)
    ]


{-| -}
keyframes : List Nri.Ui.Styles.V1.Keyframe
keyframes =
    [ Nri.Ui.Styles.V1.keyframes "Nri-Ui-Table-V1-flash"
        [ ( "0%", "opacity: 0.6" )
        , ( "50%", "opacity: 0.2" )
        , ( "100%", "opacity: 0.6" )
        ]
    , Nri.Ui.Styles.V1.keyframes "Nri-Ui-Table-V1-fadein"
        [ ( "from", "opacity: 0" )
        , ( "to", "opacity: 1" )
        ]
    ]


{-| -}
keyframeStyles : Html msg
keyframeStyles =
    Html.node "style"
        []
        (List.map (Html.text << Nri.Ui.Styles.V1.toString) keyframes)


flashAnimation : List Css.Style
flashAnimation =
    [ property "-webkit-animation" "Nri-Ui-Table-V1-flash 2s infinite"
    , property "-moz-animation" "Nri-Ui-Table-V1-flash 2s infinite"
    , property "animation" "Nri-Ui-Table-V1-flash 2s infinite"
    , opacity (num 0.6)
    ]


fadeInAnimation : List Css.Style
fadeInAnimation =
    [ property "-webkit-animation" "Nri-Ui-Table-V1-fadein 0.4s 0.2s forwards"
    , property "-moz-animation" "Nri-Ui-Table-V1-fadein 0.4s 0.2s forwards"
    , property "animation" "Nri-Ui-Table-V1-fadein 0.4s 0.2s forwards"
    , opacity (num 0)
    ]
