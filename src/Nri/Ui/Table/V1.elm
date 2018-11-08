module Nri.Ui.Table.V1 exposing
    ( Column, custom, string, styles
    , view, viewWithoutHeader
    , viewLoading, viewLoadingWithoutHeader
    , keyframes, keyframeStyles
    )

{-|

@docs Column, custom, string, styles

@docs view, viewWithoutHeader

@docs viewLoading, viewLoadingWithoutHeader

@docs keyframes, keyframeStyles

-}

import Css exposing (..)
import Css.Global exposing (Snippet, adjacentSiblings, children, class, descendants, each, everything, media, selector, withClass)
import Html exposing (..)
import Html.Attributes exposing (style)
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Fonts.V1 exposing (baseFont)
import Nri.Ui.Styles.V1 exposing (styles)


{-| Closed representation of how to render the header and cells of a column
in the table
-}
type Column data msg
    = Column (Html msg) (data -> Html msg) Int


{-| A column that renders some aspect of a value as text
-}
string :
    { header : String
    , value : data -> String
    , width : Int
    }
    -> Column data msg
string { header, value, width } =
    Column (Html.text header) (value >> Html.text) width


{-| A column that renders however you want it to
-}
custom :
    { header : Html msg
    , view : data -> Html msg
    , width : Int
    }
    -> Column data msg
custom { header, view, width } =
    Column header view width



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
        [ styles.class [ Headers ] ]
        (List.map viewRowHeader columns)


viewRowHeader : Column data msg -> Html msg
viewRowHeader (Column header _ width) =
    th
        [ styles.class [ Header ]
        , style [ ( "width", toString width ++ "px" ) ]
        ]
        [ header ]


viewRow : List (Column data msg) -> data -> Html msg
viewRow columns data =
    tr
        [ styles.class [ Row ] ]
        (List.map (viewColumn data) columns)


viewColumn : data -> Column data msg -> Html msg
viewColumn data (Column _ renderer width) =
    td
        [ styles.class [ Cell ]
        , style [ ( "width", toString width ++ "px" ) ]
        ]
        [ renderer data ]



-- VIEW LOADING


{-| Display a table with the given columns but instead of data, show blocked
out text with an interesting animation. This view lets the user know that
data is on its way and what it will look like when it arrives.
-}
viewLoading : List (Column data msg) -> Html msg
viewLoading columns =
    tableWithHeader [ LoadingTable ] columns <|
        List.map (viewLoadingRow columns) (List.range 0 8)


{-| Display the loading table without a header row
-}
viewLoadingWithoutHeader : List (Column data msg) -> Html msg
viewLoadingWithoutHeader columns =
    table [ LoadingTable ] <|
        List.map (viewLoadingRow columns) (List.range 0 8)


viewLoadingRow : List (Column data msg) -> Int -> Html msg
viewLoadingRow columns index =
    tr
        [ styles.class [ Row ] ]
        (List.indexedMap (viewLoadingColumn index) columns)


viewLoadingColumn : Int -> Int -> Column data msg -> Html msg
viewLoadingColumn rowIndex colIndex (Column _ _ width) =
    td
        [ styles.class [ Cell, LoadingCell ]
        , style (stylesLoadingColumn rowIndex colIndex width)
        ]
        [ span [ styles.class [ LoadingContent ] ] [] ]


stylesLoadingColumn : Int -> Int -> Int -> List ( String, String )
stylesLoadingColumn rowIndex colIndex width =
    [ ( "width", toString width ++ "px" )
    , ( "animation-delay"
      , toString (toFloat (rowIndex + colIndex) * 0.1) ++ "s"
      )
    ]



-- HELP


table : List CssClasses -> List (Html msg) -> Html msg
table classes =
    Html.table [ styles.class (Table :: classes) ]


tableWithHeader : List CssClasses -> List (Column data msg) -> List (Html msg) -> Html msg
tableWithHeader classes columns rows =
    table classes (viewHeaders columns :: rows)



-- STYLES


type CssClasses
    = Table
    | LoadingTable
    | Row
    | Cell
    | Headers
    | Header
    | LoadingContent
    | LoadingCell


{-| -}
styles : Nri.Ui.Styles.V1.Styles Never CssClasses msg
styles =
    Nri.Ui.Styles.V1.styles "Nri-Ui-Table-V1-"
        [ Css.Global.class Headers
            [ borderBottom3 (px 3) solid gray75
            , height (px 45)
            , fontSize (px 15)
            ]
        , Css.Global.class Header
            [ padding4 (px 15) (px 12) (px 11) (px 12)
            , textAlign left
            , fontWeight bold
            ]
        , Css.Global.class Row
            [ height (px 45)
            , fontSize (px 14)
            , color gray20
            , pseudoClass "nth-child(odd)"
                [ backgroundColor gray96 ]
            ]
        , Css.Global.class Cell
            [ width (px 300)
            , padding2 (px 14) (px 10)
            ]
        , Css.Global.class LoadingContent
            [ width (pct 100)
            , display inlineBlock
            , height (Css.em 1)
            , borderRadius (Css.em 1)
            , backgroundColor gray75
            ]
        , Css.Global.class LoadingCell
            flashAnimation
        , Css.Global.class LoadingTable
            fadeInAnimation
        , Css.Global.class Table
            [ borderCollapse collapse
            , baseFont
            ]
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
