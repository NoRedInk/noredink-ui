module Nri.Ui.SortableTable.V1 exposing
    ( Column, Config, Sorter, State
    , init, initDescending
    , custom, string, view, viewLoading
    , invariantSort, simpleSort, combineSorters
    )

{-|

@docs Column, Config, Sorter, State
@docs init, initDescending
@docs custom, string, view, viewLoading
@docs invariantSort, simpleSort, combineSorters

-}

import Css exposing (..)
import Css.Global exposing (Snippet, adjacentSiblings, children, class, descendants, each, everything, media, selector, withClass)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events
import Nri.Ui.Colors.V1
import Nri.Ui.CssVendorPrefix.V1 as CssVendorPrefix
import Nri.Ui.Icon.V5 as Icon
import Nri.Ui.Table.V4
import Svg exposing (Svg)
import Svg.Attributes


type SortDirection
    = Ascending
    | Descending


{-| -}
type alias Sorter a =
    SortDirection -> a -> a -> Order


{-| -}
type Column id entry msg
    = Column
        { id : id
        , header : Html msg
        , view : entry -> Html msg
        , sorter : Sorter entry
        , width : Int
        }


{-| -}
type alias State id =
    { column : id
    , sortDirection : SortDirection
    }


{-| -}
type alias Config id entry msg =
    { updateMsg : State id -> msg
    , columns : List (Column id entry msg)
    }


{-| -}
init : id -> State id
init initialSort =
    { column = initialSort
    , sortDirection = Ascending
    }


{-| -}
initDescending : id -> State id
initDescending initialSort =
    { column = initialSort
    , sortDirection = Descending
    }


{-| -}
string :
    { id : id
    , header : String
    , value : entry -> String
    , width : Int
    }
    -> Column id entry msg
string { id, header, value, width } =
    Column
        { id = id
        , header = Html.text header
        , view = value >> Html.text
        , sorter = simpleSort value
        , width = width
        }


{-| -}
custom :
    { id : id
    , header : Html msg
    , view : entry -> Html msg
    , sorter : Sorter entry
    , width : Int
    }
    -> Column id entry msg
custom config =
    Column
        { id = config.id
        , header = config.header
        , view = config.view
        , sorter = config.sorter
        , width = config.width
        }


{-| Create a sorter function that always orders the entries in the same order.
For example, this is useful when we want to resolve ties and sort the tied
entries by name, no matter of the sort direction set on the table.
-}
invariantSort : (entry -> comparable) -> Sorter entry
invariantSort mapper =
    \sortDirection elem1 elem2 ->
        compare (mapper elem1) (mapper elem2)


{-| Create a simple sorter function that orders entries by mapping a function
over the collection. It will also reverse it when the sort direction is descending.
-}
simpleSort : (entry -> comparable) -> Sorter entry
simpleSort mapper =
    \sortDirection elem1 elem2 ->
        let
            result =
                compare (mapper elem1) (mapper elem2)
        in
        case sortDirection of
            Ascending ->
                result

            Descending ->
                flipOrder result


flipOrder : Order -> Order
flipOrder order =
    case order of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


{-| -}
combineSorters : List (Sorter entry) -> Sorter entry
combineSorters sorters =
    \sortDirection elem1 elem2 ->
        let
            folder =
                \sorter acc ->
                    case acc of
                        EQ ->
                            sorter sortDirection elem1 elem2

                        _ ->
                            acc
        in
        List.foldl folder EQ sorters


{-| -}
viewLoading : Config id entry msg -> State id -> Html msg
viewLoading config state =
    let
        tableColumns =
            List.map (buildTableColumn config.updateMsg state) config.columns
    in
    Nri.Ui.Table.V4.viewLoading
        tableColumns


{-| -}
view : Config id entry msg -> State id -> List entry -> Html msg
view config state entries =
    let
        tableColumns =
            List.map (buildTableColumn config.updateMsg state) config.columns

        sorter =
            findSorter config.columns state.column
    in
    Nri.Ui.Table.V4.view
        tableColumns
        (List.sortWith (sorter state.sortDirection) entries)


findSorter : List (Column id entry msg) -> id -> Sorter entry
findSorter columns columnId =
    columns
        |> listExtraFind (\(Column column) -> column.id == columnId)
        |> Maybe.map (\(Column column) -> column.sorter)
        |> Maybe.withDefault identitySorter


{-| Taken from <https://github.com/elm-community/list-extra/blob/8.2.0/src/List/Extra.elm#L556>
-}
listExtraFind : (a -> Bool) -> List a -> Maybe a
listExtraFind predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                listExtraFind predicate rest


identitySorter : Sorter a
identitySorter =
    \sortDirection item1 item2 ->
        EQ


buildTableColumn : (State id -> msg) -> State id -> Column id entry msg -> Nri.Ui.Table.V4.Column entry msg
buildTableColumn updateMsg state (Column column) =
    Nri.Ui.Table.V4.custom
        { header = viewSortHeader column.header updateMsg state column.id
        , view = column.view
        , width = Css.px (toFloat column.width)
        }


viewSortHeader : Html msg -> (State id -> msg) -> State id -> id -> Html msg
viewSortHeader header updateMsg state id =
    let
        nextState =
            nextTableState state id

        headerStyle =
            if state.column == id then
                [ fontWeight bold ]

            else
                [ fontWeight normal ]
    in
    Html.div
        [ css
            [ Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.spaceBetween
            , cursor pointer
            , CssVendorPrefix.property "user-select" "none"
            ]
        , css headerStyle
        , Html.Styled.Events.onClick (updateMsg nextState)
        ]
        [ Html.div [] [ header ]
        , viewSortButton updateMsg state id
        ]


viewSortButton : (State id -> msg) -> State id -> id -> Html msg
viewSortButton updateMsg state id =
    let
        ifActive bool =
            if bool then
                [ css [ color Nri.Ui.Colors.V1.azure ] ]

            else
                []

        arrows upHighlighted downHighlighted =
            Html.div
                [ css
                    [ Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.alignItems Css.center
                    , Css.justifyContent Css.center
                    ]
                ]
                [ Html.div
                    (css [ arrow Up ] :: ifActive upHighlighted)
                    [ Icon.decorativeIcon (Icon.sortArrow { sortArrow = "sort_arrow" }) ]
                , Html.div
                    (css [ arrow Down ] :: ifActive downHighlighted)
                    [ Icon.decorativeIcon (Icon.sortArrow { sortArrow = "sort_arrow" }) ]
                ]

        buttonContent =
            case ( state.column == id, state.sortDirection ) of
                ( True, Ascending ) ->
                    arrows True False

                ( True, Descending ) ->
                    arrows False True

                ( False, _ ) ->
                    arrows False False
    in
    Html.div
        [ css
            [ padding (px 2)
            , color Nri.Ui.Colors.V1.gray75
            ]
        ]
        [ buttonContent ]


nextTableState : State id -> id -> State id
nextTableState state id =
    if state.column == id then
        { column = id
        , sortDirection = flipSortDirection state.sortDirection
        }

    else
        { column = id
        , sortDirection = Ascending
        }


flipSortDirection : SortDirection -> SortDirection
flipSortDirection order =
    case order of
        Ascending ->
            Descending

        Descending ->
            Ascending


type Direction
    = Up
    | Down


arrow : Direction -> Css.Style
arrow direction =
    let
        result =
            case direction of
                Up ->
                    []

                Down ->
                    [ transform <| rotate (deg 180) ]
    in
    Css.batch
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


sortArrow : Svg msg
sortArrow =
    Svg.svg
        [ Svg.Attributes.viewBox "0 0 8 6" ]
        [ Svg.polygon [ Svg.Attributes.points "0 6 4 0 8 6 0 6" ] []
        ]
