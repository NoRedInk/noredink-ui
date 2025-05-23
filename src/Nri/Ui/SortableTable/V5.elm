module Nri.Ui.SortableTable.V5 exposing
    ( Column, Sorter, State
    , init, initDescending
    , custom, string, placeholderColumn
    , Attribute, updateMsg, tableAttribute, state, stickyHeader, stickyHeaderCustom, StickyConfig
    , view, viewLoading
    , invariantSort, simpleSort, combineSorters
    )

{-| Changes from V4:

  - Drops `disableAlternatingRowColors`
  - Adds `tableAttribute`, which lets us add any attributes for the underlying `Nri.Ui.Table`

@docs Column, Sorter, State
@docs init, initDescending
@docs custom, string, placeholderColumn
@docs Attribute, updateMsg, tableAttribute, state, stickyHeader, stickyHeaderCustom, StickyConfig
@docs view, viewLoading
@docs invariantSort, simpleSort, combineSorters

-}

import Accessibility.Styled.Aria as Aria
import Css exposing (..)
import Css.Global
import Css.Media
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events
import Nri.Ui.Colors.V1
import Nri.Ui.CssVendorPrefix.V1 as CssVendorPrefix
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 exposing (maybe)
import Nri.Ui.Html.V3 exposing (viewJust)
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Svg.V1
import Nri.Ui.Table.V8 as Table exposing (SortDirection(..))
import Nri.Ui.UiIcon.V2


{-| -}
type alias Sorter a =
    SortDirection -> a -> a -> Order


{-| -}
type Column id entry msg
    = Column
        { id : id
        , header : Html msg
        , view : entry -> Html msg
        , sorter : Maybe (Sorter entry)
        , width : Int
        , cellStyles : entry -> List Style
        , hidden : Bool
        }


{-| -}
type alias State id =
    { column : id
    , sortDirection : SortDirection
    }


{-| -}
type alias Config id msg =
    { updateMsg : Maybe (State id -> msg)
    , state : Maybe (State id)
    , stickyHeader : Maybe StickyConfig
    , tableAttributes : List Table.Attribute
    }


defaultConfig : Config id msg
defaultConfig =
    { updateMsg = Nothing
    , state = Nothing
    , stickyHeader = Nothing
    , tableAttributes = []
    }


{-| How the header will be set up to be sticky.

  - `topOffset` controls how far off the top of the viewport the headers will
    stick, in pixels. (**Default value:** 0)
  - `zIndex` controls where in the stacking context the header will end
    up. Useful to prevent elements in rows from appearing over the header.
    (**Default value:** 0)

Headers are never sticky on mobile-sized viewports because doing so causes some
accessibility issues with zooming and panning.

-}
type alias StickyConfig =
    { topOffset : Float
    , zIndex : Int
    , pageBackgroundColor : Css.Color
    , hoverZIndex : Maybe Int
    }


defaultStickyConfig : StickyConfig
defaultStickyConfig =
    { topOffset = 0
    , zIndex = 0
    , pageBackgroundColor = Nri.Ui.Colors.V1.white
    , hoverZIndex = Nothing
    }


consJust : Maybe a -> List a -> List a
consJust maybe_ list =
    case maybe_ of
        Just value ->
            value :: list

        Nothing ->
            list


stickyConfigStyles : StickyConfig -> List Style
stickyConfigStyles { topOffset, zIndex, pageBackgroundColor, hoverZIndex } =
    [ Css.Media.withMedia
        [ MediaQuery.notMobile ]
        [ Css.Global.children
            [ Css.Global.thead
                (consJust (Maybe.map (\index -> Css.hover [ Css.zIndex (Css.int index) ]) hoverZIndex)
                    [ Css.position Css.sticky
                    , Css.top (Css.px topOffset)
                    , Css.zIndex (Css.int zIndex)
                    , Css.backgroundColor pageBackgroundColor
                    ]
                )
            ]
        ]
    ]


{-| Customize how the table is rendered, for example by adding sorting or
stickiness.
-}
type Attribute id msg
    = Attribute (Config id msg -> Config id msg)


{-| Sort a column. You can get an initial state with `init` or `initDescending`.
If you make this sorting interactive, you should store the state in your model
and provide it to this function instead of recreating it on every update.
-}
state : State id -> Attribute id msg
state state_ =
    Attribute (\config -> { config | state = Just state_ })


{-| Add interactivity in sorting columns. When this attribute is provided and
sorting is enabled, columns will be sortable by clicking the headers.
-}
updateMsg : (State id -> msg) -> Attribute id msg
updateMsg updateMsg_ =
    Attribute (\config -> { config | updateMsg = Just updateMsg_ })


{-| Make the header sticky (that is, it will stick to the top of the viewport
when it otherwise would have been scrolled off.) You probably will want to set a
background color on the header as well.
-}
stickyHeader : Attribute id msg
stickyHeader =
    Attribute (\config -> { config | stickyHeader = Just defaultStickyConfig })


{-| Attributes forwarded to the underlying Nri.Ui.Table
-}
tableAttribute : Table.Attribute -> Attribute id msg
tableAttribute attr =
    Attribute (\config -> { config | tableAttributes = attr :: config.tableAttributes })


{-| Does the same thing as `stickyHeader`, but with adaptations for your
specific use.
-}
stickyHeaderCustom : StickyConfig -> Attribute id msg
stickyHeaderCustom stickyConfig =
    Attribute (\config -> { config | stickyHeader = Just stickyConfig })


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
    , cellStyles : entry -> List Style
    }
    -> Column id entry msg
string { id, header, value, width, cellStyles } =
    Column
        { id = id
        , header = Html.text header
        , view = value >> Html.text
        , sorter = Just (simpleSort value)
        , width = width
        , cellStyles = cellStyles
        , hidden = False
        }


{-| -}
custom :
    { id : id
    , header : Html msg
    , view : entry -> Html msg
    , sorter : Maybe (Sorter entry)
    , width : Int
    , cellStyles : entry -> List Style
    }
    -> Column id entry msg
custom config =
    Column
        { id = config.id
        , header = config.header
        , view = config.view
        , sorter = config.sorter
        , width = config.width
        , cellStyles = config.cellStyles
        , hidden = False
        }


{-| Creates a placeholder column which will reserve the space for the column,
this can be used when multiple tables have similar data and we want to keep
the size consistent.
-}
placeholderColumn : { id : id, width : Int } -> Column id entry msg
placeholderColumn config =
    Column
        { id = config.id
        , header = Html.text ""
        , view = always (Html.text "")
        , sorter = Nothing
        , width = config.width
        , cellStyles = always []
        , hidden = True
        }


{-| Create a sorter function that always orders the entries in the same order.
For example, this is useful when we want to resolve ties and sort the tied
entries by name, no matter of the sort direction set on the table.
-}
invariantSort : (entry -> comparable) -> Sorter entry
invariantSort mapper =
    \_ elem1 elem2 ->
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
view : List (Attribute id msg) -> List (Column id entry msg) -> List entry -> Html msg
view attributes columns entries =
    let
        config =
            List.foldl (\(Attribute fn) soFar -> fn soFar) defaultConfig attributes

        tableColumns =
            List.map (buildTableColumn config.updateMsg config.state) columns
    in
    Table.view
        (buildTableAttributes config)
        tableColumns
        (case config.state of
            Just state_ ->
                List.sortWith
                    (findSorter columns state_.column state_.sortDirection)
                    entries

            Nothing ->
                entries
        )


{-| -}
viewLoading : List (Attribute id msg) -> List (Column id entry msg) -> Html msg
viewLoading attributes columns =
    let
        config =
            List.foldl (\(Attribute fn) soFar -> fn soFar) defaultConfig attributes

        tableColumns =
            List.map (buildTableColumn config.updateMsg config.state) columns
    in
    Table.viewLoading
        (buildTableAttributes config)
        tableColumns


buildTableAttributes : Config id msg -> List Table.Attribute
buildTableAttributes config =
    let
        stickyStyles =
            Maybe.map stickyConfigStyles config.stickyHeader
                |> Maybe.withDefault []
    in
    List.concat
        [ [ Table.css stickyStyles ]
        , List.reverse config.tableAttributes
        ]


findSorter : List (Column id entry msg) -> id -> Sorter entry
findSorter columns columnId =
    columns
        |> listExtraFind (\(Column column) -> column.id == columnId)
        |> Maybe.andThen (\(Column column) -> column.sorter)
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
    \_ _ _ ->
        EQ


buildTableColumn : Maybe (State id -> msg) -> Maybe (State id) -> Column id entry msg -> Table.Column entry msg
buildTableColumn maybeUpdateMsg maybeState (Column column) =
    if column.hidden then
        Table.placeholderColumn { width = Css.px (toFloat column.width) }

    else
        Table.custom
            { header =
                case maybeState of
                    Just state_ ->
                        viewSortHeader (column.sorter /= Nothing) column.header maybeUpdateMsg state_ column.id

                    Nothing ->
                        column.header
            , view = column.view
            , width = Css.px (toFloat column.width)
            , cellStyles = column.cellStyles
            , sort =
                Maybe.andThen
                    (\state_ ->
                        if state_.column == column.id then
                            Just state_.sortDirection

                        else
                            Nothing
                    )
                    maybeState
            }


viewSortHeader : Bool -> Html msg -> Maybe (State id -> msg) -> State id -> id -> Html msg
viewSortHeader isSortable header maybeUpdateMsg state_ id =
    let
        nextState =
            nextTableState state_ id
    in
    if isSortable then
        Html.button
            [ css
                [ Css.displayFlex
                , Css.alignItems Css.center
                , Css.property "gap" "8px"
                , CssVendorPrefix.property "user-select" "none"
                , if state_.column == id then
                    fontWeight bold

                  else
                    fontWeight normal
                , cursor pointer

                -- make this look less "buttony"
                , Css.border Css.zero
                , Css.backgroundColor Css.transparent
                , Css.width (Css.pct 100)
                , Css.height (Css.pct 100)
                , Css.margin Css.zero
                , Css.padding Css.zero
                , Fonts.baseFont
                , Css.fontSize (Css.em 1)
                ]
            , maybe (\updateMsg_ -> Html.Styled.Events.onClick (updateMsg_ nextState)) maybeUpdateMsg

            -- screen readers should know what clicking this button will do
            , Aria.roleDescription "sort button"
            ]
            [ Html.div [] [ header ]
            , viewJust (\_ -> viewSortButton state_ id) maybeUpdateMsg
            ]

    else
        Html.div
            [ css [ fontWeight normal ]
            ]
            [ header ]


viewSortButton : State id -> id -> Html msg
viewSortButton state_ id =
    let
        arrows upHighlighted downHighlighted =
            Html.div
                [ css
                    [ Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.alignItems Css.center
                    , Css.justifyContent Css.center
                    ]
                ]
                [ sortArrow Up upHighlighted
                , sortArrow Down downHighlighted
                ]

        buttonContent =
            case ( state_.column == id, state_.sortDirection ) of
                ( True, Ascending ) ->
                    arrows True False

                ( True, Descending ) ->
                    arrows False True

                ( False, _ ) ->
                    arrows False False
    in
    Html.div [ css [ padding (px 2) ] ] [ buttonContent ]


nextTableState : State id -> id -> State id
nextTableState state_ id =
    if state_.column == id then
        { column = id
        , sortDirection = flipSortDirection state_.sortDirection
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


sortArrow : Direction -> Bool -> Html msg
sortArrow direction active =
    let
        arrow =
            case direction of
                Up ->
                    Nri.Ui.UiIcon.V2.sortArrow

                Down ->
                    Nri.Ui.UiIcon.V2.sortArrowDown

        color =
            if active then
                Nri.Ui.Colors.V1.azure

            else
                Nri.Ui.Colors.V1.gray75
    in
    arrow
        |> Nri.Ui.Svg.V1.withHeight (px 6)
        |> Nri.Ui.Svg.V1.withWidth (px 8)
        |> Nri.Ui.Svg.V1.withColor color
        |> Nri.Ui.Svg.V1.withCss
            [ displayFlex
            , margin2 (px 1) zero
            ]
        |> Nri.Ui.Svg.V1.toHtml
