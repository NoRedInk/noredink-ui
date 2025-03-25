module Nri.Ui.SortableTable.V6 exposing
    ( Model
    , currentEntriesSorter
    , Msg
    , encode
    , Column, custom, string, placeholderColumn
    , Sorter, invariantSort, simpleSort, combineSorters
    , ViewConfig
    , Attribute, stickyHeader, stickyHeaderCustom, StickyConfig, tableAttribute
    , table, SortableTable
    )

{-| Changes from V5:

  - Moves update function out of onClick handler and into an explicit `update` function
  - State is renamed Model
  - Model is is opaque (and because of this, exposes encoder, decoder, and entriesSorter)
  - removed loading function. Now if view is not passed the model, it will render as loading
  - performance: caches sorting in Model instead of performing it in the view.
  - move msgWrapper and model attributes into required param for view


## Initializing the model

@docs Model
@docs entriesSorter, currentEntriesSorter
@docs Msg, update


### Encoding and Decoding

@docs encode, decoder


## Columns

@docs Column, custom, string, placeholderColumn
@docs Sorter, invariantSort, simpleSort, combineSorters


## Rendering

@docs view, ViewConfig


### Attributes

@docs Attribute, stickyHeader, stickyHeaderCustom, StickyConfig, tableAttribute

@docs table, SortableTable

-}

import Accessibility.Styled.Aria as Aria
import Css exposing (..)
import Css.Global
import Css.Media
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Maybe.Extra
import Nri.Ui.Colors.V1
import Nri.Ui.CssVendorPrefix.V1 as CssVendorPrefix
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Svg.V1
import Nri.Ui.Table.V8 as Table exposing (SortDirection(..))
import Nri.Ui.UiIcon.V1
import Sort
import Sort.Dict as Dict


{-| -}
type Msg id
    = Sort id SortDirection


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


type alias TableConfig id entry msg =
    List (Column id entry msg)


type LoadingEntries id entry
    = Loaded (Sort.Sorter ( id, SortDirection )) (Dict.Dict ( id, SortDirection ) (List entry))
    | Loading (Sort.Sorter ( id, SortDirection ))


{-| -}
type Model id entry
    = Model
        { column : id
        , sortDirection : SortDirection
        , loadingEntries : LoadingEntries id entry
        }


{-| -}
entriesSorter : TableConfig id entry msg -> id -> Sorter entry
entriesSorter columns id =
    columns
        |> listExtraFind (\(Column column) -> column.id == id)
        |> Maybe.andThen (\(Column column) -> column.sorter)
        |> Maybe.withDefault identitySorter


{-| -}
currentEntriesSorter : TableConfig id entry msg -> Model id entry -> entry -> entry -> Order
currentEntriesSorter columns (Model { sortDirection, column }) =
    entriesSorter columns column sortDirection


{-| -}
type alias ViewConfig id entry msg =
    { msgWrapper : Msg id -> msg
    , model : Model id entry
    }


type alias Config =
    { stickyHeader : Maybe StickyConfig
    , tableAttributes : List Table.Attribute
    }


defaultConfig : Config
defaultConfig =
    { stickyHeader = Nothing
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
type Attribute id entry msg
    = Attribute (Config -> Config)


{-| Make the header sticky (that is, it will stick to the top of the viewport
when it otherwise would have been scrolled off.) You probably will want to set a
background color on the header as well.
-}
stickyHeader : Attribute id entry msg
stickyHeader =
    Attribute (\config -> { config | stickyHeader = Just defaultStickyConfig })


{-| Attributes forwarded to the underlying Nri.Ui.Table
-}
tableAttribute : Table.Attribute -> Attribute id entry msg
tableAttribute attr =
    Attribute (\config -> { config | tableAttributes = attr :: config.tableAttributes })


{-| Does the same thing as `stickyHeader`, but with adaptations for your
specific use.
-}
stickyHeaderCustom : StickyConfig -> Attribute id entry msg
stickyHeaderCustom stickyConfig =
    Attribute (\config -> { config | stickyHeader = Just stickyConfig })


init_ : SortDirection -> TableConfig id entry msg -> id -> Maybe (List entry) -> Model id entry
init_ sortDirection columns columnId maybeEntries =
    let
        directionOrder : SortDirection -> Int
        directionOrder direction =
            case direction of
                Ascending ->
                    0

                Descending ->
                    1

        -- we could pass this ordering in, but not having to makes the API a little nicer,
        -- and if the column id isn't in one of the columns, it doesn't need to have an ordering anyways
        columnOrder : id -> Int
        columnOrder id =
            List.Extra.findIndex (\(Column c) -> c.id == id) columns
                |> Maybe.withDefault -1

        columnDirectionSorter : Sort.Sorter ( id, SortDirection )
        columnDirectionSorter =
            Sort.by (Tuple.first >> columnOrder) Sort.increasing
                |> Sort.tiebreaker
                    (Sort.by (Tuple.second >> directionOrder) Sort.increasing)
    in
    rebuild
        columns
        (Model
            { column = columnId
            , sortDirection = sortDirection
            , loadingEntries = Loading columnDirectionSorter
            }
        )
        maybeEntries


{-| If you want to change the entries, this will rebuild the model while retaining sort information. Otherwise you can call one of the init funtions.
-}
rebuild : TableConfig id entry msg -> Model id entry -> Maybe (List entry) -> Model id entry
rebuild tableConfig (Model model) maybeEntries =
    let
        dictSorter =
            case model.loadingEntries of
                Loaded sorter_ _ ->
                    sorter_

                Loading sorter_ ->
                    sorter_
    in
    Model
        { model
            | loadingEntries =
                case maybeEntries of
                    Just entries ->
                        Loaded dictSorter <|
                            Dict.singleton
                                dictSorter
                                ( model.column, model.sortDirection )
                                (List.sortWith
                                    (currentEntriesSorter tableConfig (Model model))
                                    entries
                                )

                    Nothing ->
                        Loading dictSorter
        }


{-| -}
init : TableConfig id entry msg -> id -> Maybe (List entry) -> Model id entry
init =
    init_ Ascending


{-| -}
initDescending : TableConfig id entry msg -> id -> Maybe (List entry) -> Model id entry
initDescending =
    init_ Descending


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
view : TableConfig id entry msg -> ViewConfig id entry msg -> List (Attribute id entry msg) -> Html msg
view columns { msgWrapper, model } attributes =
    let
        config =
            List.foldl (\(Attribute fn) soFar -> fn soFar) defaultConfig attributes

        tableColumns =
            List.map (buildTableColumn msgWrapper model) columns

        tableAttributes =
            buildTableAttributes config

        loadingEntries =
            case model of
                Model model_ ->
                    model_.loadingEntries
    in
    case loadingEntries of
        Loading _ ->
            Table.viewLoading
                tableAttributes
                tableColumns

        Loaded _ entries ->
            Table.view
                tableAttributes
                tableColumns
                (currentEntries model entries)


buildTableAttributes : Config -> List Table.Attribute
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


buildTableColumn : (Msg id -> msg) -> Model id entry -> Column id entry msg -> Table.Column entry msg
buildTableColumn msgWrapper (Model model) (Column column) =
    if column.hidden then
        Table.placeholderColumn { width = Css.px (toFloat column.width) }

    else
        Table.custom
            { header =
                viewSortHeader (column.sorter /= Nothing) column.header msgWrapper (Model model) column.id
            , view = column.view
            , width = Css.px (toFloat column.width)
            , cellStyles = column.cellStyles
            , sort =
                if model.column == column.id then
                    Just model.sortDirection

                else
                    Nothing
            }


viewSortHeader : Bool -> Html msg -> (Msg id -> msg) -> Model id entry -> id -> Html msg
viewSortHeader isSortable header msgWrapper (Model model) id =
    if isSortable then
        Html.button
            [ css
                [ Css.displayFlex
                , Css.alignItems Css.center
                , Css.property "gap" "8px"
                , CssVendorPrefix.property "user-select" "none"
                , if model.column == id then
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
            , Html.Styled.Events.onClick (msgWrapper (sortMsg (Model model) id))

            -- screen readers should know what clicking this button will do
            , Aria.roleDescription "sort button"
            ]
            [ Html.div [] [ header ]
            , viewSortButton (Model model) id
            ]

    else
        Html.div
            [ css [ fontWeight normal ]
            ]
            [ header ]


viewSortButton : Model id entry -> id -> Html msg
viewSortButton (Model model) id =
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
            case ( model.column == id, model.sortDirection ) of
                ( True, Ascending ) ->
                    arrows True False

                ( True, Descending ) ->
                    arrows False True

                ( False, _ ) ->
                    arrows False False
    in
    Html.div [ css [ padding (px 2) ] ] [ buttonContent ]


sortMsg : Model id entry -> id -> Msg id
sortMsg (Model model) id =
    Sort id
        (if model.column == id then
            flipSortDirection model.sortDirection

         else
            Ascending
        )


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
                    Nri.Ui.UiIcon.V1.sortArrow

                Down ->
                    Nri.Ui.UiIcon.V1.sortArrowDown

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


currentEntries : Model id entry -> Dict.Dict ( id, SortDirection ) (List entry) -> List entry
currentEntries (Model model) entries =
    Dict.get ( model.column, model.sortDirection ) entries
        |> Maybe.withDefault []


{-| -}
update : TableConfig id entry msg -> Msg id -> Model id entry -> Model id entry
update tableConfig msg (Model model) =
    case msg of
        Sort column sortDirection ->
            Model
                { model
                    | column = column
                    , sortDirection = sortDirection
                    , loadingEntries =
                        case model.loadingEntries of
                            Loading sorter ->
                                Loading sorter

                            Loaded sorter entries ->
                                Loaded sorter <|
                                    Dict.update
                                        ( column, sortDirection )
                                        (Maybe.Extra.withDefaultLazy
                                            -- we only insert if the sorted data isn't already cached
                                            (\() ->
                                                List.sortWith
                                                    (entriesSorter tableConfig column sortDirection)
                                                    (currentEntries (Model model) entries)
                                            )
                                            >> Just
                                        )
                                        entries
                }


{-| encode model to Json
-}
encode : (id -> Encode.Value) -> Model id entry -> Encode.Value
encode columnIdEncoder (Model model) =
    Encode.object
        [ ( "column", columnIdEncoder model.column )
        , ( "sortDirectionAscending", Encode.bool (model.sortDirection == Ascending) )
        ]


{-| decode model from Json
-}
decoder : TableConfig id entry msg -> Decode.Decoder id -> Maybe (List entry) -> Decode.Decoder (Model id entry)
decoder columns columnIdDecoder maybeEntries =
    Decode.map2
        (\column sortDirectionAscending ->
            init_
                (if sortDirectionAscending then
                    Ascending

                 else
                    Descending
                )
                columns
                column
                maybeEntries
        )
        (Decode.field "column" columnIdDecoder)
        (Decode.field "sortDirectionAscending" Decode.bool)


type alias SortableTable id entry msg =
    { update : Msg id -> Model id entry -> Model id entry
    , view : ViewConfig id entry msg -> List (Attribute id entry msg) -> Html msg
    , decoder : Decode.Decoder id -> Maybe (List entry) -> Decode.Decoder (Model id entry)
    , currentEntriesSorter : Model id entry -> entry -> entry -> Order
    , rebuild : Model id entry -> Maybe (List entry) -> Model id entry
    , init : id -> Maybe (List entry) -> Model id entry
    , initDescending : id -> Maybe (List entry) -> Model id entry
    }


table :
    TableConfig id entry msg
    -> SortableTable id entry msg
table tableConfig =
    { update = update tableConfig
    , view = view tableConfig
    , decoder = decoder tableConfig
    , currentEntriesSorter = currentEntriesSorter tableConfig
    , rebuild = rebuild tableConfig
    , init = init tableConfig
    , initDescending = initDescending tableConfig
    }
