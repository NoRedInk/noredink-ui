module Nri.Ui.SortableTable.V6 exposing
    ( Model, init, initDescending, rebuild
    , entriesSorter
    , Msg, update
    , encode, decoder
    , Column, custom, string, placeholderColumn
    , Sorter, invariantSort, simpleSort, combineSorters
    , view
    , Attribute, model, msgWrapper, stickyHeader, stickyHeaderCustom, StickyConfig, tableAttribute
    )

{-| Changes from V5:

  - Moves update function out of onClick handler and into an explicit `update` function
  - Model is renamed Model
  - Model is is opaque (and because of this, exposes encoder and decoder) (todo: rename to Model)
  - performance: caches sorting in Model instead of performing it in the view.


## Initializing the model

@docs Model, init, initDescending, rebuild
@docs entriesSorter
@docs Msg, update


### Encoding and Decoding

@docs encode, decoder


## Columns

@docs Column, custom, string, placeholderColumn
@docs Sorter, invariantSort, simpleSort, combineSorters


## Rendering

@docs view


### Attributes

@docs Attribute, model, msgWrapper, stickyHeader, stickyHeaderCustom, StickyConfig, tableAttribute

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
import Nri.Ui.Html.Attributes.V2 exposing (maybe)
import Nri.Ui.Html.V3 exposing (viewJust)
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


{-| -}
type Model id entry
    = Model
        { column : id
        , sortDirection : SortDirection
        , entries : Dict.Dict ( id, SortDirection ) (List entry)
        , sorter : id -> Sorter entry
        }


{-| The function that was used to sort the current sort direction & column in the table.
-}
entriesSorter : Model id entry -> entry -> entry -> Order
entriesSorter (Model { sortDirection, column, sorter }) =
    sorter column sortDirection


type alias Config id entry msg =
    { msgWrapper : Maybe (Msg id -> msg)
    , model : Maybe (Model id entry)
    , stickyHeader : Maybe StickyConfig
    , tableAttributes : List Table.Attribute
    }


defaultConfig : Config id entry msg
defaultConfig =
    { msgWrapper = Nothing
    , model = Nothing
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
type Attribute id entry msg
    = Attribute (Config id entry msg -> Config id entry msg)


{-| Sort a column. You can get an initial model with `init` or `initDescending`.
If you make this sorting interactive, you should store the model in your model
and provide it to this function instead of recreating it on every update.
-}
model : Model id entry -> Attribute id entry msg
model model_ =
    Attribute (\config -> { config | model = Just model_ })


{-| Add interactivity in sorting columns. When this attribute is provided and
sorting is enabled, columns will be sortable by clicking the headers.
-}
msgWrapper : (Msg id -> msg) -> Attribute id entry msg
msgWrapper msgWrapper_ =
    Attribute (\config -> { config | msgWrapper = Just msgWrapper_ })


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


init_ : SortDirection -> id -> List (Column id entry msg) -> List entry -> Model id entry
init_ sortDirection columnId columns entries =
    let
        entriesSorter_ : id -> Sorter entry
        entriesSorter_ columnId_ =
            columns
                |> listExtraFind (\(Column column) -> column.id == columnId_)
                |> Maybe.andThen (\(Column column) -> column.sorter)
                |> Maybe.withDefault identitySorter

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
    Model
        { column = columnId
        , sortDirection = sortDirection
        , sorter = entriesSorter_
        , entries =
            Dict.singleton
                columnDirectionSorter
                ( columnId, sortDirection )
                (List.sortWith
                    (entriesSorter_ columnId sortDirection)
                    entries
                )
        }


{-| If you want to change the entries, this will rebuild the model while retaining sort information. Otherwise you can call one of the init funtions.
-}
rebuild : Model id entry -> List entry -> Model id entry
rebuild (Model model_) entries =
    Model
        { model_
            | entries =
                model_.entries
                    |> Dict.dropIf (\_ _ -> True)
                    |> Dict.insert
                        ( model_.column, model_.sortDirection )
                        (List.sortWith
                            (entriesSorter (Model model_))
                            entries
                        )
        }


{-| -}
init : id -> List (Column id entry msg) -> List entry -> Model id entry
init =
    init_ Ascending


{-| -}
initDescending : id -> List (Column id entry msg) -> List entry -> Model id entry
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
view : List (Attribute id entry msg) -> List (Column id entry msg) -> Html msg
view attributes columns =
    let
        config =
            List.foldl (\(Attribute fn) soFar -> fn soFar) defaultConfig attributes

        tableColumns =
            List.map (buildTableColumn config.msgWrapper config.model) columns
    in
    case config.model of
        Just model_ ->
            Table.view
                (buildTableAttributes config)
                tableColumns
                (currentEntries model_)

        Nothing ->
            Table.viewLoading
                (buildTableAttributes config)
                tableColumns


buildTableAttributes : Config id entry msg -> List Table.Attribute
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


buildTableColumn : Maybe (Msg id -> msg) -> Maybe (Model id entry) -> Column id entry msg -> Table.Column entry msg
buildTableColumn maybeMsgWrapper maybeModel (Column column) =
    if column.hidden then
        Table.placeholderColumn { width = Css.px (toFloat column.width) }

    else
        Table.custom
            { header =
                case maybeModel of
                    Just model_ ->
                        viewSortHeader (column.sorter /= Nothing) column.header maybeMsgWrapper model_ column.id

                    Nothing ->
                        column.header
            , view = column.view
            , width = Css.px (toFloat column.width)
            , cellStyles = column.cellStyles
            , sort =
                Maybe.andThen
                    (\(Model model_) ->
                        if model_.column == column.id then
                            Just model_.sortDirection

                        else
                            Nothing
                    )
                    maybeModel
            }


viewSortHeader : Bool -> Html msg -> Maybe (Msg id -> msg) -> Model id entry -> id -> Html msg
viewSortHeader isSortable header maybeMsgWrapper (Model model_) id =
    if isSortable then
        Html.button
            [ css
                [ Css.displayFlex
                , Css.alignItems Css.center
                , Css.property "gap" "8px"
                , CssVendorPrefix.property "user-select" "none"
                , if model_.column == id then
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
            , maybe (\msgWrapper_ -> Html.Styled.Events.onClick (msgWrapper_ (sortMsg (Model model_) id))) maybeMsgWrapper

            -- screen readers should know what clicking this button will do
            , Aria.roleDescription "sort button"
            ]
            [ Html.div [] [ header ]
            , viewJust (\_ -> viewSortButton (Model model_) id) maybeMsgWrapper
            ]

    else
        Html.div
            [ css [ fontWeight normal ]
            ]
            [ header ]


viewSortButton : Model id entry -> id -> Html msg
viewSortButton (Model model_) id =
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
            case ( model_.column == id, model_.sortDirection ) of
                ( True, Ascending ) ->
                    arrows True False

                ( True, Descending ) ->
                    arrows False True

                ( False, _ ) ->
                    arrows False False
    in
    Html.div [ css [ padding (px 2) ] ] [ buttonContent ]


sortMsg : Model id entry -> id -> Msg id
sortMsg (Model model_) id =
    Sort id
        (if model_.column == id then
            flipSortDirection model_.sortDirection

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


currentEntries : Model id entry -> List entry
currentEntries (Model model_) =
    Dict.get ( model_.column, model_.sortDirection ) model_.entries
        |> Maybe.withDefault []


{-| -}
update : Msg id -> Model id entry -> Model id entry
update msg (Model model_) =
    case msg of
        Sort column sortDirection ->
            let
                entries =
                    Dict.update
                        ( column, sortDirection )
                        (Maybe.Extra.withDefaultLazy
                            -- we only insert if the sorted data isn't already cached
                            (\() ->
                                List.sortWith
                                    (model_.sorter column sortDirection)
                                    (currentEntries (Model model_))
                            )
                            >> Just
                        )
                        model_.entries
            in
            Model
                { model_
                    | column = column
                    , sortDirection = sortDirection
                    , entries = entries
                }


{-| encode model to Json
-}
encode : (id -> Encode.Value) -> Model id entry -> Encode.Value
encode columnIdEncoder (Model model_) =
    Encode.object
        [ ( "column", columnIdEncoder model_.column )
        , ( "sortDirectionAscending", Encode.bool (model_.sortDirection == Ascending) )
        ]


{-| decode model from Json
-}
decoder : Decode.Decoder id -> List (Column id entry msg) -> List entry -> Decode.Decoder (Model id entry)
decoder columnIdDecoder columns entries =
    Decode.map2
        (\column sortDirectionAscending ->
            init_
                (if sortDirectionAscending then
                    Ascending

                 else
                    Descending
                )
                column
                columns
                entries
        )
        (Decode.field "column" columnIdDecoder)
        (Decode.field "sortDirectionAscending" Decode.bool)
