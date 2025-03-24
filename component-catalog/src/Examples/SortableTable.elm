module Examples.SortableTable exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Code
import Css exposing (..)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra exposing (values)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.SortableTable.V6 as SortableTable exposing (Column)
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Table.V8 as Table
import Nri.Ui.UiIcon.V1 as UiIcon


moduleName : String
moduleName =
    "SortableTable"


version : Int
version =
    6


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Layout ]
    , keyboardSupport = []
    , init = ( init, Cmd.none )
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        let
            header name =
                div
                    [ css
                        [ Css.displayFlex
                        , Css.justifyContent Css.spaceBetween
                        , Css.alignItems Css.center
                        ]
                    ]
                    [ text name
                    , div
                        [ css
                            [ Css.displayFlex
                            , Css.flexDirection Css.column
                            , Css.marginTop (Css.px -4)
                            ]
                        ]
                        [ renderPreviewArrow UiIcon.sortArrow
                        , renderPreviewArrow UiIcon.sortArrowDown
                        ]
                    ]

            renderPreviewArrow : Svg -> Html msg
            renderPreviewArrow arrow =
                arrow
                    |> Svg.withColor Colors.gray75
                    |> Svg.withWidth (Css.px 12)
                    |> Svg.withHeight (Css.px 12)
                    |> Svg.toHtml
        in
        [ Table.view []
            [ Table.custom
                { header = header "X"
                , view = .x >> Html.text
                , width = px 50
                , cellStyles = always []
                , sort = Nothing
                }
            , Table.custom
                { header = header "Y"
                , view = .y >> Html.text
                , width = px 50
                , cellStyles = always []
                , sort = Nothing
                }
            ]
            [ { x = "Row 1 X"
              , y = "Row 1 Y"
              }
            , { x = "Row 2 X"
              , y = "Row 2 Y"
              }
            ]
        ]
    , about = []
    , view =
        \ellieLinkConfig ({ sortModel } as model) ->
            let
                settings =
                    Control.currentValue model.settings

                ( viewCode, view ) =
                    viewWithCode model
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControls
                , settings = model.settings
                , mainType = Just "RootHtml.Html Msg"
                , extraCode =
                    [ "type ColumnId = FirstName | LastName | CustomExample "
                    , "type Msg = SortableTableWrapper (SortableTable.Msg ColumnId)"
                    ]
                , renderExample = identity
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "view"
                          , code =
                                [ Code.browserElement
                                    { init =
                                        Code.newlineWithIndent 2
                                            ++ Code.always
                                                (Code.tupleMultiline
                                                    (initWithCode settings |> codeWithIndent 4)
                                                    "Cmd.none"
                                                    3
                                                )
                                    , update =
                                        Code.newlineWithIndent 2
                                            ++ Code.anonymousFunction
                                                "msg model"
                                                (Code.tuple
                                                    (Code.fromModule moduleName "update msg model")
                                                    "Cmd.none"
                                                )
                                    , view =
                                        Code.newlineWithIndent 2 ++ Code.anonymousFunction "model " (viewCode 2)
                                    , subscriptions = Code.always "Sub.none"
                                    }
                                , Code.var "columns" 1 <|
                                    Code.listMultilineFlat (columnsWithCode settings |> codeWithIndent 1) 1
                                ]
                                    |> String.join Code.newlines
                          }
                        ]
                }
            , Heading.h2
                [ Heading.plaintext "Customizable Example"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , view
            ]
    }


viewWithCode : State -> ( Int -> String, Html Msg )
viewWithCode ({ sortModel } as model) =
    let
        settings =
            Control.currentValue model.settings
    in
    ( \indentOffset ->
        ([ Code.fromModule moduleName "view"
         , Code.listMultilineFlat
            (List.concat
                [ case settings.stickyHeader of
                    Nothing ->
                        []

                    Just Default ->
                        [ "SortableTable.stickyHeader"
                        ]

                    Just (Custom stickyConfig) ->
                        [ "SortableTable.stickyHeaderCustom "
                            ++ Code.recordMultiline
                                [ ( "topOffset", String.fromFloat stickyConfig.topOffset )
                                , ( "zIndex", String.fromInt stickyConfig.zIndex )
                                , ( "pageBackgroundColor", "Css.hex \"" ++ stickyConfig.pageBackgroundColor.value ++ "\"" )
                                , ( "customZIndex"
                                  , case stickyConfig.hoverZIndex of
                                        Nothing ->
                                            "Nothing"

                                        Just zIndex ->
                                            "Just " ++ String.fromInt zIndex
                                  )
                                ]
                                (indentOffset + 3)
                        ]
                , if settings.forwardingAttributesToTable then
                    [ "SortableTable.tableAttribute (Table.disableAlternatingRowColors)"
                    , "SortableTable.tableAttribute (Table.css [ Css.border3 (Css.px 1) Css.solid Colors.red ])"
                    ]

                  else
                    []
                , [ "SortableTable.msgWrapper identity" ]
                , if settings.loading then
                    []

                  else
                    [ "SortableTable.model model" ]
                ]
            )
            (indentOffset + 2)
         , "columns"
         ]
            |> String.join (Code.newlineWithIndent (indentOffset + 2))
        )
            |> Code.unstyledViewWithIndent (indentOffset + 1)
    , SortableTable.view
        (List.concat
            [ SortableTable.msgWrapper SortableTableMsg
                :: (if settings.loading then
                        []

                    else
                        [ SortableTable.model sortModel ]
                   )
            , case settings.stickyHeader of
                Nothing ->
                    []

                Just Default ->
                    [ SortableTable.stickyHeader ]

                Just (Custom customConfig) ->
                    [ SortableTable.stickyHeaderCustom customConfig ]
            , if settings.forwardingAttributesToTable then
                [ SortableTable.tableAttribute Table.disableAlternatingRowColors
                , SortableTable.tableAttribute (Table.css [ Css.border3 (Css.px 4) Css.solid Colors.red ])
                ]

              else
                []
            ]
        )
        (columnsWithCode settings |> Tuple.second)
    )


initWithCode : Settings -> ( Int -> String, SortableTable.Model ColumnId Datum )
initWithCode settings =
    let
        ( dataCode, data ) =
            List.unzip dataWithCode
    in
    ( \indentOffset ->
        [ Code.fromModule moduleName "init"
        , "FirstName"
        , "columns"
        , Code.listMultilineFlat dataCode indentOffset
        ]
            |> String.join (Code.newlineWithIndent indentOffset)
    , SortableTable.init FirstName (columnsWithCode settings |> Tuple.second) data
    )


columnsWithCode : Settings -> ( Int -> List String, List (Column ColumnId Datum Msg) )
columnsWithCode settings =
    let
        ( indentToCodes, columns ) =
            [ ( \indent ->
                    "SortableTable.string"
                        ++ Code.recordMultiline
                            [ ( "id", "FirstName" )
                            , ( "header", Code.string "First name" )
                            , ( "value", ".firstName" )
                            , ( "width", "125" )
                            , ( "cellStyles", Code.always "[]" )
                            ]
                            indent
              , SortableTable.string
                    { id = FirstName
                    , header = "First name"
                    , value = .firstName
                    , width = 125
                    , cellStyles = \_ -> []
                    }
              )
            , ( \indent ->
                    "SortableTable.string"
                        ++ Code.recordMultiline
                            [ ( "id", "LastName" )
                            , ( "header", Code.string "Last name" )
                            , ( "value", ".lastName" )
                            , ( "width", "125" )
                            , ( "cellStyles", Code.always "[]" )
                            ]
                            indent
              , SortableTable.string
                    { id = LastName
                    , header = "Last name"
                    , value = .lastName
                    , width = 125
                    , cellStyles = \_ -> []
                    }
              )
            , ( \indent ->
                    "SortableTable.custom"
                        ++ Code.recordMultiline
                            [ ( "id", "CustomExample" )
                            , ( "header", "text " ++ Code.string settings.customizableColumnName )
                            , ( "view", ".grade >> String.fromInt >> text" )
                            , ( "sorter"
                              , if settings.customizableColumnSorter then
                                    "Just (SortableTable.simpleSort .grade)"

                                else
                                    "Nothing"
                              )
                            , ( "width", String.fromInt settings.customizableColumnWidth )
                            , ( "cellStyles", Code.always (Tuple.first settings.customizableColumnCellStyles) )
                            ]
                            indent
              , SortableTable.custom
                    { id = CustomExample
                    , header = Html.text settings.customizableColumnName
                    , view = .grade >> String.fromInt >> Html.text
                    , sorter =
                        if settings.customizableColumnSorter then
                            Just (SortableTable.simpleSort .grade)

                        else
                            Nothing
                    , width = settings.customizableColumnWidth
                    , cellStyles = \_ -> Tuple.second settings.customizableColumnCellStyles
                    }
              )
            ]
                |> List.unzip
    in
    ( \indentOffset ->
        indentToCodes
            |> List.map (\indentToCode -> indentToCode (indentOffset + 1))
    , columns
    )


type alias Datum =
    { firstName : String
    , lastName : String
    , grade : Int
    }


dataWithCode : List ( String, Datum )
dataWithCode =
    [ ( Code.record [ ( "firstName", Code.string "First1" ), ( "lastName", Code.string "Last1" ), ( "grade", "100" ) ]
      , { firstName = "First1", lastName = "Last1", grade = 100 }
      )
    , ( Code.record [ ( "firstName", Code.string "First2" ), ( "lastName", Code.string "Last2" ), ( "grade", "89" ) ]
      , { firstName = "First2", lastName = "Last2", grade = 89 }
      )
    , ( Code.record [ ( "firstName", Code.string "First3" ), ( "lastName", Code.string "Last3" ), ( "grade", "64" ) ]
      , { firstName = "First3", lastName = "Last3", grade = 64 }
      )
    , ( Code.record [ ( "firstName", Code.string "First4" ), ( "lastName", Code.string "Last4" ), ( "grade", "89" ) ]
      , { firstName = "First4", lastName = "Last4", grade = 89 }
      )
    , ( Code.record [ ( "firstName", Code.string "First5" ), ( "lastName", Code.string "Last5" ), ( "grade", "97" ) ]
      , { firstName = "First5", lastName = "Last5", grade = 97 }
      )
    ]


{-| -}
type alias State =
    { sortModel : SortableTable.Model ColumnId Datum
    , settings : Control Settings
    }


{-| -}
init : State
init =
    { sortModel = initWithCode (Control.currentValue controlSettings) |> Tuple.second
    , settings = controlSettings
    }


type alias Settings =
    { customizableColumnName : String
    , customizableColumnSorter : Bool
    , customizableColumnWidth : Int
    , customizableColumnCellStyles : ( String, List Style )
    , loading : Bool
    , stickyHeader : Maybe StickyHeader
    , forwardingAttributesToTable : Bool
    }


type StickyHeader
    = Default
    | Custom SortableTable.StickyConfig


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "Customizable column name" (Control.string "Grade")
        |> Control.field "Customizable column sorter" (Control.bool True)
        |> Control.field "Customizable column width" (Control.int 10)
        |> Control.field "Customizable column cell styles"
            (Control.choice
                [ ( "[]", Control.value ( "[]", [] ) )
                , ( "red dashed border"
                  , Control.value
                        ( "[ Css.border3 (Css.px 4) Css.dashed Colors.red ]"
                        , [ Css.border3 (Css.px 4) Css.dashed Colors.red ]
                        )
                  )
                ]
            )
        |> Control.field "Is loading" (Control.bool False)
        |> Control.field "Header"
            (Control.maybe False
                (Control.choice
                    [ ( "Default", Control.value Default )
                    , ( "Custom"
                      , Control.record SortableTable.StickyConfig
                            |> Control.field "topOffset" (values String.fromFloat [ 0, 10, 50 ])
                            |> Control.field "zIndex" (values String.fromInt [ 0, 1, 5, 10 ])
                            |> Control.field "pageBackgroundColor"
                                (Control.choice
                                    [ ( "white", Control.value Colors.white )
                                    , ( "gray", Control.value Colors.gray92 )
                                    ]
                                )
                            |> Control.field "hoverZIndex" (Control.maybe False (values String.fromInt [ 0, 1, 5, 10 ]))
                            |> Control.map Custom
                      )
                    ]
                )
                |> Control.revealed "Sticky Header"
            )
        |> Control.field "Using Nri.Ui.Table attributes" (Control.bool False)


type ColumnId
    = FirstName
    | LastName
    | CustomExample


{-| -}
type Msg
    = SortableTableMsg (SortableTable.Msg ColumnId)
    | UpdateControls (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SortableTableMsg sortableTableMsg ->
            ( { state | sortModel = SortableTable.update sortableTableMsg state.sortModel }, Cmd.none )

        UpdateControls controls ->
            let
                sortModel =
                    state.sortModel

                ( _, data ) =
                    List.unzip dataWithCode
            in
            ( { state
                | settings = controls
                , sortModel =
                    SortableTable.rebuild
                        sortModel
                        (if (Control.currentValue controls).stickyHeader /= Nothing then
                            data
                                |> List.repeat 10
                                |> List.concat

                         else
                            data
                        )
              }
            , Cmd.none
            )


codeWithIndent : Int -> ( Int -> a, b ) -> a
codeWithIndent indentOffset ( withIndent, _ ) =
    withIndent indentOffset
