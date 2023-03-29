module Examples.SortableTable exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Code
import Css exposing (..)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.SortableTable.V4 as SortableTable exposing (Column)
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Table.V6 as Table
import Nri.Ui.UiIcon.V1 as UiIcon


moduleName : String
moduleName =
    "SortableTable"


version : Int
version =
    4


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Layout ]
    , keyboardSupport = []
    , state = init
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
        [ Table.view
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
    , view =
        \ellieLinkConfig ({ sortState } as model) ->
            let
                settings =
                    Control.currentValue model.settings

                attrs =
                    [ SortableTable.updateMsg SetSortState
                    , SortableTable.state sortState
                    ]

                ( dataCode, data ) =
                    List.unzip dataWithCode

                ( columnsCode, columns ) =
                    List.unzip (columnsWithCode settings)

                toExampleCode viewName finalArgs =
                    { sectionName = viewName
                    , code =
                        [ moduleName ++ "." ++ viewName
                        , Code.listMultiline
                            [ "SortableTable.updateMsg SetSortState"
                            , "-- The SortableTable's state should be stored on the model, rather than initialized in the view"
                                ++ "\n      "
                                ++ "SortableTable.state (SortableTable.init "
                                ++ Debug.toString model.sortState.column
                                ++ ")"
                            ]
                            1
                        , Code.listMultiline columnsCode 1
                        , finalArgs
                        ]
                            |> String.join ""
                    }
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
                    , "type Msg = SetSortState (SortableTable.State ColumnId)"
                    ]
                , renderExample = Code.unstyledView
                , toExampleCode = \_ -> [ toExampleCode "view" (Code.list dataCode), toExampleCode "viewLoading" "" ]
                }
            , Heading.h2 [ Heading.plaintext "Example" ]
            , if settings.loading then
                SortableTable.viewLoading attrs columns

              else
                SortableTable.view attrs columns data
            ]
    }


columnsWithCode : Settings -> List ( String, Column ColumnId Datum Msg )
columnsWithCode settings =
    [ ( "SortableTable.string"
            ++ Code.recordMultiline
                [ ( "id", "FirstName" )
                , ( "header", Code.string "First name" )
                , ( "value", ".firstName" )
                , ( "width", "125" )
                , ( "cellStyles", Code.always "[]" )
                ]
                3
      , SortableTable.string
            { id = FirstName
            , header = "First name"
            , value = .firstName
            , width = 125
            , cellStyles = \_ -> []
            }
      )
    , ( "SortableTable.string"
            ++ Code.recordMultiline
                [ ( "id", "LastName" )
                , ( "header", Code.string "Last name" )
                , ( "value", ".lastName" )
                , ( "width", "125" )
                , ( "cellStyles", Code.always "[]" )
                ]
                3
      , SortableTable.string
            { id = LastName
            , header = "Last name"
            , value = .lastName
            , width = 125
            , cellStyles = \_ -> []
            }
      )
    , ( "SortableTable.custom"
            ++ Code.recordMultiline
                [ ( "id", "CustomExample" )
                , ( "header", "text" ++ Code.string settings.customizableColumnName )
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
                3
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
    { sortState : SortableTable.State ColumnId
    , settings : Control Settings
    }


{-| -}
init : State
init =
    { sortState = SortableTable.init FirstName
    , settings = controlSettings
    }


type alias Settings =
    { customizableColumnName : String
    , customizableColumnSorter : Bool
    , customizableColumnWidth : Int
    , customizableColumnCellStyles : ( String, List Style )
    , loading : Bool
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "Customizable column name" (Control.string "Grade")
        |> Control.field "Customizable column sorter" (Control.bool True)
        |> Control.field "Customizable column width" (ControlExtra.int 10)
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


type ColumnId
    = FirstName
    | LastName
    | CustomExample


{-| -}
type Msg
    = SetSortState (SortableTable.State ColumnId)
    | UpdateControls (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetSortState sortState ->
            ( { state | sortState = sortState }, Cmd.none )

        UpdateControls controls ->
            ( { state | settings = controls }, Cmd.none )
