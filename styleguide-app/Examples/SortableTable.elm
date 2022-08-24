module Examples.SortableTable exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Css exposing (..)
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.SortableTable.V3 as SortableTable
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Table.V6 as Table
import Nri.Ui.UiIcon.V1 as UiIcon


moduleName : String
moduleName =
    "SortableTable"


version : Int
version =
    3


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

                config =
                    { updateMsg = SetSortState
                    , columns =
                        [ SortableTable.string
                            { id = FirstName
                            , header = "First name"
                            , value = .firstName
                            , width = 125
                            , cellStyles = \_ -> []
                            }
                        , SortableTable.string
                            { id = LastName
                            , header = "Last name"
                            , value = .lastName
                            , width = 125
                            , cellStyles = \_ -> []
                            }
                        , SortableTable.custom
                            { id = CustomExample
                            , header = Html.text settings.customizableColumnName
                            , view = .grade >> String.fromInt >> Html.text
                            , sorter =
                                if settings.customizableColumnSorter then
                                    Just (SortableTable.simpleSort .grade)

                                else
                                    Nothing
                            , width = 125
                            , cellStyles = \_ -> []
                            }
                        ]
                    }

                data =
                    [ { firstName = "First1", lastName = "Last1", grade = 100 }
                    , { firstName = "First2", lastName = "Last2", grade = 89 }
                    , { firstName = "First3", lastName = "Last3", grade = 64 }
                    , { firstName = "First4", lastName = "Last4", grade = 89 }
                    , { firstName = "First5", lastName = "Last5", grade = 97 }
                    ]
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControls
                , settings = model.settings
                , mainType = Nothing
                , extraCode = []
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "view"
                          , code =
                                """TODO
                        """
                          }
                        ]
                }
            , Heading.h2 [ Heading.plaintext "Example" ]
            , if settings.loading then
                SortableTable.viewLoading config sortState

              else
                SortableTable.view config sortState data
            ]
    }


{-| -}
type alias State =
    { sortState : SortableTable.State Column
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
    , loading : Bool
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "Customizable column name" (Control.string "Grade")
        |> Control.field "Customizable column sorter" (Control.bool True)
        |> Control.field "Is loading" (Control.bool False)


type Column
    = FirstName
    | LastName
    | CustomExample


{-| -}
type Msg
    = SetSortState (SortableTable.State Column)
    | UpdateControls (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetSortState sortState ->
            ( { state | sortState = sortState }, Cmd.none )

        UpdateControls controls ->
            ( { state | settings = controls }, Cmd.none )
