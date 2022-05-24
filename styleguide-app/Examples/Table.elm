module Examples.Table exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled exposing (..)
import Category exposing (Category(..))
import Css exposing (..)
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Table.V5 as Table exposing (Column)


{-| -}
type alias State =
    Control Settings


moduleName : String
moduleName =
    "Table"


version : Int
version =
    5


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = controlSettings
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Layout ]
    , keyboardSupport = []
    , preview =
        [ Table.view
            [ Table.string
                { header = "A"
                , value = .a
                , width = px 50
                , cellStyles = always []
                }
            , Table.string
                { header = "B"
                , value = .b
                , width = px 50
                , cellStyles = always []
                }
            ]
            [ { a = "Row 1 A"
              , b = "Row 1 B"
              }
            , { a = "Row 2 A"
              , b = "Row 2 B"
              }
            ]
        ]
    , view =
        \ellieLinkConfig state ->
            let
                columns =
                    [ Table.string
                        { header = "First Name"
                        , value = .firstName
                        , width = calc (pct 50) minus (px 250)
                        , cellStyles = always []
                        }
                    , Table.string
                        { header = "Last Name"
                        , value = .lastName
                        , width = calc (pct 50) minus (px 250)
                        , cellStyles = always []
                        }
                    , Table.string
                        { header = "Submitted"
                        , value = .submitted >> String.fromInt
                        , width = px 125
                        , cellStyles = \value -> [ textAlign center ]
                        }
                    , Table.custom
                        { header = text "Actions"
                        , width = px 250
                        , view = \_ -> Button.button "Action" [ Button.small, Button.onClick (ConsoleLog "Clicked button!") ]
                        , cellStyles = always []
                        }
                    ]

                { showHeader, isLoading } =
                    Control.currentValue state
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state
                , mainType = "RootHtml.Html msg"
                , extraImports = []
                , toExampleCode =
                    \settings ->
                        let
                            toExampleCode viewName =
                                { sectionName = moduleName ++ "." ++ viewName
                                , code =
                                    (moduleName ++ "." ++ viewName)
                                        ++ "[ --TODO \n ]"
                                        ++ "[ --TODO \n ]"
                                }
                        in
                        List.map toExampleCode [ "view", "viewWithoutHeader", "viewLoading", "viewLoadingWithoutHeader" ]
                }
            , Heading.h2 [ Heading.style Heading.Subhead ] [ text "Example" ]
            , case ( showHeader, isLoading ) of
                ( True, False ) ->
                    Table.view columns data

                ( False, False ) ->
                    Table.viewWithoutHeader columns data

                ( True, True ) ->
                    Table.viewLoading columns

                ( False, True ) ->
                    Table.viewLoadingWithoutHeader columns
            ]
    }


{-| -}
type Msg
    = UpdateControl (Control Settings)
    | ConsoleLog String


update : Msg -> State -> ( State, Cmd msg )
update msg state =
    case msg of
        UpdateControl control ->
            ( control, Cmd.none )

        ConsoleLog message ->
            ( Debug.log "Menu Example" message |> always state, Cmd.none )


type alias Settings =
    { showHeader : Bool
    , isLoading : Bool
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "visible header" (Control.bool True)
        |> Control.field "is loading" (Control.bool False)


type alias Data =
    { firstName : String
    , lastName : String
    , submitted : Int
    }


data : List Data
data =
    [ { firstName = "First1", lastName = "Last1", submitted = 10 }
    , { firstName = "First2", lastName = "Last2", submitted = 0 }
    , { firstName = "First3", lastName = "Last3", submitted = 3 }
    , { firstName = "First4", lastName = "Last4", submitted = 15 }
    , { firstName = "First5", lastName = "Last5", submitted = 8 }
    ]
