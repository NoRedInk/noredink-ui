module Examples.Table exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled exposing (..)
import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Nri.Ui.Button.V10 as Button
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
                , width = Css.px 50
                , cellStyles = always []
                }
            , Table.string
                { header = "B"
                , value = .b
                , width = Css.px 50
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
                { showHeader, isLoading } =
                    Control.currentValue state

                ( columnsCode, columns ) =
                    List.unzip columnsWithCode
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state
                , mainType = "RootHtml.Html msg"
                , extraImports = [ "import Nri.Ui.Button.V10 as Button" ]
                , toExampleCode =
                    \settings ->
                        let
                            codeWithData viewName =
                                List.map datumToString data
                                    |> ControlView.codeFromListSimple
                                    |> toExampleCode viewName

                            toExampleCode viewName dataStr =
                                { sectionName = moduleName ++ "." ++ viewName
                                , code =
                                    (moduleName ++ "." ++ viewName)
                                        ++ ControlView.codeFromListSimple columnsCode
                                        ++ dataStr
                                }
                        in
                        [ codeWithData "view"
                        , codeWithData "viewWithoutHeader"
                        , toExampleCode "viewLoading" ""
                        , toExampleCode "viewLoadingWithoutHeader" ""
                        ]
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


type alias Datum =
    { firstName : String
    , lastName : String
    , submitted : Int
    }


datumToString : Datum -> String
datumToString { firstName, lastName, submitted } =
    ("{ firstName = " ++ str firstName)
        ++ (", lastName = " ++ str lastName)
        ++ (", submitted = " ++ String.fromInt 10)
        ++ "}"


str : String -> String
str s =
    "\"" ++ s ++ "\""


data : List Datum
data =
    [ { firstName = "Monique", lastName = "Garcia", submitted = 10 }
    , { firstName = "Gabriel", lastName = "Smith", submitted = 0 }
    , { firstName = "Mariah", lastName = "Lopez", submitted = 3 }
    , { firstName = "Amber", lastName = "Brown", submitted = 15 }
    , { firstName = "Carlos", lastName = "Martinez", submitted = 8 }
    ]


columnsWithCode : List ( String, Column Datum Msg )
columnsWithCode =
    [ ( [ "Table.string"
        , "  { header = \"First Name\""
        , "  , value = .firstName"
        , "  , width = Css.calc (Css.pct 50) Css.minus (Css.px 250)"
        , "  , cellStyles = always []"
        , "  }"
        ]
            |> String.join "\n\t  "
      , Table.string
            { header = "First Name"
            , value = .firstName
            , width = Css.calc (Css.pct 50) Css.minus (Css.px 250)
            , cellStyles = always []
            }
      )
    , ( [ "Table.string"
        , "  { header = \"Last Name\""
        , "  , value = .lastName"
        , "  , width = Css.calc (Css.pct 50) Css.minus (Css.px 250)"
        , "  , cellStyles = always []"
        , "  }"
        ]
            |> String.join "\n\t  "
      , Table.string
            { header = "Last Name"
            , value = .lastName
            , width = Css.calc (Css.pct 50) Css.minus (Css.px 250)
            , cellStyles = always []
            }
      )
    , ( [ "Table.string"
        , "  { header = \"Submitted\""
        , "  , value = .submitted >> String.fromInt"
        , "  , width = Css.px 125"
        , "  , cellStyles = always [ Css.textAlign Css.center ]"
        , "  }"
        ]
            |> String.join "\n\t  "
      , Table.string
            { header = "Submitted"
            , value = .submitted >> String.fromInt
            , width = Css.px 125
            , cellStyles = \value -> [ Css.textAlign Css.center ]
            }
      )
    , ( [ "Table.custom"
        , "  { header = text \"Actions\""
        , "  , width = Css.px 250"
        , "  , view = \\_ -> Button.button \"Action\" [ Button.small ]"
        , "  , cellStyles = always []"
        , "  }"
        ]
            |> String.join "\n\t  "
      , Table.custom
            { header = text "Actions"
            , width = Css.px 250
            , view = \_ -> Button.button "Action" [ Button.small, Button.onClick (ConsoleLog "Clicked button!") ]
            , cellStyles = always []
            }
      )
    ]
