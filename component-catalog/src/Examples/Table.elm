module Examples.Table exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled exposing (..)
import Category exposing (Category(..))
import Code
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Table.V8 as Table exposing (Column)


{-| -}
type alias State =
    Control Settings


moduleName : String
moduleName =
    "Table"


version : Int
version =
    8


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , init = ( controlSettings, Cmd.none )
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Layout ]
    , keyboardSupport = []
    , preview =
        [ Table.view []
            [ Table.string
                { header = "A"
                , value = .a
                , width = Css.px 50
                , cellStyles = always []
                , sort = Nothing
                }
            , Table.string
                { header = "B"
                , value = .b
                , width = Css.px 50
                , cellStyles = always []
                , sort = Nothing
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
    , about = []
    , view =
        \ellieLinkConfig state ->
            let
                { showHeader, isLoading, alternatingRowColors } =
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
                , mainType = Just "RootHtml.Html msg"
                , extraCode = [ "import Nri.Ui.Button.V10 as Button" ]
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \_ ->
                        let
                            codeWithData viewName =
                                List.map datumToString data
                                    |> Code.list
                                    |> toExampleCode viewName

                            toExampleCode viewName dataStr =
                                { sectionName = moduleName ++ "." ++ viewName
                                , code =
                                    (moduleName ++ "." ++ viewName)
                                        ++ " "
                                        ++ Code.list
                                            (if alternatingRowColors then
                                                []

                                             else
                                                [ "Table.disableAlternatingRowColors" ]
                                            )
                                        ++ Code.list columnsCode
                                        ++ dataStr
                                }
                        in
                        [ codeWithData "view"
                        , codeWithData "viewWithoutHeader"
                        , toExampleCode "viewLoading" ""
                        , toExampleCode "viewLoadingWithoutHeader" ""
                        ]
                }
            , Heading.h2
                [ Heading.plaintext "Example"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , case ( showHeader, isLoading ) of
                ( True, False ) ->
                    Table.view [] columns data

                ( False, False ) ->
                    Table.viewWithoutHeader [] columns data

                ( True, True ) ->
                    Table.viewLoading [] columns

                ( False, True ) ->
                    Table.viewLoadingWithoutHeader [] columns
            , Heading.h2
                [ Heading.plaintext "Using placeholderColumn for consistent widths tables"
                , Heading.css [ Css.marginTop (Css.px 30) ]
                ]
            , Heading.h3 [ Heading.plaintext "With placeholderColumn", Heading.css [ Css.marginTop (Css.px 30) ] ]
            , Table.view []
                [ Table.rowHeader
                    { header = text "User ID"
                    , view = text << String.fromInt << .userId
                    , width = Css.px 80
                    , cellStyles = always []
                    , sort = Nothing
                    }
                , Table.string
                    { header = "First Name"
                    , value = .firstName
                    , width = Css.calc (Css.pct 50) Css.minus (Css.px 250)
                    , cellStyles = always []
                    , sort = Nothing
                    }
                , Table.string
                    { header = "Last Name"
                    , value = .lastName
                    , width = Css.calc (Css.pct 50) Css.minus (Css.px 250)
                    , cellStyles = always []
                    , sort = Nothing
                    }
                , Table.placeholderColumn
                    { width = Css.px 125
                    }
                , Table.placeholderColumn
                    { width = Css.px 250
                    }
                ]
                data
            , Heading.h3 [ Heading.plaintext "Without placeholderColumn", Heading.css [ Css.marginTop (Css.px 30) ] ]
            , Table.view []
                [ Table.rowHeader
                    { header = text "User ID"
                    , view = text << String.fromInt << .userId
                    , width = Css.px 80
                    , cellStyles = always []
                    , sort = Nothing
                    }
                , Table.string
                    { header = "First Name"
                    , value = .firstName
                    , width = Css.calc (Css.pct 50) Css.minus (Css.px 250)
                    , cellStyles = always []
                    , sort = Nothing
                    }
                , Table.string
                    { header = "Last Name"
                    , value = .lastName
                    , width = Css.calc (Css.pct 50) Css.minus (Css.px 250)
                    , cellStyles = always []
                    , sort = Nothing
                    }
                , Table.string
                    { header = "Submitted"
                    , value = .submitted >> String.fromInt
                    , width = Css.px 125
                    , cellStyles = always [ Css.textAlign Css.center ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Actions"
                    , width = Css.px 250
                    , view = \_ -> Button.button "Action" [ Button.small ]
                    , cellStyles = always []
                    , sort = Nothing
                    }
                ]
                data
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
    , alternatingRowColors : Bool
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "visible header" (Control.bool True)
        |> Control.field "is loading" (Control.bool False)
        |> Control.field "alternatingRowColors" (Control.bool True)


type alias Datum =
    { userId : Int
    , firstName : String
    , lastName : String
    , submitted : Int
    }


datumToString : Datum -> String
datumToString { userId, firstName, lastName, submitted } =
    ("{ userId = " ++ String.fromInt userId)
        ++ (", firstName = " ++ str firstName)
        ++ (", lastName = " ++ str lastName)
        ++ (", submitted = " ++ String.fromInt 10)
        ++ "}"


str : String -> String
str s =
    "\"" ++ s ++ "\""


data : List Datum
data =
    [ { userId = 1, firstName = "Monique", lastName = "Garcia", submitted = 10 }
    , { userId = 2, firstName = "Gabriel", lastName = "Smith", submitted = 0 }
    , { userId = 3, firstName = "Mariah", lastName = "Lopez", submitted = 3 }
    , { userId = 4, firstName = "Amber", lastName = "Brown", submitted = 15 }
    , { userId = 5, firstName = "Carlos", lastName = "Martinez", submitted = 8 }
    ]


columnsWithCode : List ( String, Column Datum Msg )
columnsWithCode =
    [ ( [ "Table.rowHeader"
        , "  { header = text \"User ID\""
        , "  , view = text << String.fromInt << .userId"
        , "  , width = Css.px 80"
        , "  , cellStyles = always []"
        , "  , sort = Nothing"
        , "  }"
        ]
            |> String.join "\n\t  "
      , Table.rowHeader
            { header = text "User ID"
            , view = text << String.fromInt << .userId
            , width = Css.px 80
            , cellStyles = always []
            , sort = Nothing
            }
      )
    , ( [ "Table.string"
        , "  { header = \"First Name\""
        , "  , value = .firstName"
        , "  , width = Css.calc (Css.pct 50) Css.minus (Css.px 250)"
        , "  , cellStyles = always []"
        , "  , sort = Nothing"
        , "  }"
        ]
            |> String.join "\n\t  "
      , Table.string
            { header = "First Name"
            , value = .firstName
            , width = Css.calc (Css.pct 50) Css.minus (Css.px 250)
            , cellStyles = always []
            , sort = Nothing
            }
      )
    , ( [ "Table.string"
        , "  { header = \"Last Name\""
        , "  , value = .lastName"
        , "  , width = Css.calc (Css.pct 50) Css.minus (Css.px 250)"
        , "  , cellStyles = always []"
        , "  , sort = Nothing"
        , "  }"
        ]
            |> String.join "\n\t  "
      , Table.string
            { header = "Last Name"
            , value = .lastName
            , width = Css.calc (Css.pct 50) Css.minus (Css.px 250)
            , cellStyles = always []
            , sort = Nothing
            }
      )
    , ( [ "Table.string"
        , "  { header = \"Submitted\""
        , "  , value = .submitted >> String.fromInt"
        , "  , width = Css.px 125"
        , "  , cellStyles = always [ Css.textAlign Css.center ]"
        , "  , sort = Nothing"
        , "  }"
        ]
            |> String.join "\n\t  "
      , Table.string
            { header = "Submitted"
            , value = .submitted >> String.fromInt
            , width = Css.px 125
            , cellStyles = \value -> [ Css.textAlign Css.center ]
            , sort = Nothing
            }
      )
    , ( [ "Table.custom"
        , "  { header = text \"Actions\""
        , "  , width = Css.px 250"
        , "  , view = \\_ -> Button.button \"Action\" [ Button.small ]"
        , "  , cellStyles = always []"
        , "  , sort = Nothing"
        , "  }"
        ]
            |> String.join "\n\t  "
      , Table.custom
            { header = text "Actions"
            , width = Css.px 250
            , view = \_ -> Button.button "Action" [ Button.small, Button.onClick (ConsoleLog "Clicked button!") ]
            , cellStyles = always []
            , sort = Nothing
            }
      )
    ]
