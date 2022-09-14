module Examples.Select exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled.Key as Key
import Category exposing (Category(..))
import Code
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Select.V8 as Select exposing (Choice)


moduleName : String
moduleName =
    "Select"


version : Int
version =
    8


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Inputs ]
    , keyboardSupport = []
    , preview =
        [ Select.view "Label" [ Select.custom [ Key.tabbable False ] ]
        , Select.view "Hidden label"
            [ Select.hiddenLabel
            , Select.defaultDisplayText "Hidden label"
            , Select.custom [ Key.tabbable False ]
            ]
        ]
    , view =
        \ellieLinkConfig state ->
            let
                label =
                    (Control.currentValue state).label

                ( attributesCode, attributes ) =
                    List.unzip (Control.currentValue state).attributes
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateSettings
                , settings = state
                , mainType = Just "RootHtml.Html String"
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "Example"
                          , code =
                                "Select.view \""
                                    ++ label
                                    ++ "\""
                                    ++ "\n    [ "
                                    ++ String.join "\n    , " attributesCode
                                    ++ "\n    ] "
                          }
                        ]
                }
            , Heading.h2 [ Heading.plaintext "Example" ]
            , Select.view label attributes
                |> Html.Styled.map ConsoleLog
            ]
    }


{-| -}
type alias State =
    Control Settings


{-| -}
init : State
init =
    Control.record Settings
        |> Control.field "label" (Control.string "Tortilla Selector")
        |> Control.field "attributes" initControls


type alias Settings =
    { label : String
    , attributes : List ( String, Select.Attribute String )
    }


initControls : Control (List ( String, Select.Attribute String ))
initControls =
    ControlExtra.list
        |> ControlExtra.listItem "choices"
            (Control.map
                (\( code, choices ) ->
                    ( "Select.choices identity" ++ code
                    , Select.choices identity choices
                    )
                )
                initChoices
            )
        |> ControlExtra.optionalListItem "hiddenLabel"
            (Control.value ( "Select.hiddenLabel", Select.hiddenLabel ))
        |> ControlExtra.optionalListItem "defaultDisplayText"
            (Control.map
                (\str ->
                    ( "Select.defaultDisplayText \"" ++ str ++ "\""
                    , Select.defaultDisplayText str
                    )
                )
                (Control.string "Select a tasty tortilla based treat!")
            )
        |> ControlExtra.optionalListItem "containerCss"
            (Control.choice
                [ ( "max-width: 300px"
                  , Control.value
                        ( "Select.containerCss [ Css.maxWidth (Css.px 300) ]"
                        , Select.containerCss [ Css.maxWidth (Css.px 300) ]
                        )
                  )
                , ( "background-color: lichen"
                  , Control.value
                        ( "Select.containerCss [ Css.backgroundColor Colors.lichen ]"
                        , Select.containerCss [ Css.backgroundColor Colors.lichen ]
                        )
                  )
                ]
            )
        |> ControlExtra.optionalListItem "noMargin"
            (Control.map
                (\bool ->
                    ( "Select.noMargin " ++ Debug.toString bool
                    , Select.noMargin bool
                    )
                )
                (Control.bool True)
            )
        |> ControlExtra.optionalListItem "errorMessage"
            (Control.map
                (\str ->
                    ( "Select.errorMessage (Just \"" ++ str ++ "\")"
                    , Select.errorMessage (Just str)
                    )
                )
                (Control.string "The right item must be selected.")
            )
        |> ControlExtra.optionalListItem "guidance"
            (Control.map
                (\str ->
                    ( "Select.guidance \"" ++ str ++ "\""
                    , Select.guidance str
                    )
                )
             <|
                Control.string "The right item must be selected."
            )
        |> ControlExtra.optionalListItem "disabled"
            (Control.value ( "Select.disabled", Select.disabled ))
        |> ControlExtra.optionalListItem "loading"
            (Control.value ( "Select.loading", Select.loading ))


initChoices : Control ( String, List (Choice String) )
initChoices =
    Control.choice
        [ ( "Short choices"
          , ( """
        [ { label = "Tacos", value = "tacos" }
        , { label = "Burritos", value = "burritos" }
        , { label = "Enchiladas", value = "enchiladas" }
        ]"""
            , [ { label = "Tacos", value = "tacos" }
              , { label = "Burritos", value = "burritos" }
              , { label = "Enchiladas", value = "enchiladas" }
              ]
            )
                |> Control.value
          )
        , ( "Overflowing text choices"
          , ( """
        [ { label = "Look at me, I design coastlines, I got an award for Norway. Where's the sense in that? My mistress' eyes are nothing like the sun. Coral be far more red than her lips red.", value = "design-coastlines" }
        ]"""
            , [ { label = "Look at me, I design coastlines, I got an award for Norway. Where's the sense in that? My mistress' eyes are nothing like the sun. Coral be far more red than her lips red.", value = "design-coastlines" }
              ]
            )
                |> Control.value
          )
        ]


{-| -}
type Msg
    = ConsoleLog String
    | UpdateSettings (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ConsoleLog message ->
            ( Debug.log "SelectExample" message |> always state, Cmd.none )

        UpdateSettings settings ->
            ( settings, Cmd.none )
