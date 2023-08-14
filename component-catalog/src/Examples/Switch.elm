module Examples.Switch exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled.Key as Key
import Category
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled exposing (..)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Switch.V2 as Switch
import Nri.Ui.Table.V7 as Table


moduleName : String
moduleName =
    "Switch"


version : Int
version =
    2


example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ Switch.view { label = "Toggle Off", id = "preview-switch-a" }
            [ Switch.selected False
            , Switch.custom [ Key.tabbable False ]
            ]
        , Switch.view { label = "Toggle On", id = "preview-switch-b" }
            [ Switch.selected True
            , Switch.custom [ Key.tabbable False ]
            ]
        ]
    , about = []
    , view =
        \ellieLinkConfig state ->
            let
                currentValue =
                    Control.currentValue state.settings
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateSettings
                , settings = state.settings
                , mainType = Just "RootHtml.Html msg"
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \{ label, attributes } ->
                        [ { sectionName = "Example"
                          , code =
                                Code.fromModule moduleName "view"
                                    ++ Code.recordMultiline
                                        [ ( "label", Code.string label )
                                        , ( "id", Code.string "view-switch-example" )
                                        ]
                                        1
                                    ++ Code.listMultiline
                                        (("Switch.selected "
                                            ++ Debug.toString state.selected
                                         )
                                            :: "Switch.onSwitch Switch -- <- you'll need to wire in a Msg for the Switch to work"
                                            :: List.map Tuple.first attributes
                                        )
                                        1
                          }
                        ]
                }
            , Heading.h2
                [ Heading.plaintext "Customizable example"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , Switch.view { label = currentValue.label, id = "view-switch-example" }
                (Switch.selected state.selected
                    :: Switch.onSwitch Switch
                    :: List.map Tuple.second currentValue.attributes
                )
            , Heading.h2
                [ Heading.plaintext "Examples"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , Table.view []
                [ Table.string
                    { header = "State"
                    , value = .state
                    , width = Css.pct 30
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle, Css.fontWeight Css.bold ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Enabled"
                    , view = .enabled
                    , width = Css.px 150
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Disabled"
                    , view = .disabled
                    , width = Css.px 150
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
                    , sort = Nothing
                    }
                ]
                [ { state = "Off"
                  , enabled =
                        Switch.view
                            { label = "Show dropped students"
                            , id = "show-dropped-students-off-enabled"
                            }
                            [ Switch.selected False
                            , Switch.onSwitch (\_ -> Swallow)
                            ]
                  , disabled =
                        Switch.view
                            { label = "Show dropped students"
                            , id = "show-dropped-students-off-disabled"
                            }
                            [ Switch.selected False
                            , Switch.disabled True
                            ]
                  }
                , { state = "On"
                  , enabled =
                        Switch.view
                            { label = "Show dropped students"
                            , id = "show-dropped-students-on-enabled"
                            }
                            [ Switch.selected True
                            , Switch.onSwitch (\_ -> Swallow)
                            ]
                  , disabled =
                        Switch.view
                            { label = "Show dropped students"
                            , id = "show-dropped-students-on-disabled"
                            }
                            [ Switch.selected True
                            , Switch.disabled True
                            ]
                  }
                ]
            ]
    , categories = [ Category.Inputs ]
    , keyboardSupport =
        [ { keys = [ Space ]
          , result = "Toggle the Switch state"
          }
        ]
    }


{-| -}
type alias State =
    { selected : Bool
    , settings : Control Settings
    }


init : State
init =
    { selected = True
    , settings = controlSettings
    }


type alias Settings =
    { label : String
    , attributes : List ( String, Switch.Attribute Msg )
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "label" (Control.string "Show pandas in results")
        |> Control.field "attributes" initAttributes


initAttributes : Control (List ( String, Switch.Attribute msg ))
initAttributes =
    ControlExtra.list
        |> CommonControls.disabledListItem moduleName Switch.disabled


{-| -}
type Msg
    = Switch Bool
    | UpdateSettings (Control Settings)
    | Swallow


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Switch bool ->
            ( { state | selected = bool }
            , Cmd.none
            )

        UpdateSettings settings ->
            ( { state | settings = settings }
            , Cmd.none
            )

        Swallow ->
            ( state
            , Cmd.none
            )
