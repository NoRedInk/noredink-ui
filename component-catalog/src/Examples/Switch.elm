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
import Debug.Control.View as ControlView
import Example exposing (Example)
import Guidance
import Html.Styled exposing (..)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Switch.V3 as Switch
import Nri.Ui.Table.V7 as Table
import Nri.Ui.Tooltip.V3 as Tooltip


moduleName : String
moduleName =
    "Switch"


version : Int
version =
    3


type TooltipType
    = HelpfullyDisabled


example : Example State Msg
example =
    { name = moduleName
    , version = version
    , init = ( init, Cmd.none )
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
    , about = [ Guidance.helpfullyDisabled moduleName ]
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
                , mainType = Just "RootHtml.Html Msg"
                , extraCode =
                    [ Code.newlines
                    , Code.unionType "Msg" [ "Switch Bool" ]
                    ]
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
            , Table.view { additionalStyles = [], alternatingRowColors = True }
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
            , Heading.h2
                [ Heading.plaintext "Helpfully Disabled Example"
                , Heading.css
                    [ Css.marginTop Spacing.verticalSpacerPx
                    , Css.marginBottom (Css.px 10)
                    ]
                ]
            , Tooltip.view
                { trigger =
                    \attrs ->
                        Switch.view { id = "tooltip-example", label = "Show pandas in results" }
                            [ Switch.disabled True
                            , Switch.custom attrs
                            ]
                , id = "tooltip"
                }
                [ Tooltip.helpfullyDisabled
                , Tooltip.open (state.openTooltip == Just HelpfullyDisabled)
                , Tooltip.onToggle (ToggleTooltip HelpfullyDisabled)
                , Tooltip.paragraph "Reasons why you can't toggle this switch"
                , Tooltip.onRight
                , Tooltip.fitToContent
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
    , openTooltip : Maybe TooltipType
    }


init : State
init =
    { selected = True
    , settings = controlSettings
    , openTooltip = Nothing
    }


type alias Settings =
    { label : String
    , attributes : List ( String, Switch.Attribute Msg )
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "label" (Control.string "Show pandas in results")
        |> Control.field "" initAttributes


initAttributes : Control (List ( String, Switch.Attribute msg ))
initAttributes =
    Control.list
        |> CommonControls.disabledListItem moduleName Switch.disabled


{-| -}
type Msg
    = Switch Bool
    | UpdateSettings (Control Settings)
    | Swallow
    | ToggleTooltip TooltipType Bool


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

        ToggleTooltip type_ isOpen ->
            if isOpen then
                ( { state | openTooltip = Just type_ }, Cmd.none )

            else
                ( { state | openTooltip = Nothing }, Cmd.none )
