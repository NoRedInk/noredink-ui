module Examples.Switch exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled.Key as Key
import Category
import Code
import CommonControls
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Switch.V2 as Switch


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
                                moduleName
                                    ++ ".view"
                                    ++ " \""
                                    ++ label
                                    ++ "\"\t"
                                    ++ Code.list
                                        (("Switch.selected "
                                            ++ Debug.toString state.selected
                                            ++ Code.commentInline "\n,  Switch.onSwitch Switch -- <- you'll need to wire in a Msg for the Switch to work"
                                         )
                                            :: List.map Tuple.first attributes
                                        )
                          }
                        ]
                }
            , Switch.view { label = currentValue.label, id = "view-switch-example" }
                (Switch.selected state.selected
                    :: Switch.onSwitch Switch
                    :: List.map Tuple.second currentValue.attributes
                )
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
