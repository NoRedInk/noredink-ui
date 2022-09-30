module Examples.HighlighterToolbar exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Code
import CommonControls
import Css exposing (Color)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Examples.Colors
import Html.Styled exposing (..)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.HighlighterToolbar.V1 as HighlighterToolbar


moduleName : String
moduleName =
    "HighlighterToolbar"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        []
    , view =
        \ellieLinkConfig state ->
            let
                getColor name =
                    { solid = Colors.magenta
                    , light = Colors.highlightYellow
                    , name = name
                    }

                tags =
                    [ "Claim", "Evidence", "Reasoning" ]
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControls
                , settings = state.settings
                , mainType = Nothing
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode = \_ -> []
                }
            , Heading.h2 [ Heading.plaintext "Interactive Example" ]
            , HighlighterToolbar.view
                { onSetEraser = SetTool Nothing
                , onChangeTag = SetTool << Just
                , getColor = getColor
                , getName = identity
                }
                { currentTool = state.currentTool
                , tags = tags
                }
            , Heading.h2 [ Heading.plaintext "Non-interactive examples" ]
            , HighlighterToolbar.static identity getColor tags
            ]
    , categories = [ Buttons, Interactions ]
    , keyboardSupport = []
    }


{-| -}
type alias State =
    { settings : Control Settings
    , currentTool : Maybe String
    }


{-| -}
init : State
init =
    let
        settings =
            controlSettings
    in
    { settings = settings
    , currentTool = Nothing
    }


type alias Settings =
    {}


controlSettings : Control Settings
controlSettings =
    Control.record Settings


{-| -}
type Msg
    = UpdateControls (Control Settings)
    | SetTool (Maybe String)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControls settings ->
            ( { state | settings = settings }, Cmd.none )

        SetTool tag ->
            ( { state | currentTool = tag }, Cmd.none )
