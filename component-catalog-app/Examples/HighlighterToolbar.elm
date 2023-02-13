module Examples.HighlighterToolbar exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Code
import Css exposing (Color)
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
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
            , Heading.h2 [ Heading.plaintext "Example" ]
            , HighlighterToolbar.view
                { onSetEraser = SetTool Nothing
                , onChangeTag = SetTool << Just
                , getColor = getColor
                , getName = getName
                }
                { currentTool = state.currentTool
                , tags = tags
                }
            ]
    , categories = [ Buttons, Interactions ]
    , keyboardSupport = []
    }


type Tag
    = Claim
    | Evidence
    | Reasoning


tags : List Tag
tags =
    [ Claim, Evidence, Reasoning ]


getName : Tag -> String
getName tag =
    case tag of
        Claim ->
            "Claim"

        Evidence ->
            "Evidence"

        Reasoning ->
            "Reasoning"


getColor : Tag -> { colorSolid : Color, colorLight : Color }
getColor tag =
    case tag of
        Claim ->
            { colorSolid = Colors.mustard
            , colorLight = Colors.highlightYellow
            }

        Evidence ->
            { colorSolid = Colors.magenta
            , colorLight = Colors.highlightMagenta
            }

        Reasoning ->
            { colorSolid = Colors.cyan
            , colorLight = Colors.highlightCyan
            }


{-| -}
type alias State =
    { settings : Control Settings
    , currentTool : Maybe Tag
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
    | SetTool (Maybe Tag)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControls settings ->
            ( { state | settings = settings }, Cmd.none )

        SetTool tag ->
            ( { state | currentTool = tag }, Cmd.none )
