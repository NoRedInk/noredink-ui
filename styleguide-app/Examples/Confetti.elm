module Examples.Confetti exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Browser.Events
import Category exposing (Category(..))
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Confetti.V2 as Confetti
import Nri.Ui.Html.V3 exposing (viewJust)


moduleName : String
moduleName =
    "Confetti"


version : Int
version =
    2


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Animations ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions =
        \state ->
            case state.model of
                Just model ->
                    Sub.batch
                        [ Browser.Events.onResize WindowResized
                        , Confetti.subscriptions ConfettiMsg model
                        ]

                Nothing ->
                    Sub.none
    , preview = []
    , view =
        \ellieLinkConfig state ->
            [ viewJust Confetti.view state.model
            , ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state.settings
                , mainType = "RootHtml.Html msg"
                , extraCode = []
                , toExampleCode =
                    \settings -> [ { sectionName = "TODO", code = "TODO" } ]
                }
            , Button.button "Launch confetti!"
                [ Button.onClick LaunchConfetti
                , Button.small
                , Button.secondary
                ]
            ]
    }


{-| -}
type alias State =
    { settings : Control Settings
    , model : Maybe Confetti.Model
    }


init : State
init =
    { settings = initSettings
    , model = Nothing
    }


type alias Settings =
    { center : Float
    }


initSettings : Control Settings
initSettings =
    Control.record Settings
        |> Control.field "center" (ControlExtra.float 700)


{-| -}
type Msg
    = LaunchConfetti
    | ConfettiMsg Confetti.Msg
    | WindowResized Int Int
    | UpdateControl (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    ( case msg of
        LaunchConfetti ->
            { state | model = Just (Confetti.burst (Confetti.init (Control.currentValue state.settings).center)) }

        ConfettiMsg confettiMsg ->
            { state | model = Maybe.map (Confetti.update confettiMsg) state.model }

        WindowResized width _ ->
            { state | model = Maybe.map (Confetti.updatePageWidth width) state.model }

        UpdateControl newControl ->
            { state | settings = newControl }
    , Cmd.none
    )
