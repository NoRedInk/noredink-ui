module Examples.Confetti exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Browser.Events
import Category exposing (Category(..))
import Example exposing (Example)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Confetti.V2 as Confetti


{-| -}
example : Example State Msg
example =
    { name = "Confetti"
    , version = 2
    , categories = [ Animations ]
    , keyboardSupport = []
    , state = Confetti.init 700
    , update = update
    , subscriptions =
        \state ->
            Sub.batch
                [ Browser.Events.onResize WindowResized
                , Confetti.subscriptions ConfettiMsg state
                ]
    , preview = []
    , view =
        \ellieLinkConfig state ->
            [ Button.button "Launch confetti!"
                [ Button.onClick LaunchConfetti
                , Button.small
                , Button.secondary
                ]
            , Confetti.view state
            ]
    }


{-| -}
type alias State =
    Confetti.Model


{-| -}
type Msg
    = LaunchConfetti
    | ConfettiMsg Confetti.Msg
    | WindowResized Int Int


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    ( case msg of
        LaunchConfetti ->
            Confetti.burst state

        ConfettiMsg confettiMsg ->
            Confetti.update confettiMsg state

        WindowResized width _ ->
            Confetti.updatePageWidth width state
    , Cmd.none
    )
