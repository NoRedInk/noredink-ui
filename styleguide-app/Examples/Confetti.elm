module Examples.Confetti exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Html.Styled as Html
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Confetti.V1 as Confetti


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.Confetti.V1"
    , categories = [ Animations ]
    , state = init
    , update = update
    , subscriptions = \state -> Confetti.subscriptions ConfettiMsg state.confettiState
    , view =
        \state ->
            [ Button.button "Launch confetti!"
                [ Button.onClick LaunchConfetti
                , Button.small
                , Button.secondary
                ]
            , Confetti.view state.confettiState
            ]
    }


{-| -}
type alias State =
    { confettiState : Confetti.System
    }


init : State
init =
    { confettiState = Confetti.init 50
    }


{-| -}
type Msg
    = LaunchConfetti
    | ConfettiMsg Confetti.Msg


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    let
        words =
            [ { color = Colors.azure, text = "Hello!" } ]
    in
    case msg of
        LaunchConfetti ->
            ( { state | confettiState = Confetti.burst words state.confettiState }
            , Cmd.none
            )

        ConfettiMsg confettiMsg ->
            ( { state | confettiState = Confetti.update confettiMsg state.confettiState }
            , Cmd.none
            )
