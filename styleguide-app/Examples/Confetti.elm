module Examples.Confetti exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Browser.Events
import Category exposing (Category(..))
import Css exposing (Color)
import Dict exposing (Dict)
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
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
    , subscriptions =
        \state ->
            Sub.batch
                [ Browser.Events.onResize WindowResized
                , Confetti.subscriptions ConfettiMsg state.confetti
                ]
    , view =
        \state ->
            [ Button.button "Launch confetti!"
                [ Button.onClick LaunchConfetti
                , Button.small
                , Button.secondary
                ]
            , Confetti.view state.confetti
            ]
    }


{-| -}
type alias State =
    { confetti : Confetti.Model
    }


init : State
init =
    { confetti = Confetti.init 700
    }


{-| -}
type Msg
    = LaunchConfetti
    | ConfettiMsg Confetti.Msg
    | WindowResized Int Int


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        LaunchConfetti ->
            ( { state | confetti = Confetti.burst [] state.confetti }
            , Cmd.none
            )

        ConfettiMsg confettiMsg ->
            ( { state | confetti = Confetti.update confettiMsg state.confetti }
            , Cmd.none
            )

        WindowResized width _ ->
            ( { state | confetti = Confetti.updatePageWidth width state.confetti }
            , Cmd.none
            )


getColor : Int -> Color
getColor key =
    let
        dict =
            List.indexedMap (\i c -> ( i, c ))
                [ Colors.highlightBlue
                , Colors.highlightBlueDark
                , Colors.highlightCyan
                , Colors.highlightCyanDark
                , Colors.highlightGreen
                , Colors.highlightGreenDark
                , Colors.highlightMagenta
                , Colors.highlightMagentaDark
                , Colors.highlightYellow
                , Colors.highlightYellowDark
                ]
                |> Dict.fromList
    in
    Maybe.withDefault Colors.highlightYellow (Dict.get key dict)
