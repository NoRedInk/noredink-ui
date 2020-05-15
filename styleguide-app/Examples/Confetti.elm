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
import Nri.Ui.TextArea.V4 as TextArea


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
            [ confettiWordsInput state.confettiWords
            , Button.button "Launch confetti!"
                [ Button.onClick LaunchConfetti
                , Button.small
                , Button.secondary
                ]
            , Confetti.view state.confetti
            ]
    }


confettiWordsInput : String -> Html Msg
confettiWordsInput confettiWords =
    Html.div [ css [ Css.width (Css.px 600), Css.marginBottom (Css.px 10) ] ]
        [ TextArea.writing
            { value = confettiWords
            , autofocus = False
            , onInput = OnInput
            , onBlur = Nothing
            , isInError = False
            , label = "Confetti Words"
            , height = TextArea.Fixed
            , placeholder = ""
            , showLabel = True
            }
        ]


{-| -}
type alias State =
    { confetti : Confetti.Model
    , confettiWords : String
    }


init : State
init =
    { confetti = Confetti.init 700
    , confettiWords = "lemonade iced tea coca-cola pepsi"
    }


{-| -}
type Msg
    = LaunchConfetti
    | ConfettiMsg Confetti.Msg
    | WindowResized Int Int
    | OnInput String


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    let
        words =
            List.indexedMap
                (\index word -> { color = getColor index, text = word })
                (String.words state.confettiWords)
    in
    case msg of
        LaunchConfetti ->
            ( { state | confetti = Confetti.burst words state.confetti }
            , Cmd.none
            )

        ConfettiMsg confettiMsg ->
            ( { state | confetti = Confetti.update confettiMsg state.confetti }
            , Cmd.none
            )

        WindowResized width _ ->
            ( { state | confetti = Confetti.updateCenter (toFloat (width // 2)) state.confetti }
            , Cmd.none
            )

        OnInput confettiWords ->
            ( { state | confettiWords = confettiWords }
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
