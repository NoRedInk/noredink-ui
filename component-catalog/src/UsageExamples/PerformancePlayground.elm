module UsageExamples.PerformancePlayground exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Css
import Debug.Control as Control exposing (Control)
import Html.Styled exposing (..)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Text.V6 as Text
import UsageExample exposing (UsageExample)


example : UsageExample State Msg
example =
    { name = "Performance Playground"
    , init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    , about =
        [ Text.mediumBody [ Text.plaintext "Page to check performance when rendering inline css" ]
        ]
    , categories = []
    }


view : State -> List (Html Msg)
view state =
    let
        currentValue =
            Control.currentValue state
    in
    [ Control.view UpdateSettings state
    , Heading.h2
        [ Heading.plaintext "Playground"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , div []
        (List.repeat currentValue.buttonCount ()
            |> List.indexedMap
                (\i _ ->
                    Button.button ("A button " ++ String.fromInt i) []
                )
        )
    ]


type alias Config =
    { buttonCount : Int
    }


{-| -}
type alias State =
    Control Config


init : State
init =
    Control.record Config
        |> Control.field "button count" (Control.int 0)


{-| -}
type Msg
    = UpdateSettings State


update : Msg -> State -> ( State, Cmd Msg )
update msg _ =
    case msg of
        UpdateSettings newState ->
            ( newState
            , Cmd.none
            )
