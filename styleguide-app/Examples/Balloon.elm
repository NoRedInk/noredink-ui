module Examples.Balloon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import Html.Styled exposing (Html, div, fromUnstyled, text)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Balloon.V1 as Balloon
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading


{-| -}
example : Example State Msg
example =
    { name = "Balloon"
    , version = 1
    , categories = [ Messaging ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }


{-| -}
type alias State =
    Control Settings


type alias Settings =
    { copy : String
    }


init =
    Control.record Settings
        |> Control.field "copy" (Control.string "Conducting a growth quiz with a previously used passage is not recommended, as students will already be familiar with the material.")


{-| -}
type Msg
    = SetDebugControlsState (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetDebugControlsState newDebugControlsState ->
            ( newDebugControlsState
            , Cmd.none
            )


view : State -> List (Html Msg)
view state =
    let
        settings =
            Control.currentValue state
    in
    [ Control.view SetDebugControlsState state |> fromUnstyled
    , Heading.h3 [] [ text "Balloon.balloon []" ]
    , Balloon.balloon [] (text settings.copy)
    , Heading.h3 [] [ text "Balloon.balloon [orange, onTop]" ]
    , Balloon.balloon [ Balloon.onTop, Balloon.orange ] (text settings.copy)
    , Heading.h3 [] [ text "Balloon.balloon [purple, onRight]" ]
    , Balloon.balloon [ Balloon.onRight, Balloon.purple ] (text settings.copy)
    , Heading.h3 [] [ text "Balloon.balloon [onBottom]" ]
    , Balloon.balloon [ Balloon.onBottom ] (text settings.copy)
    , Heading.h3 [] [ text "Balloon.balloon [white, onLeft]" ]
    , Balloon.balloon [ Balloon.white, Balloon.onLeft ] (text settings.copy)
    , Heading.h3 [] [ text "Balloon.balloon [navy, onRight]" ]
    , Balloon.balloon [ Balloon.navy, Balloon.onRight ] (text settings.copy)
    ]
