module Examples.Balloon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import Html.Styled exposing (Html, div, text)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Balloon.V1 as Balloon
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading


{-| -}
type alias State =
    ()


{-| -}
type alias Msg =
    ()


{-| -}
example : Example State Msg
example =
    { name = "Balloon"
    , version = 1
    , categories = [ Messaging ]
    , keyboardSupport = []
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , view = view
    }


view : State -> List (Html msg)
view _ =
    let
        copy =
            "Conducting a growth quiz with a previously used passage is not recommended, as students will already be familiar with the material."
    in
    [ Heading.h3 [] [ text "Balloon.balloon []" ]
    , Balloon.balloon [] (text copy)
    , Heading.h3 [] [ text "Balloon.balloon [orange, onTop]" ]
    , Balloon.balloon [ Balloon.onTop, Balloon.orange ] (text copy)
    , Heading.h3 [] [ text "Balloon.balloon [purple, onRight]" ]
    , Balloon.balloon [ Balloon.onRight, Balloon.purple ] (text copy)
    , Heading.h3 [] [ text "Balloon.balloon [onBottom]" ]
    , Balloon.balloon [ Balloon.onBottom ] (text copy)
    , Heading.h3 [] [ text "Balloon.balloon [white, onLeft]" ]
    , Balloon.balloon [ Balloon.white, Balloon.onLeft ] (text copy)
    , Heading.h3 [] [ text "Balloon.balloon [navy, onRight]" ]
    , Balloon.balloon [ Balloon.navy, Balloon.onRight ] (text copy)
    ]
