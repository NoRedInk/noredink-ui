module Examples.Divider exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Example exposing (Example)
import Nri.Ui.Divider.V2 as Divider


{-| -}
type alias State =
    {}


{-| -}
type alias Msg =
    ()


{-| -}
example : Example State Msg
example =
    { name = "Divider"
    , version = 2
    , categories = [ Layout ]
    , keyboardSupport = []
    , state = {}
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , preview = [ Divider.view "Dividing Line" ]
    , about = []
    , view = \ellieLinkConfig state -> [ Divider.view "Dividing Line" ]
    }
