module Examples.BreadCrumbs exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Example exposing (Example)


{-| -}
type alias State =
    {}


{-| -}
type alias Msg =
    ()


{-| -}
example : Example State Msg
example =
    { name = "BreadCrumbs"
    , version = 1
    , categories = [ Layout ]
    , keyboardSupport = []
    , state = {}
    , update = \_ m -> ( m, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , preview = []
    , view =
        \ellieLinkConfig settings ->
            []
    }
