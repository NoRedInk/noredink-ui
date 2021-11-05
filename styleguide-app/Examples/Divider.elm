module Examples.Divider exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Direction(..), Key(..))
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
    , view = \state -> [ Divider.view "Dividing Line" ]
    }
