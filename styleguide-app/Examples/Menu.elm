module Examples.Menu exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Html.Styled
import Html.Styled.Attributes
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Menu.V1 as Meny


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.Menu.V1"
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Widgets ]
    , view =
        \state ->
            []
    }


{-| -}
init : State
init =
    ()


{-| -}
type alias State =
    ()


{-| -}
type Msg
    = NoOp


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        NoOp ->
            ( state, Cmd.none )
