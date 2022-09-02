module Examples.Layout exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Browser.Events
import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Events as Events
import Json.Decode
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Layout.V1 as Layout
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V6 as Text


{-| -}
example : Example State Msg
example =
    { name = "Layout"
    , version = 1
    , categories = [ Layout ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = []
    , view = \ellieLinkConfig model -> []
    }


{-| -}
type alias State =
    {}


init : State
init =
    {}


{-| -}
type alias Msg =
    ()


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    ( model
    , Cmd.none
    )
