module Examples.RadioButtonDotless exposing
    ( Msg
    , State
    , example
    )

import Category exposing (Category(..))
import EllieLink
import Example exposing (Example)
import Guidance
import Html.Styled exposing (..)
import Nri.Ui.RadioButtonDotless.V1 as RadioButtonDotless
import Platform.Sub as Sub


type alias State =
    { radioValue : Maybe Int }


type Msg
    = Select Int


moduleName : String
moduleName =
    "RadioButtonDotless"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = preview
    , about = Guidance.useATACGuide moduleName
    , view = view
    , categories = [ Inputs ]
    , keyboardSupport = []
    }


init : State
init =
    { radioValue = Nothing }


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Select i ->
            ( { state | radioValue = Just i }, Cmd.none )


preview : List (Html Never)
preview =
    []


view : EllieLink.Config -> State -> List (Html Msg)
view ellieLink state =
    [ RadioButtonDotless.view
        { label = "Button 1"
        , name = "button-1"
        , value = 1
        , valueToString = String.fromInt
        , selectedValue = state.radioValue
        }
        [ RadioButtonDotless.onSelect Select ]
    , RadioButtonDotless.view
        { label = "Button 2"
        , name = "button-2"
        , value = 2
        , valueToString = String.fromInt
        , selectedValue = state.radioValue
        }
        [ RadioButtonDotless.onSelect Select ]
    ]
