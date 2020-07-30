module Examples.RadioButton exposing
    ( example
    , State, Msg
    )

{-|

@docs example
@docs State, Msg

-}

import AtomicDesignType exposing (AtomicDesignType(..))
import Category exposing (Category(..))
import Css exposing (..)
import Dict exposing (Dict)
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.RadioButton.V1 as RadioButton
import Nri.Ui.Text.V4 as Text


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.RadioButton.V1"
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    , categories = [ Layout ]
    , atomicDesignType = Atom
    , keyboardSupport = []
    }


{-| -}
view : State -> List (Html Msg)
view model =
    [ Heading.h3 [] [ Html.text "RadioButton" ]
    ]


type alias Msg =
    ()


{-| -}
init : State
init =
    ()


{-| -}
type alias State =
    ()


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    ( model, Cmd.none )
