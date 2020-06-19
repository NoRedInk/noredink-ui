module Examples.MasteryIcon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import AtomicDesignType exposing (AtomicDesignType(..))
import Category exposing (Category(..))
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.MasteryIcon.V1 as MasteryIcon


{-| -}
type alias State =
    ()


{-| -}
type alias Msg =
    ()


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.MasteryIcon.V1"
    , categories = [ Icons ]
    , atomicDesignType = Atom
    , keyboardSupport = []
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , view =
        \_ ->
            [ IconExamples.view "Levels"
                [ ( "levelZero", MasteryIcon.levelZero )
                , ( "levelOne", MasteryIcon.levelOne )
                , ( "levelTwo", MasteryIcon.levelTwo )
                , ( "levelThree", MasteryIcon.levelThree )
                ]
            ]
    }
