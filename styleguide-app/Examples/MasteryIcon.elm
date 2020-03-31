module Examples.MasteryIcon exposing (example)

{-|

@docs example, styles

-}

import Category exposing (Category(..))
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.MasteryIcon.V1 as MasteryIcon


{-| -}
example : Example () ()
example =
    { name = "Nri.Ui.MasteryIcon.V1"
    , categories = List.singleton Icons
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
