module Examples.MasteryIcon exposing (example)

{-|

@docs example, styles

-}

import Examples.IconExamples as IconExamples
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.MasteryIcon.V1 as MasteryIcon


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.MasteryIcon.V1"
    , category = Icons
    , content =
        [ IconExamples.view "Levels"
            [ ( "levelZero", MasteryIcon.levelZero )
            , ( "levelOne", MasteryIcon.levelOne )
            , ( "levelTwo", MasteryIcon.levelTwo )
            , ( "levelThree", MasteryIcon.levelThree )
            ]
        ]
    }
