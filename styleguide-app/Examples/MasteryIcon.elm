module Examples.MasteryIcon exposing (example)

{-|

@docs example, styles

-}

import Category exposing (Category(..))
import Examples.IconExamples as IconExamples
import ModuleExample exposing (ModuleExample)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.MasteryIcon.V1 as MasteryIcon
import Sort.Set as Set exposing (Set)


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.MasteryIcon.V1"
    , categories = Set.fromList Category.sorter <| List.singleton Icons
    , content =
        [ IconExamples.view "Levels"
            [ ( "levelZero", MasteryIcon.levelZero )
            , ( "levelOne", MasteryIcon.levelOne )
            , ( "levelTwo", MasteryIcon.levelTwo )
            , ( "levelThree", MasteryIcon.levelThree )
            ]
        ]
    }
