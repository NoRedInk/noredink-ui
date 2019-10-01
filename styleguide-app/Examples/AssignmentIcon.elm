module Examples.AssignmentIcon exposing (example)

{-|

@docs example, styles

-}

import Examples.IconExamples as IconExamples
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.AssignmentIcon.V1 as AssignmentIcon
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Icon.V5 as Icon
import Nri.Ui.Svg.V1 as Svg


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.AssignmentIcon.V1"
    , category = Icons
    , content =
        [ (IconExamples.view "Assignment Icons" << List.map (Tuple.mapSecond Svg.toHtml))
            [ ( "diagnostic", AssignmentIcon.diagnostic )
            , ( "practice", AssignmentIcon.practice )
            , ( "quiz", AssignmentIcon.quiz )
            , ( "quickWrite", AssignmentIcon.quickWrite )
            , ( "guidedDraft", AssignmentIcon.guidedDraft )
            , ( "peerReview", AssignmentIcon.peerReview )
            , ( "selfReview", AssignmentIcon.selfReview )
            ]
        ]
    }
