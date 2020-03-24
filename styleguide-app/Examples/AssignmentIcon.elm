module Examples.AssignmentIcon exposing (example)

{-|

@docs example, styles

-}

import Category exposing (Category(..))
import Examples.IconExamples as IconExamples
import ModuleExample exposing (ModuleExample)
import Nri.Ui.AssignmentIcon.V1 as AssignmentIcon
import Nri.Ui.Icon.V5 as Icon
import Sort.Set as Set exposing (Set)


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.AssignmentIcon.V1"
    , categories = Set.fromList Category.sorter <| List.singleton Icons
    , content =
        [ IconExamples.view "Quiz engine"
            [ ( "diagnostic", AssignmentIcon.diagnostic )
            , ( "practice", AssignmentIcon.practice )
            , ( "quiz", AssignmentIcon.quiz )
            ]
        , IconExamples.view "Writing"
            [ ( "quickWrite", AssignmentIcon.quickWrite )
            , ( "guidedDraft", AssignmentIcon.guidedDraft )
            , ( "peerReview", AssignmentIcon.peerReview )
            , ( "selfReview", AssignmentIcon.selfReview )
            ]
        , IconExamples.view "Stages"
            [ ( "submitting", AssignmentIcon.submitting )
            , ( "rating", AssignmentIcon.rating )
            , ( "revising", AssignmentIcon.revising )
            ]
        , IconExamples.view "Start"
            [ ( "startPrimary", AssignmentIcon.startPrimary )
            , ( "startSecondary", AssignmentIcon.startSecondary )
            ]
        ]
    }
