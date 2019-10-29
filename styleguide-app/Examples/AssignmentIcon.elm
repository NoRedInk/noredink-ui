module Examples.AssignmentIcon exposing (example)

{-|

@docs example, styles

-}

import Examples.IconExamples as IconExamples
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.AssignmentIcon.V1 as AssignmentIcon
import Nri.Ui.Icon.V5 as Icon


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.AssignmentIcon.V1"
    , category = Icons
    , content =
        [ IconExamples.view "Quiz engine icons"
            [ ( "diagnostic", AssignmentIcon.diagnostic )
            , ( "practice", AssignmentIcon.practice )
            , ( "quiz", AssignmentIcon.quiz )
            ]
        , IconExamples.view "Writing assignment icons"
            [ ( "quickWrite", AssignmentIcon.quickWrite )
            , ( "guidedDraft", AssignmentIcon.guidedDraft )
            , ( "peerReview", AssignmentIcon.peerReview )
            , ( "selfReview", AssignmentIcon.selfReview )
            ]
        , IconExamples.view "Peer Review sub-assignment icons"
            [ ( "submitting", AssignmentIcon.submitting )
            , ( "rating", AssignmentIcon.rating )
            , ( "revising", AssignmentIcon.revising )
            ]
        ]
    }
