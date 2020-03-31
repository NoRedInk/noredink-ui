module Examples.AssignmentIcon exposing (example)

{-|

@docs example

-}

import Category exposing (Category(..))
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import Nri.Ui.AssignmentIcon.V1 as AssignmentIcon
import Nri.Ui.Icon.V5 as Icon


{-| -}
example : Example () ()
example =
    { name = "Nri.Ui.AssignmentIcon.V1"
    , categories = [ Icons ]
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , view =
        \_ ->
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
