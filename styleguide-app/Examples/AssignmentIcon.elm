module Examples.AssignmentIcon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.AssignmentIcon.V2 as AssignmentIcon


{-| -}
type alias State =
    ()


{-| -}
type alias Msg =
    ()


{-| -}
example : Example State Msg
example =
    { name = "AssignmentIcon"
    , version = 2
    , categories = [ Icons ]
    , keyboardSupport = []
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , preview =
        IconExamples.preview
            [ AssignmentIcon.planningDiagnosticCircled
            , AssignmentIcon.unitDiagnosticCircled
            , AssignmentIcon.practiceCircled
            , AssignmentIcon.quizCircled
            , AssignmentIcon.quickWriteCircled
            , AssignmentIcon.guidedDraftCircled
            , AssignmentIcon.peerReviewCircled
            , AssignmentIcon.selfReviewCircled
            , AssignmentIcon.startPrimary
            , AssignmentIcon.assessment
            , AssignmentIcon.standards
            , AssignmentIcon.writing
            ]
    , view =
        \_ ->
            [ IconExamples.view "Diagnostic"
                [ ( "diagnostic", AssignmentIcon.diagnostic )
                , ( "planningDiagnosticCircled", AssignmentIcon.planningDiagnosticCircled )
                , ( "unitDiagnosticCircled", AssignmentIcon.unitDiagnosticCircled )
                ]
            , IconExamples.view "Practice"
                [ ( "practice", AssignmentIcon.practice )
                , ( "practiceCircled", AssignmentIcon.practiceCircled )
                ]
            , IconExamples.view "Quiz"
                [ ( "quiz", AssignmentIcon.quiz )
                , ( "quizCircled", AssignmentIcon.quizCircled )
                , ( "passageQuizCircled", AssignmentIcon.passageQuizCircled )
                ]
            , IconExamples.view "Writing"
                [ ( "quickWrite", AssignmentIcon.quickWrite )
                , ( "guidedDraft", AssignmentIcon.guidedDraft )
                , ( "peerReview", AssignmentIcon.peerReview )
                , ( "selfReview", AssignmentIcon.selfReview )
                ]
            , IconExamples.view "Writing II"
                [ ( "quickWriteCircled", AssignmentIcon.quickWriteCircled )
                , ( "guidedDraftCircled", AssignmentIcon.guidedDraftCircled )
                , ( "peerReviewCircled", AssignmentIcon.peerReviewCircled )
                , ( "selfReviewCircled", AssignmentIcon.selfReviewCircled )
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
            , IconExamples.view "Activities"
                [ ( "assessment", AssignmentIcon.assessment )
                , ( "standards", AssignmentIcon.standards )
                , ( "writing", AssignmentIcon.writing )
                ]
            ]
    }
