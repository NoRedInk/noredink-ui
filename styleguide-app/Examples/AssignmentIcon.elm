module Examples.AssignmentIcon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import Nri.Ui.AssignmentIcon.V2 as AssignmentIcon


{-| -}
type alias State =
    IconExamples.Settings


{-| -}
type alias Msg =
    IconExamples.Msg


{-| -}
example : Example State Msg
example =
    { name = "AssignmentIcon"
    , version = 2
    , categories = [ Icons ]
    , keyboardSupport = []
    , state = IconExamples.init
    , update = IconExamples.update
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
        \ellieLinkConfig settings ->
            let
                viewExampleSection =
                    IconExamples.view settings
            in
            [ IconExamples.viewSettings settings
            , viewExampleSection "Diagnostic"
                [ ( "diagnostic", AssignmentIcon.diagnostic )
                , ( "planningDiagnosticCircled", AssignmentIcon.planningDiagnosticCircled )
                , ( "unitDiagnosticCircled", AssignmentIcon.unitDiagnosticCircled )
                ]
            , viewExampleSection "Practice" <|
                [ ( "practice", AssignmentIcon.practice )
                , ( "practiceCircled", AssignmentIcon.practiceCircled )
                ]
            , viewExampleSection "Quiz" <|
                [ ( "quiz", AssignmentIcon.quiz )
                , ( "quizCircled", AssignmentIcon.quizCircled )
                , ( "passageQuizCircled", AssignmentIcon.passageQuizCircled )
                ]
            , viewExampleSection "Writing" <|
                [ ( "quickWrite", AssignmentIcon.quickWrite )
                , ( "guidedDraft", AssignmentIcon.guidedDraft )
                , ( "peerReview", AssignmentIcon.peerReview )
                , ( "selfReview", AssignmentIcon.selfReview )
                ]
            , viewExampleSection "Writing II" <|
                [ ( "quickWriteCircled", AssignmentIcon.quickWriteCircled )
                , ( "guidedDraftCircled", AssignmentIcon.guidedDraftCircled )
                , ( "peerReviewCircled", AssignmentIcon.peerReviewCircled )
                , ( "selfReviewCircled", AssignmentIcon.selfReviewCircled )
                ]
            , viewExampleSection "Stages" <|
                [ ( "submitting", AssignmentIcon.submitting )
                , ( "rating", AssignmentIcon.rating )
                , ( "revising", AssignmentIcon.revising )
                ]
            , viewExampleSection "Start" <|
                [ ( "startPrimary", AssignmentIcon.startPrimary )
                , ( "startSecondary", AssignmentIcon.startSecondary )
                ]
            , viewExampleSection "Activities" <|
                [ ( "assessment", AssignmentIcon.assessment )
                , ( "standards", AssignmentIcon.standards )
                , ( "writing", AssignmentIcon.writing )
                , ( "modules", AssignmentIcon.modules )
                ]
            ]
    }
