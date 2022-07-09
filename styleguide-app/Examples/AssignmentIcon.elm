module Examples.AssignmentIcon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Example exposing (Example)
import Examples.IconExamples as IconExamples
    exposing
        ( IconExampleGroup
        , viewByGroupWithSettings
        )
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
    , state =
        IconExamples.init
            { label = "Planning Diagnostics"
            , name = "planningDiagnosticCircled"
            , icon = AssignmentIcon.planningDiagnosticCircled
            , renderSvgCode = \name -> "AssignmentIcon." ++ name
            }
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
    , view = \_ settings -> viewByGroupWithSettings settings all
    }


all : List IconExampleGroup
all =
    [ ( "Diagnostic"
      , [ ( "diagnostic", AssignmentIcon.diagnostic )
        , ( "planningDiagnosticCircled", AssignmentIcon.planningDiagnosticCircled )
        , ( "unitDiagnosticCircled", AssignmentIcon.unitDiagnosticCircled )
        ]
      )
    , ( "Practice"
      , [ ( "practice", AssignmentIcon.practice )
        , ( "practiceCircled", AssignmentIcon.practiceCircled )
        ]
      )
    , ( "Quiz"
      , [ ( "quiz", AssignmentIcon.quiz )
        , ( "quizCircled", AssignmentIcon.quizCircled )
        , ( "passageQuizCircled", AssignmentIcon.passageQuizCircled )
        ]
      )
    , ( "Writing"
      , [ ( "quickWrite", AssignmentIcon.quickWrite )
        , ( "guidedDraft", AssignmentIcon.guidedDraft )
        , ( "peerReview", AssignmentIcon.peerReview )
        , ( "selfReview", AssignmentIcon.selfReview )
        ]
      )
    , ( "Writing II"
      , [ ( "quickWriteCircled", AssignmentIcon.quickWriteCircled )
        , ( "guidedDraftCircled", AssignmentIcon.guidedDraftCircled )
        , ( "peerReviewCircled", AssignmentIcon.peerReviewCircled )
        , ( "selfReviewCircled", AssignmentIcon.selfReviewCircled )
        ]
      )
    , ( "Stages"
      , [ ( "submitting", AssignmentIcon.submitting )
        , ( "rating", AssignmentIcon.rating )
        , ( "revising", AssignmentIcon.revising )
        ]
      )
    , ( "Start"
      , [ ( "startPrimary", AssignmentIcon.startPrimary )
        , ( "startSecondary", AssignmentIcon.startSecondary )
        ]
      )
    , ( "Activities"
      , [ ( "assessment", AssignmentIcon.assessment )
        , ( "standards", AssignmentIcon.standards )
        , ( "writing", AssignmentIcon.writing )
        , ( "modules", AssignmentIcon.modules )
        ]
      )
    ]
