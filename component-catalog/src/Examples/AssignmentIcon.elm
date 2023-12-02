module Examples.AssignmentIcon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Example exposing (Example)
import Examples.IconExamples as IconExamples exposing (Group)
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
    { moduleName = "AssignmentIcon"
    , version = 2
    , label = "Planning Diagnostics"
    , name = "planningDiagnosticCircled"
    , icon = AssignmentIcon.planningDiagnosticCircled
    , renderSvgCode = \name -> "AssignmentIcon." ++ name
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
    , all = all
    }
        |> IconExamples.example


all : List Group
all =
    [ ( "Diagnostic"
      , [ ( "diagnostic", AssignmentIcon.diagnostic, [] )
        , ( "planningDiagnosticCircled", AssignmentIcon.planningDiagnosticCircled, [] )
        , ( "planningDiagnosticCircledX", AssignmentIcon.planningDiagnosticCircledX, [] )
        , ( "unitDiagnosticCircled", AssignmentIcon.unitDiagnosticCircled, [] )
        , ( "unitDiagnosticCircledX", AssignmentIcon.unitDiagnosticCircledX, [] )
        ]
      )
    , ( "Practice"
      , [ ( "practice", AssignmentIcon.practice, [] )
        , ( "practiceCircled", AssignmentIcon.practiceCircled, [] )
        , ( "practiceCircledX", AssignmentIcon.practiceCircledX, [] )
        ]
      )
    , ( "Quiz"
      , [ ( "quiz", AssignmentIcon.quiz, [] )
        , ( "quizCircled", AssignmentIcon.quizCircled, [] )
        , ( "quizCircledX", AssignmentIcon.quizCircledX, [] )
        , ( "passageQuizCircled", AssignmentIcon.passageQuizCircled, [] )
        , ( "passageQuizCircledX", AssignmentIcon.passageQuizCircledX, [] )
        ]
      )
    , ( "Writing"
      , [ ( "quickWrite", AssignmentIcon.quickWrite, [] )
        , ( "guidedDraft", AssignmentIcon.guidedDraft, [] )
        , ( "peerReview", AssignmentIcon.peerReview, [] )
        , ( "selfReview", AssignmentIcon.selfReview, [] )
        , ( "dailyWriting", AssignmentIcon.dailyWriting, [] )
        ]
      )
    , ( "Writing II"
      , [ ( "quickWriteCircled", AssignmentIcon.quickWriteCircled, [] )
      , ( "quickWriteCircledX", AssignmentIcon.quickWriteCircledX, [] )
        , ( "guidedDraftCircled", AssignmentIcon.guidedDraftCircled, [] )
        , ( "guidedDraftCircledX", AssignmentIcon.guidedDraftCircledX, [] )
        , ( "peerReviewCircled", AssignmentIcon.peerReviewCircled, [] )
        , ( "peerReviewCircledX", AssignmentIcon.peerReviewCircledX, [] )
        , ( "selfReviewCircled", AssignmentIcon.selfReviewCircled, [] )
        , ( "selfReviewCircledX", AssignmentIcon.selfReviewCircledX, [] )
        , ( "gradingAssistantCircled", AssignmentIcon.gradingAssistantCircled, [] )
        , ( "gradingAssistantCircledX", AssignmentIcon.gradingAssistantCircledX, [] )
        , ( "dailyWritingCircled", AssignmentIcon.dailyWritingCircled, [] )
        , ( "dailyWritingCircledX", AssignmentIcon.dailyWritingCircledX, [] )
        ]
      )
    , ( "Stages"
      , [ ( "submitting", AssignmentIcon.submitting, [] )
        , ( "rating", AssignmentIcon.rating, [] )
        , ( "revising", AssignmentIcon.revising, [] )
        ]
      )
    , ( "Start"
      , [ ( "startPrimary", AssignmentIcon.startPrimary, [] )
        , ( "startPrimaryX", AssignmentIcon.startPrimaryX, [] )
        , ( "startSecondary", AssignmentIcon.startSecondary, [] )
        ]
      )
    , ( "Activities"
      , [ ( "assessment", AssignmentIcon.assessment, [] )
      , ( "assessmentX", AssignmentIcon.assessmentX, [] )
        , ( "standards", AssignmentIcon.standards, [] )
        -- , ( "standardsX", AssignmentIcon.standardsX, [] )
        , ( "writing", AssignmentIcon.writing, [] )
        , ( "writingX", AssignmentIcon.writingX, [] )
        , ( "modules", AssignmentIcon.modules, [] )
        , ( "modulesX", AssignmentIcon.modulesX, [] )
        ]
      )
    ]
