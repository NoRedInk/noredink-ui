module Examples.AssignmentIcon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Example exposing (Example)
import IconExamples as IconExamples exposing (Group)
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
        , ( "unitDiagnosticCircled", AssignmentIcon.unitDiagnosticCircled, [] )
        ]
      )
    , ( "Practice"
      , [ ( "practice", AssignmentIcon.practice, [] )
        , ( "practiceCircled", AssignmentIcon.practiceCircled, [] )
        ]
      )
    , ( "Quiz"
      , [ ( "quiz", AssignmentIcon.quiz, [] )
        , ( "quizCircled", AssignmentIcon.quizCircled, [] )
        , ( "passageQuizCircled", AssignmentIcon.passageQuizCircled, [] )
        ]
      )
    , ( "Writing"
      , [ ( "quickWrite", AssignmentIcon.quickWrite, [] )
        , ( "guidedDraft", AssignmentIcon.guidedDraft, [] )
        , ( "peerReview", AssignmentIcon.peerReview, [] )
        , ( "selfReview", AssignmentIcon.selfReview, [] )
        , ( "dailyWriting", AssignmentIcon.dailyWriting, [] )
        , ( "novels", AssignmentIcon.novels, [] )
        ]
      )
    , ( "Writing II"
      , [ ( "quickWriteCircled", AssignmentIcon.quickWriteCircled, [] )
        , ( "guidedDraftCircled", AssignmentIcon.guidedDraftCircled, [] )
        , ( "peerReviewCircled", AssignmentIcon.peerReviewCircled, [] )
        , ( "selfReviewCircled", AssignmentIcon.selfReviewCircled, [] )
        , ( "gradingAssistantCircled", AssignmentIcon.gradingAssistantCircled, [] )
        , ( "dailyWritingCircled", AssignmentIcon.dailyWritingCircled, [] )
        , ( "novelsCircled", AssignmentIcon.novelsCircled, [] )
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
        , ( "startSecondary", AssignmentIcon.startSecondary, [] )
        ]
      )
    , ( "Activities"
      , [ ( "assessment", AssignmentIcon.assessment, [] )
        , ( "standards", AssignmentIcon.standards, [] )
        , ( "writing", AssignmentIcon.writing, [] )
        , ( "modules", AssignmentIcon.modules, [] )
        ]
      )
    ]
