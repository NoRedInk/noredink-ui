module Examples.AssignmentIcon exposing (example)

{-|

@docs example, styles

-}

import Examples.IconHelpers exposing (viewIconSection)
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
        [ viewIconSection "Assignment Icons"
            [ ( "diagnostic", Svg.toHtml AssignmentIcon.diagnostic )
            , ( "practice", Svg.toHtml AssignmentIcon.practice )
            , ( "quiz", Svg.toHtml AssignmentIcon.quiz )
            , ( "quickWrite", Svg.toHtml AssignmentIcon.quickWrite )
            , ( "guidedDraft", Svg.toHtml AssignmentIcon.guidedDraft )
            , ( "peerReview", Svg.toHtml AssignmentIcon.peerReview )
            , ( "selfReview", Svg.toHtml AssignmentIcon.selfReview )
            ]
        , viewIconSection "Peer Review"
            [ ( "submitting", Svg.toHtml AssignmentIcon.submitting )
            ]
        ]
    }
