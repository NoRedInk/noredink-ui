module Examples.UiIcon exposing (example)

{-|

@docs example, styles

-}

import Examples.IconExamples as IconExamples
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.UiIcon.V1"
    , category = Icons
    , content =
        [ IconExamples.view "Interface"
            [ ( "seeMore", UiIcon.seeMore )
            , ( "openClose", UiIcon.openClose )
            , ( "download", UiIcon.download )
            , ( "sort", UiIcon.sort )
            , ( "gear", UiIcon.gear )
            , ( "sortArrow", UiIcon.sortArrow )
            ]
        , IconExamples.view "Actions"
            [ ( "unarchive", UiIcon.unarchive )
            , ( "share", UiIcon.share )
            , ( "preview", UiIcon.preview )
            , ( "activity", UiIcon.activity )
            , ( "skip", UiIcon.skip )
            ]
        , IconExamples.view "Guidance"
            [ ( "footsteps", UiIcon.footsteps )
            , ( "compass", UiIcon.compass )
            , ( "speedometer", UiIcon.speedometer )
            , ( "bulb", UiIcon.bulb )
            , ( "help", UiIcon.help )
            ]
        , IconExamples.view "Humans & Class"
            [ ( "person", UiIcon.person )
            , ( "class", UiIcon.class )
            , ( "leaderboard", UiIcon.leaderboard )
            , ( "performance", UiIcon.performance )
            ]
        , IconExamples.view "Time"
            [ ( "calendar", UiIcon.calendar )
            , ( "clock", UiIcon.clock )
            ]
        , IconExamples.view "Writing & Writing Utensils"
            [ ( "document", UiIcon.document )
            , ( "newspaper", UiIcon.newspaper )
            , ( "edit", UiIcon.edit )
            , ( "pen", UiIcon.pen )
            ]
        , IconExamples.view "Arrows"
            [ ( "arrowTop", UiIcon.arrowTop )
            , ( "arrowRight", UiIcon.arrowRight )
            , ( "arrowDown", UiIcon.arrowDown )
            , ( "arrowLeft", UiIcon.arrowLeft )
            , ( "arrowPointingRight", UiIcon.arrowPointingRight )
            ]
        , IconExamples.view "Sticky things"
            [ ( "checkmark", UiIcon.checkmark )
            , ( "x", UiIcon.x )
            , ( "attention", UiIcon.attention )
            , ( "exclamation", UiIcon.exclamation )
            ]
        , IconExamples.view "Notifs"
            [ ( "flag", UiIcon.flag )
            , ( "star", UiIcon.star )
            , ( "starOutline", UiIcon.starOutline )
            ]
        , IconExamples.view "Math"
            [ ( "equals", UiIcon.equals )
            , ( "plus", UiIcon.plus )
            ]
        , IconExamples.view "Lock & Key"
            [ ( "lock", UiIcon.lock )
            , ( "key", UiIcon.key )
            ]
        , IconExamples.view "Badges & Levels"
            [ ( "badge", UiIcon.badge )
            ]
        ]
    }
