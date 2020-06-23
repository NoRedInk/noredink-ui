module Examples.UiIcon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import AtomicDesignType exposing (AtomicDesignType(..))
import Category exposing (Category(..))
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
type alias State =
    ()


{-| -}
type alias Msg =
    ()


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.UiIcon.V1"
    , categories = List.singleton Icons
    , atomicDesignType = Atom
    , keyboardSupport = []
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , view =
        \_ ->
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
                , ( "copyToClipboard", UiIcon.copyToClipboard )
                , ( "gift", UiIcon.gift )
                ]
            , IconExamples.view "Guidance"
                [ ( "footsteps", UiIcon.footsteps )
                , ( "compass", UiIcon.compass )
                , ( "speedometer", UiIcon.speedometer )
                , ( "bulb", UiIcon.bulb )
                , ( "help", UiIcon.help )
                , ( "checklist", UiIcon.checklist )
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
            , IconExamples.view "Texts"
                [ ( "document", UiIcon.document )
                , ( "newspaper", UiIcon.newspaper )
                , ( "openBook", UiIcon.openBook )
                ]
            , IconExamples.view "Communication"
                [ ( "speechBalloon", UiIcon.speechBalloon )
                ]
            , IconExamples.view "Writing Utensils"
                [ ( "edit", UiIcon.edit )
                , ( "pen", UiIcon.pen )
                ]
            , IconExamples.view "Arrows"
                [ ( "arrowTop", UiIcon.arrowTop )
                , ( "arrowRight", UiIcon.arrowRight )
                , ( "arrowDown", UiIcon.arrowDown )
                , ( "arrowLeft", UiIcon.arrowLeft )
                , ( "arrowPointingRight", UiIcon.arrowPointingRight )
                , ( "arrowPointingRightThick", UiIcon.arrowPointingRightThick )
                ]
            , IconExamples.view "Sticky things"
                [ ( "checkmark", UiIcon.checkmark )
                , ( "checkmarkInCircle", UiIcon.checkmarkInCircle )
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
                [ ( "key", UiIcon.key )
                , ( "lock", UiIcon.lock )
                , ( "premiumLock", UiIcon.premiumLock )
                ]
            , IconExamples.view "Badges & Levels"
                [ ( "badge", UiIcon.badge )
                ]
            , IconExamples.view "Tips & Tricks"
                [ ( "hat", UiIcon.hat )
                , ( "keychain", UiIcon.keychain )
                ]
            , IconExamples.view "Growth"
                [ ( "sprout", UiIcon.sprout )
                , ( "sapling", UiIcon.sapling )
                , ( "tree", UiIcon.tree )
                ]
            , IconExamples.view "Rich Text Formatting"
                [ ( "bold", UiIcon.bold )
                , ( "italic", UiIcon.italic )
                , ( "underline", UiIcon.underline )
                , ( "list", UiIcon.list )
                , ( "link", UiIcon.link )
                , ( "undo", UiIcon.undo )
                , ( "redo", UiIcon.redo )
                ]
            , IconExamples.view "Navigation"
                [ ( "home", UiIcon.home )
                , ( "library", UiIcon.library )
                ]
            ]
    }
