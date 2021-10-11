module Examples.UiIcon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

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
    { name = "UiIcon"
    , version = 1
    , categories = List.singleton Icons
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
            , IconExamples.view "Archive & Unarchive"
                [ ( "archive", UiIcon.archive )
                , ( "unarchive", UiIcon.unarchive )
                ]
            , IconExamples.view "Actions"
                [ ( "share", UiIcon.share )
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
                , ( "couple", UiIcon.couple )
                , ( "class", UiIcon.class )
                , ( "leaderboard", UiIcon.leaderboard )
                , ( "performance", UiIcon.performance )
                ]
            , IconExamples.view "Time"
                [ ( "calendar", UiIcon.calendar )
                , ( "clock", UiIcon.clock )
                ]
            , IconExamples.view "Texts"
                [ ( "missingDocument", UiIcon.missingDocument )
                , ( "document", UiIcon.document )
                , ( "documents", UiIcon.documents )
                , ( "newspaper", UiIcon.newspaper )
                , ( "openBook", UiIcon.openBook )
                , ( "openBooks", UiIcon.openBooks )
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
            , IconExamples.view "Math"
                [ ( "equals", UiIcon.equals )
                , ( "plus", UiIcon.plus )
                , ( "null", UiIcon.null )
                ]
            , IconExamples.view "Notifs"
                [ ( "flag", UiIcon.flag )
                , ( "star", UiIcon.star )
                , ( "starFilled", UiIcon.starFilled )
                , ( "starOutline", UiIcon.starOutline )
                ]
            , IconExamples.view "Badges & Levels"
                [ ( "badge", UiIcon.badge )
                ]
            , IconExamples.view "Lock & Key"
                [ ( "key", UiIcon.key )
                , ( "lock", UiIcon.lock )
                , ( "premiumLock", UiIcon.premiumLock )
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
            , IconExamples.view "Search"
                [ ( "search", UiIcon.search )
                , ( "searchInCircle", UiIcon.searchInCicle )
                ]
            ]
    }
