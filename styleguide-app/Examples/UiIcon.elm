module Examples.UiIcon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
type alias State =
    IconExamples.Settings


{-| -}
type alias Msg =
    IconExamples.Msg


{-| -}
example : Example State Msg
example =
    { name = "UiIcon"
    , version = 1
    , categories = List.singleton Icons
    , keyboardSupport = []
    , state = IconExamples.init
    , update = IconExamples.update
    , subscriptions = \_ -> Sub.none
    , preview =
        IconExamples.preview
            [ UiIcon.seeMore
            , UiIcon.archive
            , UiIcon.share
            , UiIcon.footsteps
            , UiIcon.person
            , UiIcon.calendar
            , UiIcon.missingDocument
            , UiIcon.speechBalloon
            , UiIcon.edit
            , UiIcon.arrowTop
            , UiIcon.checkmark
            , UiIcon.equals
            ]
    , view =
        \settings ->
            let
                viewExampleSection =
                    IconExamples.view settings
            in
            [ IconExamples.viewSettings settings
            , viewExampleSection "Interface"
                [ ( "seeMore", UiIcon.seeMore )
                , ( "openClose", UiIcon.openClose )
                , ( "download", UiIcon.download )
                , ( "sort", UiIcon.sort )
                , ( "gear", UiIcon.gear )
                , ( "sortArrow", UiIcon.sortArrow )
                ]
            , viewExampleSection "Archive & Unarchive"
                [ ( "archive", UiIcon.archive )
                , ( "unarchive", UiIcon.unarchive )
                ]
            , viewExampleSection "Media"
                [ ( "playInCircle", UiIcon.playInCircle )
                , ( "pauseInCircle", UiIcon.pauseInCircle )
                , ( "stopInCircle", UiIcon.stopInCircle )
                , ( "skip", UiIcon.skip )
                ]
            , viewExampleSection "Actions"
                [ ( "share", UiIcon.share )
                , ( "preview", UiIcon.preview )
                , ( "activity", UiIcon.activity )
                , ( "copyToClipboard", UiIcon.copyToClipboard )
                , ( "gift", UiIcon.gift )
                , ( "openInNewTab", UiIcon.openInNewTab )
                , ( "sync", UiIcon.sync )
                ]
            , viewExampleSection "Guidance"
                [ ( "footsteps", UiIcon.footsteps )
                , ( "bulb", UiIcon.bulb )
                , ( "help", UiIcon.help )
                , ( "checklist", UiIcon.checklist )
                ]
            , viewExampleSection "Science & Measurement"
                [ ( "compass", UiIcon.compass )
                , ( "speedometer", UiIcon.speedometer )
                , ( "performance", UiIcon.performance )
                , ( "microscope", UiIcon.microscope )
                , ( "scale", UiIcon.scale )
                ]
            , viewExampleSection "Humans & Class"
                [ ( "person", UiIcon.person )
                , ( "couple", UiIcon.couple )
                , ( "class", UiIcon.class )
                , ( "leaderboard", UiIcon.leaderboard )
                ]
            , viewExampleSection "Time"
                [ ( "calendar", UiIcon.calendar )
                , ( "clock", UiIcon.clock )
                ]
            , viewExampleSection "Texts"
                [ ( "missingDocument", UiIcon.missingDocument )
                , ( "document", UiIcon.document )
                , ( "documents", UiIcon.documents )
                , ( "newspaper", UiIcon.newspaper )
                , ( "openBook", UiIcon.openBook )
                , ( "openBooks", UiIcon.openBooks )
                ]
            , viewExampleSection "Communication"
                [ ( "speechBalloon", UiIcon.speechBalloon )
                , ( "mail", UiIcon.mail )
                ]
            , viewExampleSection "Writing Utensils"
                [ ( "edit", UiIcon.edit )
                , ( "pen", UiIcon.pen )
                , ( "highlighter", UiIcon.highlighter )
                ]
            , viewExampleSection "Arrows"
                [ ( "arrowTop", UiIcon.arrowTop )
                , ( "arrowRight", UiIcon.arrowRight )
                , ( "arrowDown", UiIcon.arrowDown )
                , ( "arrowLeft", UiIcon.arrowLeft )
                , ( "arrowPointingRight", UiIcon.arrowPointingRight )
                , ( "arrowPointingRightThick", UiIcon.arrowPointingRightThick )
                ]
            , viewExampleSection "Sticky things"
                [ ( "checkmark", UiIcon.checkmark )
                , ( "checkmarkInCircle", UiIcon.checkmarkInCircle )
                , ( "x", UiIcon.x )
                , ( "attention", UiIcon.attention )
                , ( "exclamation", UiIcon.exclamation )
                ]
            , viewExampleSection "Math"
                [ ( "equals", UiIcon.equals )
                , ( "plus", UiIcon.plus )
                , ( "null", UiIcon.null )
                ]
            , viewExampleSection "Notifs"
                [ ( "flag", UiIcon.flag )
                , ( "star", UiIcon.star )
                , ( "starFilled", UiIcon.starFilled )
                , ( "starOutline", UiIcon.starOutline )
                ]
            , viewExampleSection "Badges & Celebration"
                [ ( "badge", UiIcon.badge )
                , ( "tada", UiIcon.tada )
                ]
            , viewExampleSection "Lock & Key"
                [ ( "key", UiIcon.key )
                , ( "lock", UiIcon.lock )
                , ( "premiumLock", UiIcon.premiumLock )
                ]
            , viewExampleSection "Tips & Tricks"
                [ ( "hat", UiIcon.hat )
                , ( "keychain", UiIcon.keychain )
                ]
            , viewExampleSection "Growth"
                [ ( "sprout", UiIcon.sprout )
                , ( "sapling", UiIcon.sapling )
                , ( "tree", UiIcon.tree )
                ]
            , viewExampleSection "Rich Text Formatting"
                [ ( "bold", UiIcon.bold )
                , ( "italic", UiIcon.italic )
                , ( "underline", UiIcon.underline )
                , ( "list", UiIcon.list )
                , ( "link", UiIcon.link )
                , ( "undo", UiIcon.undo )
                , ( "redo", UiIcon.redo )
                ]
            , viewExampleSection "Punctuation"
                [ ( "openQuotationMark", UiIcon.openQuotationMark )
                , ( "closeQuotationMark", UiIcon.closeQuotationMark )
                ]
            , viewExampleSection "Navigation"
                [ ( "home", UiIcon.home )
                , ( "library", UiIcon.library )
                ]
            , viewExampleSection "Search"
                [ ( "search", UiIcon.search )
                , ( "searchInCircle", UiIcon.searchInCicle )
                ]
            ]
    }
