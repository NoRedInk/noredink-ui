module Examples.UiIcon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Example exposing (Example)
import Examples.IconExamples as IconExamples exposing (Group)
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
    { moduleName = "UiIcon"
    , version = 1
    , label = "Mastered"
    , name = "starFilled"
    , icon = UiIcon.starFilled
    , renderSvgCode = \name -> "UiIcon." ++ name
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
    , all = all
    }
        |> IconExamples.example


all : List Group
all =
    [ ( "Interface"
      , [ ( "seeMore", UiIcon.seeMore, [] )
        , ( "openClose", UiIcon.openClose, [] )
        , ( "download", UiIcon.download, [] )
        , ( "sort", UiIcon.sort, [] )
        , ( "gear", UiIcon.gear, [] )
        , ( "hamburger", UiIcon.hamburger, [] )
        , ( "kebab", UiIcon.kebab, [] )
        ]
      )
    , ( "Archive & Unarchive"
      , [ ( "archive", UiIcon.archive, [] )
        , ( "unarchive", UiIcon.unarchive, [] )
        ]
      )
    , ( "Media in Circles"
      , [ ( "playInCircle", UiIcon.playInCircle, [] )
        , ( "pauseInCircle", UiIcon.pauseInCircle, [] )
        , ( "stopInCircle", UiIcon.stopInCircle, [] )
        , ( "speaker", UiIcon.speaker, [] )
        , ( "mutedSpeaker", UiIcon.mutedSpeaker, [] )
        ]
      )
    , ( "Media"
      , [ ( "play", UiIcon.play, [] )
        , ( "skip", UiIcon.skip, [] )
        ]
      )
    , ( "Actions"
      , [ ( "share", UiIcon.share, [] )
        , ( "preview", UiIcon.preview, [] )
        , ( "activity", UiIcon.activity, [] )
        , ( "print", UiIcon.print, [] )
        , ( "copyToClipboard", UiIcon.copyToClipboard, [] )
        , ( "gift", UiIcon.gift, [] )
        , ( "openInNewTab", UiIcon.openInNewTab, [] )
        , ( "sync", UiIcon.sync, [] )
        ]
      )
    , ( "Guidance"
      , [ ( "footsteps", UiIcon.footsteps, [] )
        , ( "footstepsVideo", UiIcon.footstepsVideo, [] )
        , ( "help", UiIcon.help, [] )
        , ( "info", UiIcon.info, [] )
        , ( "checklist", UiIcon.checklist, [] )
        , ( "checklistComplete", UiIcon.checklistComplete, [] )
        ]
      )
    , ( "Bulbs"
      , [ ( "sparkleBulb", UiIcon.sparkleBulb, [] )
        , ( "baldBulb", UiIcon.baldBulb, [] )
        ]
      )
    , ( "Science & Measurement"
      , [ ( "compass", UiIcon.compass, [] )
        , ( "speedometer", UiIcon.speedometer, [] )
        , ( "performance", UiIcon.performance, [] )
        , ( "microscope", UiIcon.microscope, [] )
        , ( "scale", UiIcon.scale, [] )
        , ( "brain", UiIcon.brain, [] )
        ]
      )
    , ( "Humans & Class"
      , [ ( "person", UiIcon.person, [] )
        , ( "couple", UiIcon.couple, [] )
        , ( "class", UiIcon.class, [] )
        , ( "leaderboard", UiIcon.leaderboard, [] )
        , ( "stretch", UiIcon.stretch, [] )
        , ( "graduateCap", UiIcon.graduateCap, [] )
        , ( "apple", UiIcon.apple, [] )
        , ( "appleOutline", UiIcon.appleOutline, [] )
        , ( "briefcase", UiIcon.briefcase, [] )
        , ( "heapOfBoxes", UiIcon.heapOfBoxes, [] )
        , ( "climbingStairs", UiIcon.climbingStairs, [] )
        , ( "twoHandsHoldingBox", UiIcon.twoHandsHoldingBox, [] )
        , ( "atlas", UiIcon.atlas, [] )
        ]
      )
    , ( "Time"
      , [ ( "emptyCalendar", UiIcon.emptyCalendar, [] )
        , ( "calendar", UiIcon.calendar, [] )
        , ( "clock", UiIcon.clock, [] )
        ]
      )
    , ( "Texts"
      , [ ( "missingDocument", UiIcon.missingDocument, [] )
        , ( "document", UiIcon.document, [] )
        , ( "documents", UiIcon.documents, [] )
        , ( "newspaper", UiIcon.newspaper, [] )
        , ( "openBook", UiIcon.openBook, [] )
        , ( "openBooks", UiIcon.openBooks, [] )
        ]
      )
    , ( "Communication"
      , [ ( "speechBalloon", UiIcon.speechBalloon, [] )
        , ( "projectorScreen", UiIcon.projectorScreen, [] )
        , ( "mail", UiIcon.mail, [] )
        ]
      )
    , ( "Writing Implements"
      , [ ( "edit", UiIcon.edit, [] )
        , ( "pen", UiIcon.pen, [] )
        , ( "highlighter", UiIcon.highlighter, [] )
        , ( "eraser", UiIcon.eraser, [] )
        ]
      )
    , ( "Arrows"
      , [ ( "arrowTop", UiIcon.arrowTop, [] )
        , ( "arrowRight", UiIcon.arrowRight, [] )
        , ( "arrowDown", UiIcon.arrowDown, [] )
        , ( "arrowLeft", UiIcon.arrowLeft, [] )
        , ( "arrowPointingRight", UiIcon.arrowPointingRight, [] )
        , ( "arrowPointingRightThick", UiIcon.arrowPointingRightThick, [] )
        , ( "sortArrow", UiIcon.sortArrow, [] )
        , ( "sortArrowDown", UiIcon.sortArrowDown, [] )
        ]
      )
    , ( "Checks"
      , [ ( "checkmark", UiIcon.checkmark, [] )
        , ( "checkmarkInCircle", UiIcon.checkmarkInCircle, [] )
        , ( "checkmarkInCircleInverse", UiIcon.checkmarkInCircleInverse, [] )
        , ( "emptyCircle", UiIcon.emptyCircle, [] )
        ]
      )
    , ( "Xs"
      , [ ( "x", UiIcon.x, [] )
        , ( "xInCircle", UiIcon.xInCircle, [] )
        ]
      )
    , ( "Bangs"
      , [ ( "attention", UiIcon.attention, [] )
        , ( "exclamation", UiIcon.exclamation, [] )
        ]
      )
    , ( "Math"
      , [ ( "equals", UiIcon.equals, [] )
        , ( "plus", UiIcon.plus, [] )
        , ( "null", UiIcon.null, [] )
        ]
      )
    , ( "Notifs"
      , [ ( "flag", UiIcon.flag, [] )
        , ( "star", UiIcon.star, [] )
        , ( "starFilled", UiIcon.starFilled, [] )
        , ( "starOutline", UiIcon.starOutline, [] )
        , ( "no", UiIcon.no, [] )
        ]
      )
    , ( "Badges & Celebration"
      , [ ( "badge", UiIcon.badge, [] )
        , ( "tada", UiIcon.tada, [] )
        , ( "count", UiIcon.count, [] )
        ]
      )
    , ( "Lock & Key"
      , [ ( "key", UiIcon.key, [] )
        , ( "lock", UiIcon.lock, [] )
        , ( "premiumLock", UiIcon.premiumLock, [] )
        ]
      )
    , ( "Tips & Tricks"
      , [ ( "hat", UiIcon.hat, [] )
        , ( "keychain", UiIcon.keychain, [] )
        ]
      )
    , ( "Growth"
      , [ ( "sprout", UiIcon.sprout, [] )
        , ( "sapling", UiIcon.sapling, [] )
        , ( "tree", UiIcon.tree, [] )
        ]
      )
    , ( "Rich Text Formatting"
      , [ ( "bold", UiIcon.bold, [] )
        , ( "italic", UiIcon.italic, [] )
        , ( "underline", UiIcon.underline, [] )
        , ( "list", UiIcon.list, [] )
        , ( "link", UiIcon.link, [] )
        , ( "undo", UiIcon.undo, [] )
        , ( "redo", UiIcon.redo, [] )
        ]
      )
    , ( "Punctuation"
      , [ ( "openQuotationMark", UiIcon.openQuotationMark, [] )
        , ( "closeQuotationMark", UiIcon.closeQuotationMark, [] )
        ]
      )
    , ( "Navigation"
      , [ ( "home", UiIcon.home, [] )
        , ( "homeInCircle", UiIcon.homeInCircle, [] )
        , ( "library", UiIcon.library, [] )
        ]
      )
    , ( "Search"
      , [ ( "search", UiIcon.search, [] )
        , ( "searchInCircle", UiIcon.searchInCicle, [] )
        ]
      )
    , ( "School Category"
      , [ ( "school", UiIcon.school, [] )
        , ( "highSchool", UiIcon.highSchool, [] )
        , ( "company", UiIcon.company, [] )
        , ( "homeSchool", UiIcon.homeSchool, [] )
        ]
      )
    , ( "Location"
      , [ ( "flagUs", UiIcon.flagUs, [] )
        , ( "globe", UiIcon.globe, [] )
        ]
      )
    , ( "Grading Assistant"
      , [ ( "gradingAssistant", UiIcon.gradingAssistant, [] )
        , ( "manuallyGraded", UiIcon.manuallyGraded, [] )
        ]
      )
    ]
