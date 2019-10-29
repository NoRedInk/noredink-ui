module Examples.Icon exposing (example)

{-|

@docs example, styles

-}

import Css
import Css.Global
import Examples.IconExamples as IconExamples
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.AssetPath as AssetPath exposing (Asset(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Icon.V5 as Icon


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.Icon.V5"
    , category = Icons
    , content =
        [ IconExamples.view "Mastery Icons"
            [ deprecatedIcon { icon = Icon.gardening, background = Colors.blue, alt = "Gardening" }
            , deprecatedIcon { icon = Icon.highFive, background = Colors.blue, alt = "HighFive" }
            , deprecatedIcon { icon = Icon.okay, background = Colors.blue, alt = "Okay" }
            , deprecatedIcon { icon = Icon.thumbsUp, background = Colors.blue, alt = "ThumbsUp" }
            , deprecatedIcon { icon = Icon.masteryBadge, background = Colors.frost, alt = "Badge " }
            ]
        , IconExamples.view "Student Assignment Actions"
            [ deprecatedIcon { icon = Icon.assignmentStartButtonPrimary, background = Colors.frost, alt = "Start primary" }
            , deprecatedIcon { icon = Icon.assignmentStartButtonSecondary, background = Colors.frost, alt = "Start secondary" }
            ]
        , IconExamples.view "Social Media"
            [ deprecatedIcon { icon = Icon.facebook, background = Colors.frost, alt = "Facebook" }
            , deprecatedIcon { icon = Icon.twitter, background = Colors.frost, alt = "Twitter" }
            , deprecatedIcon { icon = Icon.clever, background = Colors.frost, alt = "Clever" }
            ]
        , IconExamples.view "Bulbs and Tips"
            [ deprecatedIcon { icon = Icon.bulb, background = Colors.frost, alt = "Bulb" }
            , deprecatedIcon { icon = Icon.lightBulb, background = Colors.frost, alt = "LightBulb" }
            , deprecatedIcon { icon = Icon.helpSvg, background = Colors.frost, alt = "Help" }
            ]
        , IconExamples.view "Locks and keys"
            [ deprecatedIcon { icon = Icon.key, background = Colors.frost, alt = "Key" }
            , deprecatedIcon { icon = Icon.lock, background = Colors.frost, alt = "Lock" }
            ]
        , IconExamples.view "Uncategorized (SVGs)"
            [ deprecatedIcon { icon = Icon.activity, background = Colors.frost, alt = "Activity" }
            , deprecatedIcon { icon = Icon.compassSvg, background = Colors.frost, alt = "CompassSvg" }
            , deprecatedIcon { icon = Icon.footsteps, background = Colors.frost, alt = "Footsteps" }
            , deprecatedIcon { icon = Icon.speedometer, background = Colors.frost, alt = "Speedometer" }
            , deprecatedIcon { icon = Icon.skip, background = Colors.frost, alt = "Skip" }
            , deprecatedIcon { icon = Icon.equalitySign, background = Colors.frost, alt = "EqualitySign" }
            , deprecatedIcon { icon = Icon.logo, background = Colors.frost, alt = "Logo" }
            ]
        ]
    }


deprecatedIcon : { alt : String, background : Css.Color, icon : Assets -> Icon.IconType } -> ( String, Html msg )
deprecatedIcon { alt, background, icon } =
    ( alt
    , Html.div
        [ css
            [ Css.backgroundColor background
            , Css.height (Css.px 80)
            , Css.width (Css.px 80)
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.Global.descendants
                [ Css.Global.img
                    [ Css.maxWidth (Css.pct 100)
                    , Css.maxHeight (Css.pct 100)
                    ]
                ]
            ]
        ]
        [ Icon.icon { alt = alt, icon = icon assets }
        ]
    )


type alias Assets =
    { activity : String
    , arrowDown : String
    , assignmentStartButtonPrimary_svg : Asset
    , assignmentStartButtonSecondary_svg : Asset
    , attention_svg : Asset
    , bulb : String
    , calendar : String
    , checkmark : String
    , class : String
    , clever : String
    , clock : String
    , commentNotStarred_png : Asset
    , commentStarred_png : Asset
    , compass : String
    , diagnostic : String
    , document : String
    , download : String
    , edit : String
    , editWriting : String
    , exclamation : String
    , facebookBlue_svg : Asset
    , flipper : String
    , footsteps : String
    , gear : String
    , guidedDraft : String
    , help : String
    , hint_png : Asset
    , iconFlag_png : Asset
    , icons_equals_svg : Asset
    , icons_xBlue_svg : Asset
    , key : String
    , leaderboard : String
    , level1Badge_png : Asset
    , level2Badge_png : Asset
    , level3Badge_png : Asset
    , lock : String
    , logoRedBlack_svg : Asset
    , masteryBadge : String
    , newspaper : String
    , openClose : String
    , peerReview : String
    , pen : Asset
    , performance : String
    , personBlue_svg : Asset
    , practice : String
    , preview : String
    , quickWrite : String
    , quiz : String
    , rating : String
    , revising : String
    , seemore : String
    , selfReview : String
    , share : String
    , skip : String
    , sort : String
    , sortArrow : String
    , speedometer : String
    , startingOffBadge_png : Asset
    , submitting : String
    , tip_svg : Asset
    , twitterBlue_svg : Asset
    , unarchiveBlue2x_png : Asset
    , writingAssignment : String
    , writingcycle : String
    , x : String
    }


assets : Assets
assets =
    { activity = "icon-activity"
    , arrowDown = "icon-arrow-down"
    , assignmentStartButtonPrimary_svg = Asset "assets/images/assignment-start-button-primary.svg"
    , assignmentStartButtonSecondary_svg = Asset "assets/images/assignment-start-button-secondary.svg"
    , attention_svg = Asset "assets/images/attention.svg"
    , bulb = "icon-bulb"
    , calendar = "icon-calendar"
    , checkmark = "icon-checkmark"
    , class = "icon-class"
    , clever = "icon-clever"
    , clock = "icon-clock"
    , commentNotStarred_png = Asset "assets/images/comment-notStarred.png"
    , commentStarred_png = Asset "assets/images/comment-starred.png"
    , compass = "icon-compass"
    , diagnostic = "icon-diagnostic"
    , document = "icon-document"
    , download = "icon-download"
    , edit = "icon-edit"
    , editWriting = "icon-edit-writing"
    , exclamation = "icon-exclamation"
    , facebookBlue_svg = Asset "assets/images/facebook-blue.svg"
    , flipper = "icon-flipper"
    , footsteps = "icon-footsteps"
    , gear = "icon-gear"
    , guidedDraft = "icon-guided-draft"
    , help = "icon-help"
    , hint_png = Asset "assets/images/hint.png"
    , iconFlag_png = Asset "assets/images/icon-flag.png"
    , icons_equals_svg = Asset "assets/images/equals.svg"
    , icons_xBlue_svg = Asset "assets/images/x-blue.svg"
    , key = "icon-key"
    , leaderboard = "icon-leaderboard"
    , level1Badge_png = Asset "assets/images/level-1-badge.png"
    , level2Badge_png = Asset "assets/images/level-2-badge.png"
    , level3Badge_png = Asset "assets/images/level-3-badge.png"
    , lock = "icon-lock"
    , logoRedBlack_svg = Asset "assets/images/logo-red-black.svg"
    , masteryBadge = "icon-mastery-badge"
    , newspaper = "icon-newspaper"
    , openClose = "icon-open-close"
    , peerReview = "icon-peer-review"
    , pen = Asset "assets/images/pen.svg"
    , performance = "icon-performance"
    , personBlue_svg = Asset "assets/images/person-blue.svg"
    , practice = "icon-practice"
    , preview = "icon-preview"
    , quickWrite = "icon-quick-write"
    , quiz = "icon-quiz"
    , rating = "icon-rating"
    , revising = "icon-revising"
    , seemore = "icon-seemore"
    , selfReview = "icon-self-review"
    , share = "icon-share"
    , skip = "icon-skip"
    , sort = "icon-sort"
    , sortArrow = "icon-sort-arrow"
    , speedometer = "icon-speedometer"
    , startingOffBadge_png = Asset "assets/images/starting-off-badge.png"
    , submitting = "icon-submitting"
    , tip_svg = Asset "assets/images/tip.svg"
    , twitterBlue_svg = Asset "assets/images/twitter-blue.svg"
    , unarchiveBlue2x_png = Asset "assets/images/unarchive-blue_2x.png"
    , writingAssignment = "icon-writing-assignment"
    , writingcycle = "icon-writingcycle"
    , x = "icon-x"
    }
