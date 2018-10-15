module Examples.Icon exposing (example)

{-|

@docs example, styles

-}

import Assets exposing (Assets, assets)
import Css
import Css.Foreign
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css, style, title)
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Icon.V3 as Icon
import Nri.Ui.Text.V2 as Text


{-| -}
example : ModuleExample msg
example =
    { filename = "ui/src/Nri/Icon.elm"
    , category = Icons
    , content =
        [ viewIconSection "Mastery Icons"
            [ { icon = Icon.gardening, background = Dark, alt = "Gardening" }
            , { icon = Icon.highFive, background = Dark, alt = "HighFive" }
            , { icon = Icon.okay, background = Dark, alt = "Okay" }
            , { icon = Icon.thumbsUp, background = Dark, alt = "ThumbsUp" }
            , { icon = Icon.masteryBadge, background = Light, alt = "Badge " }
            ]
        , viewIconSection "Stars and Flags"
            [ { icon = Icon.starred, background = Light, alt = "Starred" }
            , { icon = Icon.notStarred, background = Light, alt = "NotStarred" }
            , { icon = Icon.flag, background = Light, alt = "Flag" }
            ]
        , viewIconSection "Assignment Types"
            [ { icon = Icon.assignmentTypeDiagnostic, background = Light, alt = "Diagnostic" }
            , { icon = Icon.assignmentTypePractice, background = Light, alt = "Practice" }
            , { icon = Icon.assignmentTypeQuiz, background = Light, alt = "Quiz" }
            , { icon = Icon.assignmentTypeQuickWrite, background = Dark, alt = "QuickWrite" }
            , { icon = Icon.assignmentTypeWritingCycle, background = Light, alt = "WritingCycle" }
            , { icon = Icon.assignmentTypePeerReview, background = Dark, alt = "PeerReview" }
            , { icon = Icon.peerReview, background = Light, alt = "PeerReview" }
            , { icon = Icon.submitting, background = Light, alt = "Submitting" }
            , { icon = Icon.rating, background = Light, alt = "Rating" }
            , { icon = Icon.revising, background = Light, alt = "Revising" }
            , { icon = Icon.quickWrite, background = Light, alt = "QuickWrite" }

            --, { icon = Icon.guidedWrite, background = Light, alt = "GuidedWrite" }
            , { icon = Icon.writingAssignment, background = Light, alt = "WritingAssignment" }
            ]
        , viewIconSection "Student Assignment Actions"
            [ { icon = Icon.assignmentStartButtonPrimary, background = Light, alt = "Start primary" }
            , { icon = Icon.assignmentStartButtonSecondary, background = Light, alt = "Start secondary" }
            ]
        , viewIconSection "Teacher Assignment Actions"
            [ { icon = Icon.unarchive, background = Light, alt = "Unarchive" }
            , { icon = Icon.share, background = Light, alt = "Share" }
            , { icon = Icon.seeMore, background = Light, alt = "See More" }
            , { icon = Icon.preview, background = Light, alt = "Preview" }
            , { icon = Icon.performance, background = Light, alt = "Performance" }
            , { icon = Icon.openClose, background = Light, alt = "OpenClose" }
            ]
        , viewIconSection "Edit"
            [ { icon = Icon.edit, background = Light, alt = "Edit" }
            , { icon = Icon.editWriting, background = Light, alt = "EditWriting" }
            ]
        , viewIconSection "Humans"
            [ { icon = Icon.class, background = Light, alt = "Class" }
            , { icon = Icon.leaderboard, background = Light, alt = "Leaderboard" }
            , { icon = Icon.personBlue, background = Light, alt = "PersonBlue" }
            ]
        , viewIconSection "Social Media"
            [ { icon = Icon.facebook, background = Light, alt = "Facebook" }
            , { icon = Icon.twitter, background = Light, alt = "Twitter" }
            , { icon = Icon.clever, background = Light, alt = "Clever" }
            ]
        , viewIconSection "Arrows and Carets"
            [ { icon = Icon.arrowDown, background = Light, alt = "ArrowDown" }
            , { icon = Icon.sortArrow, background = Light, alt = "SortArrow" }
            , { icon = Icon.arrowLeft, background = Light, alt = "ArrowLeft" }
            , { icon = Icon.arrowRight, background = Light, alt = "ArrowRight" }
            , { icon = Icon.caret, background = Light, alt = "Caret" }
            ]
        , viewIconSection "Checkmarks"
            [ { icon = Icon.checkMarkSvg, background = Light, alt = "CheckMarkSvg" }
            , { icon = Icon.darkBlueCheckMark, background = Light, alt = "DarkBlueCheckMark" }
            , { icon = Icon.checkMark, background = Dark, alt = "CheckMark" }
            , { icon = Icon.checkMarkSquiggily, background = Dark, alt = "CheckMarkSquiggily" }
            , { icon = Icon.greenCheckMark, background = Light, alt = "GreenCheckMark" }
            ]
        , viewIconSection "Xs"
            [ { icon = Icon.xSvg, background = Light, alt = "XSvg" }
            , { icon = Icon.x, background = Dark, alt = "X" }
            , { icon = Icon.close, background = Light, alt = "Close" }
            ]
        , viewIconSection "Bangs"
            [ { icon = Icon.exclamation, background = Light, alt = "Exclamation" }
            , { icon = Icon.attention, background = Dark, alt = "Attention" }
            , { icon = Icon.bang, background = Dark, alt = "Bang" }
            ]
        , viewIconSection "Bulbs and Tips"
            [ { icon = Icon.bulb, background = Light, alt = "Bulb" }
            , { icon = Icon.lightBulb, background = Light, alt = "LightBulb" }
            , { icon = Icon.help, background = Light, alt = "Help" }
            ]
        , viewIconSection "Locks and keys"
            [ { icon = Icon.key, background = Light, alt = "Key" }
            , { icon = Icon.lock, background = Light, alt = "Lock" }
            , { icon = Icon.lockDeprecated, background = Light, alt = "LockDeprecated" }
            ]
        , viewIconSection "Time"
            [ { icon = Icon.calendar, background = Light, alt = "Calendar" }
            , { icon = Icon.clock, background = Light, alt = "Clock" }
            , { icon = Icon.late, background = Light, alt = "Late" }
            ]
        , viewIconSection "Save helpers"
            [ { icon = Icon.copy, background = Dark, alt = "Copy" }
            , { icon = Icon.download, background = Light, alt = "Download" }
            ]
        , viewIconSection "Uncategorized (SVGs)"
            [ { icon = Icon.activity, background = Light, alt = "Activity" }
            , { icon = Icon.compassSvg, background = Light, alt = "CompassSvg" }
            , { icon = Icon.document, background = Light, alt = "Document" }
            , { icon = Icon.flipper, background = Light, alt = "Flipper" }
            , { icon = Icon.footsteps, background = Light, alt = "Footsteps" }
            , { icon = Icon.gear, background = Light, alt = "Gear" }
            , { icon = Icon.pen, background = Light, alt = "Pen" }
            , { icon = Icon.newspaper, background = Light, alt = "Newspaper" }
            , { icon = Icon.sort, background = Light, alt = "Sort" }
            , { icon = Icon.speedometer, background = Light, alt = "Speedometer" }
            ]
        , viewIconSection "Uncategorized Images"
            [ { icon = Icon.add, background = Light, alt = "Add" }
            , { icon = Icon.equalitySign, background = Light, alt = "EqualitySign" }
            , { icon = Icon.logo, background = Light, alt = "Logo" }
            ]
        ]
            |> List.map Html.toUnstyled
    }


viewIconSection :
    String
    -> List { alt : String, background : Background, icon : Assets -> Icon.IconType }
    -> Html msg
viewIconSection headerText icons =
    Html.section []
        [ Text.subHeading [ Html.text headerText ]
        , Html.div [ css [ Css.displayFlex, Css.flexWrap Css.wrap ] ]
            (List.map viewIcon icons)
        ]


type Background
    = Light
    | Dark


toColor : Background -> Css.Color
toColor background =
    case background of
        Dark ->
            blue

        Light ->
            frost


viewIcon : { alt : String, background : Background, icon : Assets -> Icon.IconType } -> Html msg
viewIcon { alt, background, icon } =
    Html.div
        [ css
            [ Css.margin (Css.px 10)
            , Css.width (Css.px 160)
            , Css.boxShadow4 (Css.px 10) (Css.px 5) (Css.px 5) navy
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignItems Css.center
            , Css.justifyContent Css.flexStart
            ]
        ]
        [ Html.div
            [ css
                [ Css.displayFlex
                , Css.alignItems Css.center
                , Css.justifyContent Css.center
                , Css.backgroundColor (toColor background)
                , Css.height (Css.px 80)
                , Css.width (Css.px 80)
                , Css.margin (Css.px 10)
                , Css.color azure
                , Css.Foreign.descendants
                    [ Css.Foreign.img
                        [ Css.maxWidth (Css.pct 100)
                        , Css.maxHeight (Css.pct 100)
                        ]
                    ]
                ]
            , title alt
            ]
            [ Icon.icon { alt = alt, icon = icon assets }
            ]
        , Text.mediumBody [ Html.text alt ]
        ]
