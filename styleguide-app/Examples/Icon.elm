module Examples.Icon exposing (example)

{-|

@docs example, styles

-}

import Assets exposing (Assets, assets)
import Css
import Css.Global
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css, style, title)
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.AssignmentIcon.V1 as AssignmentIcon
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Icon.V5 as Icon
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V4 as Text


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.Icon.V4"
    , category = Icons
    , content =
        [ viewIconSection "Mastery Icons"
            [ deprecatedIcon { icon = Icon.gardening, background = Colors.blue, alt = "Gardening" }
            , deprecatedIcon { icon = Icon.highFive, background = Colors.blue, alt = "HighFive" }
            , deprecatedIcon { icon = Icon.okay, background = Colors.blue, alt = "Okay" }
            , deprecatedIcon { icon = Icon.thumbsUp, background = Colors.blue, alt = "ThumbsUp" }
            , deprecatedIcon { icon = Icon.masteryBadge, background = Colors.frost, alt = "Badge " }
            ]
        , viewIconSection "Stars and Flags"
            [ deprecatedIcon { icon = Icon.starred, background = Colors.frost, alt = "Starred" }
            , deprecatedIcon { icon = Icon.notStarred, background = Colors.frost, alt = "NotStarred" }
            , deprecatedIcon { icon = Icon.flag, background = Colors.frost, alt = "Flag" }
            ]
        , viewIconSection "Nri.Ui.AssignmentIcon.V1."
            [ ( "diagnostic", Svg.toHtml AssignmentIcon.diagnostic )
            , ( "practice", Svg.toHtml AssignmentIcon.practice )
            , ( "quiz", Svg.toHtml AssignmentIcon.quiz )
            , ( "quickWrite", Svg.toHtml AssignmentIcon.quickWrite )
            , ( "guidedDraft", Svg.toHtml AssignmentIcon.guidedDraft )
            , ( "peerReview", Svg.toHtml AssignmentIcon.peerReview )
            , ( "selfReview", Svg.toHtml AssignmentIcon.selfReview )
            ]
        , viewIconSection "Assignment Types"
            [ deprecatedIcon { icon = Icon.submitting, background = Colors.frost, alt = "Submitting" }
            , deprecatedIcon { icon = Icon.rating, background = Colors.frost, alt = "Rating" }
            , deprecatedIcon { icon = Icon.revising, background = Colors.frost, alt = "Revising" }
            , deprecatedIcon { icon = Icon.assignmentTypeWritingCycle, background = Colors.frost, alt = "WritingCycle" }
            , deprecatedIcon { icon = Icon.writingAssignment, background = Colors.frost, alt = "WritingAssignment" }
            ]
        , viewIconSection "Student Assignment Actions"
            [ deprecatedIcon { icon = Icon.assignmentStartButtonPrimary, background = Colors.frost, alt = "Start primary" }
            , deprecatedIcon { icon = Icon.assignmentStartButtonSecondary, background = Colors.frost, alt = "Start secondary" }
            ]
        , viewIconSection "Teacher Assignment Actions"
            [ deprecatedIcon { icon = Icon.unarchive, background = Colors.frost, alt = "Unarchive" }
            , deprecatedIcon { icon = Icon.share, background = Colors.frost, alt = "Share" }
            , deprecatedIcon { icon = Icon.seeMore, background = Colors.frost, alt = "See More" }
            , deprecatedIcon { icon = Icon.preview, background = Colors.frost, alt = "Preview" }
            , deprecatedIcon { icon = Icon.performance, background = Colors.frost, alt = "Performance" }
            , deprecatedIcon { icon = Icon.openClose, background = Colors.frost, alt = "OpenClose" }
            , deprecatedIcon { icon = Icon.download, background = Colors.frost, alt = "Download" }
            ]
        , viewIconSection "Edit"
            [ deprecatedIcon { icon = Icon.edit, background = Colors.frost, alt = "Edit" }
            , deprecatedIcon { icon = Icon.editWriting, background = Colors.frost, alt = "EditWriting" }
            ]
        , viewIconSection "Humans"
            [ deprecatedIcon { icon = Icon.class, background = Colors.frost, alt = "Class" }
            , deprecatedIcon { icon = Icon.leaderboard, background = Colors.frost, alt = "Leaderboard" }
            , deprecatedIcon { icon = Icon.personBlue, background = Colors.frost, alt = "PersonBlue" }
            ]
        , viewIconSection "Social Media"
            [ deprecatedIcon { icon = Icon.facebook, background = Colors.frost, alt = "Facebook" }
            , deprecatedIcon { icon = Icon.twitter, background = Colors.frost, alt = "Twitter" }
            , deprecatedIcon { icon = Icon.clever, background = Colors.frost, alt = "Clever" }
            ]
        , viewIconSection "Arrows and Carets"
            [ deprecatedIcon { icon = Icon.arrowDown, background = Colors.frost, alt = "ArrowDown" }
            , deprecatedIcon { icon = Icon.sortArrow, background = Colors.frost, alt = "SortArrow" }
            ]
        , viewIconSection "Checkmarks"
            [ deprecatedIcon { icon = Icon.checkMarkSvg, background = Colors.frost, alt = "CheckMarkSvg" }
            ]
        , viewIconSection "Xs"
            [ deprecatedIcon { icon = Icon.xSvg, background = Colors.frost, alt = "XSvg" }
            ]
        , viewIconSection "Bangs"
            [ deprecatedIcon { icon = Icon.exclamation, background = Colors.frost, alt = "Exclamation" }
            , deprecatedIcon { icon = Icon.attention, background = Colors.blue, alt = "Attention" }
            ]
        , viewIconSection "Bulbs and Tips"
            [ deprecatedIcon { icon = Icon.bulb, background = Colors.frost, alt = "Bulb" }
            , deprecatedIcon { icon = Icon.lightBulb, background = Colors.frost, alt = "LightBulb" }
            , deprecatedIcon { icon = Icon.helpSvg, background = Colors.frost, alt = "Help" }
            ]
        , viewIconSection "Locks and keys"
            [ deprecatedIcon { icon = Icon.key, background = Colors.frost, alt = "Key" }
            , deprecatedIcon { icon = Icon.lock, background = Colors.frost, alt = "Lock" }
            ]
        , viewIconSection "Time"
            [ deprecatedIcon { icon = Icon.calendar, background = Colors.frost, alt = "Calendar" }
            , deprecatedIcon { icon = Icon.clock, background = Colors.frost, alt = "Clock" }
            ]
        , viewIconSection "Uncategorized (SVGs)"
            [ deprecatedIcon { icon = Icon.activity, background = Colors.frost, alt = "Activity" }
            , deprecatedIcon { icon = Icon.compassSvg, background = Colors.frost, alt = "CompassSvg" }
            , deprecatedIcon { icon = Icon.document, background = Colors.frost, alt = "Document" }
            , deprecatedIcon { icon = Icon.flipper, background = Colors.frost, alt = "Flipper" }
            , deprecatedIcon { icon = Icon.footsteps, background = Colors.frost, alt = "Footsteps" }
            , deprecatedIcon { icon = Icon.gear, background = Colors.frost, alt = "Gear" }
            , deprecatedIcon { icon = Icon.pen, background = Colors.frost, alt = "Pen" }
            , deprecatedIcon { icon = Icon.newspaper, background = Colors.frost, alt = "Newspaper" }
            , deprecatedIcon { icon = Icon.sort, background = Colors.frost, alt = "Sort" }
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


viewIconSection :
    String
    -> List ( String, Html msg )
    -> Html msg
viewIconSection headerText icons =
    Html.section [ css [ Css.marginTop (Css.px 16) ] ]
        [ Heading.h2 [] [ Html.text headerText ]
        , Html.div [ css [ Css.displayFlex, Css.flexWrap Css.wrap ] ]
            (List.map viewIcon icons)
        ]


viewIcon : ( String, Html msg ) -> Html msg
viewIcon ( name, assignmentIcon ) =
    Html.div
        [ css
            [ Css.margin (Css.px 10)
            , Css.width (Css.px 160)
            , Css.boxShadow4 (Css.px 10) (Css.px 5) (Css.px 5) Colors.navy
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignItems Css.center
            , Css.justifyContent Css.flexStart
            ]
        ]
        [ Html.div
            [ css
                [ Css.height (Css.px 80)
                , Css.width (Css.px 80)
                , Css.margin (Css.px 10)
                , Css.color Colors.green
                ]
            ]
            [ assignmentIcon
            ]
        , Text.mediumBody [ Html.text name ]
        ]
