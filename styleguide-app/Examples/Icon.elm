module Examples.Icon exposing (example, styles)

{-|

@docs example, styles

-}

import Assets exposing (assets)
import Css
import Css.Foreign exposing (Snippet, adjacentSiblings, children, class, descendants, each, everything, media, selector, withClass)
import Html exposing (Html)
import Html.Attributes exposing (style, title)
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Icon.V2 as Icon
import Nri.Ui.Styles.V1


{-| -}
example : ModuleExample msg
example =
    { filename = "Nri/Ui/Icon/V1.elm"
    , category = Icons
    , content =
        [ Html.h3 [] [ Html.text "Icon" ]
        , Html.h4 [] [ Html.text "Images" ]
        , Html.div [ styles.class [ Container ] ]
            (List.map viewIcon imageIcons)
        , Html.h4 [] [ Html.text "Scalable Vector Graphics (SVGs)" ]
        , Html.div [ styles.class [ Container ] ]
            (List.map viewIcon svgIcons)
        , Html.h5 [] [ Html.text "Assignment Types" ]
        , Html.div [ styles.class [ Container ] ]
            (List.map viewIcon assignmentTypeSvgIcons)
        ]
    }


{-| -}
styles : Nri.Ui.Styles.V1.Styles id class msg
styles =
    Nri.Ui.Styles.V1.styles "Examples-Icon-"
        [ Css.Foreign.class Container
            [ Css.displayFlex
            , Css.flexWrap Css.wrap
            ]
        , Css.Foreign.class (Cell Dark)
            (cellStyles blue)
        , Css.Foreign.class (Cell Light)
            (cellStyles frost)
        ]



-- INTERNAL


type CssClasses
    = Container
    | Cell Background


type Background
    = Light
    | Dark


viewIcon : { alt : String, background : Background, icon : Icon.IconType } -> Html msg
viewIcon { alt, background, icon } =
    Html.div [ styles.class [ Cell background ], title alt ] [ Icon.icon { alt = alt, icon = icon } ]


imageIcons : List { alt : String, background : Background, icon : Icon.IconType }
imageIcons =
    [ { icon = Icon.add assets, background = Light, alt = "Add" }
    , { icon = Icon.arrowLeft assets, background = Light, alt = "ArrowLeft" }
    , { icon = Icon.arrowRight assets, background = Light, alt = "ArrowRight" }
    , { icon = Icon.attention assets, background = Dark, alt = "Attention" }
    , { icon = Icon.bang assets, background = Dark, alt = "Bang" }
    , { icon = Icon.caret assets, background = Light, alt = "Caret" }
    , { icon = Icon.checkMark assets, background = Dark, alt = "CheckMark" }
    , { icon = Icon.checkMarkSquiggily assets, background = Dark, alt = "CheckMarkSquiggily" }
    , { icon = Icon.close assets, background = Light, alt = "Close" }
    , { icon = Icon.copy assets, background = Dark, alt = "Copy" }
    , { icon = Icon.darkBlueCheckMark assets, background = Light, alt = "DarkBlueCheckMark" }
    , { icon = Icon.equalitySign assets, background = Light, alt = "EqualitySign" }
    , { icon = Icon.facebook assets, background = Light, alt = "Facebook" }
    , { icon = Icon.flag assets, background = Light, alt = "Flag" }
    , { icon = Icon.gardening assets, background = Dark, alt = "Gardening" }
    , { icon = Icon.greenCheckMark assets, background = Light, alt = "GreenCheckMark" }
    , { icon = Icon.help assets, background = Light, alt = "Help" }
    , { icon = Icon.highFive assets, background = Dark, alt = "HighFive" }
    , { icon = Icon.late assets, background = Light, alt = "Late" }
    , { icon = Icon.lightBulb assets, background = Light, alt = "LightBulb" }
    , { icon = Icon.lockDeprecated assets, background = Light, alt = "LockDeprecated" }
    , { icon = Icon.logo assets, background = Light, alt = "Logo" }
    , { icon = Icon.notStarred assets, background = Light, alt = "NotStarred" }
    , { icon = Icon.okay assets, background = Dark, alt = "Okay" }
    , { icon = Icon.peerReview assets, background = Light, alt = "PeerReview" }
    , { icon = Icon.personBlue assets, background = Light, alt = "PersonBlue" }
    , { icon = Icon.quickWrite assets, background = Light, alt = "QuickWrite" }
    , { icon = Icon.starred assets, background = Light, alt = "Starred" }
    , { icon = Icon.thumbsUp assets, background = Dark, alt = "ThumbsUp" }
    , { icon = Icon.twitter assets, background = Light, alt = "Twitter" }
    , { icon = Icon.unarchive assets, background = Light, alt = "Unarchive" }
    , { icon = Icon.x assets, background = Dark, alt = "X" }
    ]


svgIcons : List { alt : String, background : Background, icon : Icon.IconType }
svgIcons =
    [ { icon = Icon.activity assets, background = Light, alt = "Activity" }
    , { icon = Icon.arrowDown assets, background = Light, alt = "ArrowDown" }
    , { icon = Icon.bulb assets, background = Light, alt = "Bulb" }
    , { icon = Icon.calendar assets, background = Light, alt = "Calendar" }
    , { icon = Icon.checkMarkSvg assets, background = Light, alt = "CheckMarkSvg" }
    , { icon = Icon.class assets, background = Light, alt = "Class" }
    , { icon = Icon.clever assets, background = Light, alt = "Clever" }
    , { icon = Icon.clock assets, background = Light, alt = "Clock" }
    , { icon = Icon.compassSvg assets, background = Light, alt = "CompassSvg" }
    , { icon = Icon.document assets, background = Light, alt = "Document" }
    , { icon = Icon.download assets, background = Light, alt = "Download" }
    , { icon = Icon.edit assets, background = Light, alt = "Edit" }
    , { icon = Icon.editWriting assets, background = Light, alt = "EditWriting" }
    , { icon = Icon.exclamation assets, background = Light, alt = "Exclamation" }
    , { icon = Icon.flipper assets, background = Light, alt = "Flipper" }
    , { icon = Icon.footsteps assets, background = Light, alt = "Footsteps" }
    , { icon = Icon.gear assets, background = Light, alt = "Gear" }
    , { icon = Icon.key assets, background = Light, alt = "Key" }
    , { icon = Icon.leaderboard assets, background = Light, alt = "Leaderboard" }
    , { icon = Icon.lock assets, background = Light, alt = "Lock" }
    , { icon = Icon.newspaper assets, background = Light, alt = "Newspaper" }
    , { icon = Icon.openClose assets, background = Light, alt = "OpenClose" }
    , { icon = Icon.performance assets, background = Light, alt = "Performance" }
    , { icon = Icon.preview assets, background = Light, alt = "Preview" }
    , { icon = Icon.seeMore assets, background = Light, alt = "See More" }
    , { icon = Icon.share assets, background = Light, alt = "Share" }
    , { icon = Icon.sort assets, background = Light, alt = "Sort" }
    , { icon = Icon.sortArrow assets, background = Light, alt = "SortArrow" }
    , { icon = Icon.speedometer assets, background = Light, alt = "Speedometer" }
    , { icon = Icon.writingAssignment assets, background = Light, alt = "WritingAssignment" }
    , { icon = Icon.xSvg assets, background = Light, alt = "XSvg" }
    ]


assignmentTypeSvgIcons : List { alt : String, background : Background, icon : Icon.IconType }
assignmentTypeSvgIcons =
    [ { alt = "Diagnostic", background = Light, icon = Icon.assignmentTypeDiagnostic assets }
    , { alt = "Practice", background = Light, icon = Icon.assignmentTypePractice assets }
    , { alt = "Quiz", background = Light, icon = Icon.assignmentTypeQuiz assets }
    , { alt = "WritingCycle", background = Light, icon = Icon.assignmentTypeWritingCycle assets }
    ]


cellStyles : Css.Color -> List Css.Style
cellStyles backgroundColor =
    [ Css.width (Css.px 80)
    , Css.height (Css.px 80)
    , Css.margin (Css.px 10)
    , Css.displayFlex
    , Css.boxShadow4 (Css.px 10) (Css.px 5) (Css.px 5) navy
    , Css.color azure
    , Css.alignItems Css.center
    , Css.justifyContent Css.center
    , Css.backgroundColor backgroundColor
    , Css.Foreign.descendants
        [ Css.Foreign.img
            [ Css.maxWidth (Css.pct 100)
            , Css.maxHeight (Css.pct 100)
            ]
        ]
    ]
