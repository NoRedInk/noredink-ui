module Examples.Icon exposing (example, styles)

{-|

@docs example, styles

-}

import Css
import Css.Foreign exposing (Snippet, adjacentSiblings, children, class, descendants, each, everything, media, selector, withClass)
import Html exposing (Html)
import Html.Attributes exposing (style, title)
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Icon.V1 as Icon
import Nri.Ui.Styles.V1


{-| -}
example : Icon.Assets r -> ModuleExample msg
example assets =
    { filename = "Nri/Ui/Icon/V1.elm"
    , category = Icons
    , content =
        [ Html.h3 [] [ Html.text "Icon" ]
        , Html.h4 [] [ Html.text "Images" ]
        , Html.div [ styles.class [ Container ] ]
            (List.map (viewIcon assets) imageIcons)
        , Html.h4 [] [ Html.text "Scalable Vector Graphics (SVGs)" ]
        , Html.div [ styles.class [ Container ] ]
            (List.map (viewIcon assets) svgIcons)
        , Html.h5 [] [ Html.text "Assignment Types" ]
        , Html.div [ styles.class [ Container ] ]
            (List.map (viewIcon assets) assignmentTypeSvgIcons)
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


viewIcon : Icon.Assets r -> { alt : String, background : Background, icon : Icon.IconType } -> Html msg
viewIcon assets { alt, background, icon } =
    Html.div [ styles.class [ Cell background ], title alt ] [ Icon.icon assets { alt = alt, icon = icon } ]


imageIcons : List { alt : String, background : Background, icon : Icon.IconType }
imageIcons =
    [ { icon = Icon.Add, background = Light, alt = "Add" }
    , { icon = Icon.ArrowLeft, background = Light, alt = "ArrowLeft" }
    , { icon = Icon.ArrowRight, background = Light, alt = "ArrowRight" }
    , { icon = Icon.Attention, background = Dark, alt = "Attention" }
    , { icon = Icon.Bang, background = Dark, alt = "Bang" }
    , { icon = Icon.Caret, background = Light, alt = "Caret" }
    , { icon = Icon.CheckMark, background = Dark, alt = "CheckMark" }
    , { icon = Icon.CheckMarkSquiggily, background = Dark, alt = "CheckMarkSquiggily" }
    , { icon = Icon.Close, background = Light, alt = "Close" }
    , { icon = Icon.Copy, background = Dark, alt = "Copy" }
    , { icon = Icon.DarkBlueCheckMark, background = Light, alt = "DarkBlueCheckMark" }
    , { icon = Icon.EqualitySign, background = Light, alt = "EqualitySign" }
    , { icon = Icon.Facebook, background = Light, alt = "Facebook" }
    , { icon = Icon.Flag, background = Light, alt = "Flag" }
    , { icon = Icon.Gardening, background = Dark, alt = "Gardening" }
    , { icon = Icon.GreenCheckMark, background = Light, alt = "GreenCheckMark" }
    , { icon = Icon.Help, background = Light, alt = "Help" }
    , { icon = Icon.HighFive, background = Dark, alt = "HighFive" }
    , { icon = Icon.Late, background = Light, alt = "Late" }
    , { icon = Icon.LightBulb, background = Light, alt = "LightBulb" }
    , { icon = Icon.LockDeprecated, background = Light, alt = "LockDeprecated" }
    , { icon = Icon.Logo, background = Light, alt = "Logo" }
    , { icon = Icon.NotStarred, background = Light, alt = "NotStarred" }
    , { icon = Icon.Okay, background = Dark, alt = "Okay" }
    , { icon = Icon.PeerReview, background = Light, alt = "PeerReview" }
    , { icon = Icon.PersonBlue, background = Light, alt = "PersonBlue" }
    , { icon = Icon.QuickWrite, background = Light, alt = "QuickWrite" }
    , { icon = Icon.Starred, background = Light, alt = "Starred" }
    , { icon = Icon.ThumbsUp, background = Dark, alt = "ThumbsUp" }
    , { icon = Icon.Twitter, background = Light, alt = "Twitter" }
    , { icon = Icon.Unarchive, background = Light, alt = "Unarchive" }
    , { icon = Icon.X, background = Dark, alt = "X" }
    ]


svgIcons : List { alt : String, background : Background, icon : Icon.IconType }
svgIcons =
    [ { icon = Icon.Activity, background = Light, alt = "Activity" }
    , { icon = Icon.ArrowDown, background = Light, alt = "ArrowDown" }
    , { icon = Icon.Bulb, background = Light, alt = "Bulb" }
    , { icon = Icon.Calendar, background = Light, alt = "Calendar" }
    , { icon = Icon.CheckMarkSvg, background = Light, alt = "CheckMarkSvg" }
    , { icon = Icon.Class, background = Light, alt = "Class" }
    , { icon = Icon.Clever, background = Light, alt = "Clever" }
    , { icon = Icon.Clock, background = Light, alt = "Clock" }
    , { icon = Icon.CompassSvg, background = Light, alt = "CompassSvg" }
    , { icon = Icon.Document, background = Light, alt = "Document" }
    , { icon = Icon.Download, background = Light, alt = "Download" }
    , { icon = Icon.Edit, background = Light, alt = "Edit" }
    , { icon = Icon.EditWriting, background = Light, alt = "EditWriting" }
    , { icon = Icon.Exclamation, background = Light, alt = "Exclamation" }
    , { icon = Icon.Flipper, background = Light, alt = "Flipper" }
    , { icon = Icon.Footsteps, background = Light, alt = "Footsteps" }
    , { icon = Icon.Gear, background = Light, alt = "Gear" }
    , { icon = Icon.Key, background = Light, alt = "Key" }
    , { icon = Icon.Leaderboard, background = Light, alt = "Leaderboard" }
    , { icon = Icon.Lock, background = Light, alt = "Lock" }
    , { icon = Icon.Newspaper, background = Light, alt = "Newspaper" }
    , { icon = Icon.OpenClose, background = Light, alt = "OpenClose" }
    , { icon = Icon.Performance, background = Light, alt = "Performance" }
    , { icon = Icon.Preview, background = Light, alt = "Preview" }
    , { icon = Icon.SeeMore, background = Light, alt = "See More" }
    , { icon = Icon.Share, background = Light, alt = "Share" }
    , { icon = Icon.Sort, background = Light, alt = "Sort" }
    , { icon = Icon.SortArrow, background = Light, alt = "SortArrow" }
    , { icon = Icon.Speedometer, background = Light, alt = "Speedometer" }
    , { icon = Icon.WritingAssignment, background = Light, alt = "WritingAssignment" }
    , { icon = Icon.XSvg, background = Light, alt = "XSvg" }
    ]


assignmentTypeSvgIcons : List { alt : String, background : Background, icon : Icon.IconType }
assignmentTypeSvgIcons =
    [ { alt = "Diagnostic", background = Light, icon = Icon.AssignmentType Icon.Diagnostic }
    , { alt = "Practice", background = Light, icon = Icon.AssignmentType Icon.Practice }
    , { alt = "Quiz", background = Light, icon = Icon.AssignmentType Icon.Quiz }
    , { alt = "WritingCycle", background = Light, icon = Icon.AssignmentType Icon.WritingCycle }
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
