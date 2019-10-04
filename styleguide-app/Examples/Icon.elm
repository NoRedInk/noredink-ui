module Examples.Icon exposing (example)

{-|

@docs example, styles

-}

import Assets exposing (Assets, assets)
import Css
import Css.Global
import Examples.IconExamples as IconExamples
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import ModuleExample exposing (Category(..), ModuleExample)
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
        , IconExamples.view "Stars and Flags"
            [ deprecatedIcon { icon = Icon.starred, background = Colors.frost, alt = "Starred" }
            , deprecatedIcon { icon = Icon.notStarred, background = Colors.frost, alt = "NotStarred" }
            , deprecatedIcon { icon = Icon.flag, background = Colors.frost, alt = "Flag" }
            ]
        , IconExamples.view "Student Assignment Actions"
            [ deprecatedIcon { icon = Icon.assignmentStartButtonPrimary, background = Colors.frost, alt = "Start primary" }
            , deprecatedIcon { icon = Icon.assignmentStartButtonSecondary, background = Colors.frost, alt = "Start secondary" }
            ]
        , IconExamples.view "Edit"
            [ deprecatedIcon { icon = Icon.edit, background = Colors.frost, alt = "Edit" }
            , deprecatedIcon { icon = Icon.editWriting, background = Colors.frost, alt = "EditWriting" }
            ]
        , IconExamples.view "Humans"
            [ deprecatedIcon { icon = Icon.class, background = Colors.frost, alt = "Class" }
            , deprecatedIcon { icon = Icon.leaderboard, background = Colors.frost, alt = "Leaderboard" }
            , deprecatedIcon { icon = Icon.personBlue, background = Colors.frost, alt = "PersonBlue" }
            ]
        , IconExamples.view "Social Media"
            [ deprecatedIcon { icon = Icon.facebook, background = Colors.frost, alt = "Facebook" }
            , deprecatedIcon { icon = Icon.twitter, background = Colors.frost, alt = "Twitter" }
            , deprecatedIcon { icon = Icon.clever, background = Colors.frost, alt = "Clever" }
            ]
        , IconExamples.view "Arrows and Carets"
            [ deprecatedIcon { icon = Icon.arrowDown, background = Colors.frost, alt = "ArrowDown" }
            , deprecatedIcon { icon = Icon.sortArrow, background = Colors.frost, alt = "SortArrow" }
            ]
        , IconExamples.view "Checkmarks"
            [ deprecatedIcon { icon = Icon.checkMarkSvg, background = Colors.frost, alt = "CheckMarkSvg" }
            ]
        , IconExamples.view "Xs"
            [ deprecatedIcon { icon = Icon.xSvg, background = Colors.frost, alt = "XSvg" }
            ]
        , IconExamples.view "Bangs"
            [ deprecatedIcon { icon = Icon.exclamation, background = Colors.frost, alt = "Exclamation" }
            , deprecatedIcon { icon = Icon.attention, background = Colors.blue, alt = "Attention" }
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
        , IconExamples.view "Time"
            [ deprecatedIcon { icon = Icon.calendar, background = Colors.frost, alt = "Calendar" }
            , deprecatedIcon { icon = Icon.clock, background = Colors.frost, alt = "Clock" }
            ]
        , IconExamples.view "Uncategorized (SVGs)"
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
