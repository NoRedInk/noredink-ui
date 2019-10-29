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
            [ deprecatedIcon { icon = Icon.gardening { startingOffBadge_png = Asset "assets/images/starting-off-badge.png" }, background = Colors.blue, alt = "Gardening" }
            , deprecatedIcon { icon = Icon.highFive { level3Badge_png = Asset "assets/images/level-3-badge.png" }, background = Colors.blue, alt = "HighFive" }
            , deprecatedIcon { icon = Icon.okay { level2Badge_png = Asset "assets/images/level-2-badge.png" }, background = Colors.blue, alt = "Okay" }
            , deprecatedIcon { icon = Icon.thumbsUp { level1Badge_png = Asset "assets/images/level-1-badge.png" }, background = Colors.blue, alt = "ThumbsUp" }
            , deprecatedIcon { icon = Icon.masteryBadge { masteryBadge = "icon-mastery-badge" }, background = Colors.frost, alt = "Badge " }
            ]
        , IconExamples.view "Student Assignment Actions"
            [ deprecatedIcon { icon = Icon.assignmentStartButtonPrimary { assignmentStartButtonPrimary_svg = Asset "assets/images/assignment-start-button-primary.svg" }, background = Colors.frost, alt = "Start primary" }
            , deprecatedIcon { icon = Icon.assignmentStartButtonSecondary { assignmentStartButtonSecondary_svg = Asset "assets/images/assignment-start-button-secondary.svg" }, background = Colors.frost, alt = "Start secondary" }
            ]
        , IconExamples.view "Social Media"
            [ deprecatedIcon { icon = Icon.facebook { facebookBlue_svg = Asset "assets/images/facebook-blue.svg" }, background = Colors.frost, alt = "Facebook" }
            , deprecatedIcon { icon = Icon.twitter { twitterBlue_svg = Asset "assets/images/twitter-blue.svg" }, background = Colors.frost, alt = "Twitter" }
            , deprecatedIcon { icon = Icon.clever { clever = "icon-clever" }, background = Colors.frost, alt = "Clever" }
            ]
        , IconExamples.view "Bulbs and Tips"
            [ deprecatedIcon { icon = Icon.bulb { bulb = "icon-bulb" }, background = Colors.frost, alt = "Bulb" }
            , deprecatedIcon { icon = Icon.lightBulb { hint_png = Asset "assets/images/hint.png" }, background = Colors.frost, alt = "LightBulb" }
            , deprecatedIcon { icon = Icon.helpSvg { help = "icon-help" }, background = Colors.frost, alt = "Help" }
            ]
        , IconExamples.view "Locks and keys"
            [ deprecatedIcon { icon = Icon.key { key = "icon-key" }, background = Colors.frost, alt = "Key" }
            , deprecatedIcon { icon = Icon.lock { lock = "icon-lock" }, background = Colors.frost, alt = "Lock" }
            ]
        , IconExamples.view "Uncategorized (SVGs)"
            [ deprecatedIcon { icon = Icon.activity { activity = "icon-activity" }, background = Colors.frost, alt = "Activity" }
            , deprecatedIcon { icon = Icon.compassSvg { compass = "icon-compass" }, background = Colors.frost, alt = "CompassSvg" }
            , deprecatedIcon { icon = Icon.footsteps { footsteps = "icon-footsteps" }, background = Colors.frost, alt = "Footsteps" }
            , deprecatedIcon { icon = Icon.speedometer { speedometer = "icon-speedometer" }, background = Colors.frost, alt = "Speedometer" }
            , deprecatedIcon { icon = Icon.skip { skip = "icon-skip" }, background = Colors.frost, alt = "Skip" }
            , deprecatedIcon { icon = Icon.equalitySign { icons_equals_svg = Asset "assets/images/equals.svg" }, background = Colors.frost, alt = "EqualitySign" }
            , deprecatedIcon { icon = Icon.logo { logoRedBlack_svg = Asset "assets/images/logo-red-black.svg" }, background = Colors.frost, alt = "Logo" }
            ]
        ]
    }


deprecatedIcon : { alt : String, background : Css.Color, icon : Icon.IconType } -> ( String, Html msg )
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
        [ Icon.icon { alt = alt, icon = icon }
        ]
    )
