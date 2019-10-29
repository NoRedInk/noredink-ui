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
    , assignmentStartButtonPrimary_svg : Asset
    , assignmentStartButtonSecondary_svg : Asset
    , bulb : String
    , clever : String
    , compass : String
    , facebookBlue_svg : Asset
    , flipper : String
    , footsteps : String
    , help : String
    , hint_png : Asset
    , icons_equals_svg : Asset
    , icons_xBlue_svg : Asset
    , key : String
    , level1Badge_png : Asset
    , level2Badge_png : Asset
    , level3Badge_png : Asset
    , lock : String
    , logoRedBlack_svg : Asset
    , masteryBadge : String
    , skip : String
    , speedometer : String
    , startingOffBadge_png : Asset
    , tip_svg : Asset
    , twitterBlue_svg : Asset
    }


assets : Assets
assets =
    { activity = "icon-activity"
    , assignmentStartButtonPrimary_svg = Asset "assets/images/assignment-start-button-primary.svg"
    , assignmentStartButtonSecondary_svg = Asset "assets/images/assignment-start-button-secondary.svg"
    , bulb = "icon-bulb"
    , clever = "icon-clever"
    , compass = "icon-compass"
    , facebookBlue_svg = Asset "assets/images/facebook-blue.svg"
    , flipper = "icon-flipper"
    , footsteps = "icon-footsteps"
    , help = "icon-help"
    , hint_png = Asset "assets/images/hint.png"
    , icons_equals_svg = Asset "assets/images/equals.svg"
    , icons_xBlue_svg = Asset "assets/images/x-blue.svg"
    , key = "icon-key"
    , level1Badge_png = Asset "assets/images/level-1-badge.png"
    , level2Badge_png = Asset "assets/images/level-2-badge.png"
    , level3Badge_png = Asset "assets/images/level-3-badge.png"
    , lock = "icon-lock"
    , logoRedBlack_svg = Asset "assets/images/logo-red-black.svg"
    , masteryBadge = "icon-mastery-badge"
    , skip = "icon-skip"
    , speedometer = "icon-speedometer"
    , startingOffBadge_png = Asset "assets/images/starting-off-badge.png"
    , tip_svg = Asset "assets/images/tip.svg"
    , twitterBlue_svg = Asset "assets/images/twitter-blue.svg"
    }
