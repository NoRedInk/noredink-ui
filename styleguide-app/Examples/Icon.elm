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
