module Examples.Icon exposing (example)

{-|

@docs example, styles

-}

import Css
import Css.Global
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.AssetPath as AssetPath exposing (Asset(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Icon.V5 as Icon
import Nri.Ui.Text.V4 as Text


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.Icon.V5"
    , category = Icons
    , content =
        [ viewLarge "Mastery Icons"
            [ deprecatedIcon { icon = Icon.gardening { startingOffBadge_png = Asset "assets/images/starting-off-badge.png" }, background = Colors.blue, alt = "Gardening" }
            , deprecatedIcon { icon = Icon.highFive { level3Badge_png = Asset "assets/images/level-3-badge.png" }, background = Colors.blue, alt = "HighFive" }
            , deprecatedIcon { icon = Icon.okay { level2Badge_png = Asset "assets/images/level-2-badge.png" }, background = Colors.blue, alt = "Okay" }
            , deprecatedIcon { icon = Icon.thumbsUp { level1Badge_png = Asset "assets/images/level-1-badge.png" }, background = Colors.blue, alt = "ThumbsUp" }
            , deprecatedIcon { icon = Icon.masteryBadge { masteryBadge = "icon-mastery-badge" }, background = Colors.frost, alt = "Badge " }
            ]
        , viewLarge "Social Media"
            [ deprecatedIcon { icon = Icon.facebook { facebookBlue_svg = Asset "assets/images/facebook-blue.svg" }, background = Colors.frost, alt = "Facebook" }
            , deprecatedIcon { icon = Icon.twitter { twitterBlue_svg = Asset "assets/images/twitter-blue.svg" }, background = Colors.frost, alt = "Twitter" }
            , deprecatedIcon { icon = Icon.clever { clever = "icon-clever" }, background = Colors.frost, alt = "Clever" }
            ]
        , viewLarge "Bulbs and Tips"
            [ deprecatedIcon { icon = Icon.lightBulb { hint_png = Asset "assets/images/hint.png" }, background = Colors.frost, alt = "LightBulb" }
            ]
        , viewLarge "Locks and keys"
            [ deprecatedIcon { icon = Icon.key { key = "icon-key" }, background = Colors.frost, alt = "Key" }
            , deprecatedIcon { icon = Icon.lock { lock = "icon-lock" }, background = Colors.frost, alt = "Lock" }
            ]
        , viewLarge "Uncategorized (SVGs)"
            [ deprecatedIcon { icon = Icon.logo { logoRedBlack_svg = Asset "assets/images/logo-red-black.svg" }, background = Colors.frost, alt = "Logo" }
            ]
        ]
    }


viewLarge :
    String
    -> List ( String, Html msg )
    -> Html msg
viewLarge headerText icons =
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
