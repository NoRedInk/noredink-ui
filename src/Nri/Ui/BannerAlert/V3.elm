module Nri.Ui.BannerAlert.V3 exposing
    ( error
    , neutral
    , success
    )

{-|

@docs Assets
@docs error
@docs neutral
@docs success

-}

import Accessibility.Styled as Html exposing (Html)
import Css
import Css.Global
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.AssetPath as AssetPath exposing (Asset(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1
import Nri.Ui.Icon.V4 as Icon exposing (IconType)


{-| The assets required to use this module.
-}
type alias Assets assets =
    { assets
        | exclamationPoint_svg : Asset
        , tip_svg : Asset
        , checkWhite_svg : Asset
    }


{-| A banner to show error alerts
-}
error : Assets a -> String -> Html msg
error assets =
    banner
        { backgroundColor = Colors.purpleLight
        , color = Colors.purpleDark
        }


{-| A banner to show neutral alerts
-}
neutral : Assets a -> String -> Html msg
neutral assets =
    banner
        { backgroundColor = Colors.frost
        , color = Colors.navy
        }


{-| A banner for success alerts
-}
success : Assets a -> String -> Html msg
success assets =
    banner
        { backgroundColor = Colors.greenLightest
        , color = Colors.greenDarkest
        }


type alias Config =
    { color : Css.Color
    , backgroundColor : Css.Color
    }


banner : Config -> String -> Html msg
banner { color, backgroundColor } alertMessage =
    Html.div
        [ css
            [ Css.alignItems Css.center
            , Css.displayFlex
            , Css.justifyContent Css.center
            , Css.padding (Css.px 20)
            , Css.width (Css.pct 100)
            , Css.Global.children
                [ Css.Global.button
                    [ Css.position Css.absolute
                    , Css.right (Css.px 15)
                    ]
                ]
            , Css.backgroundColor backgroundColor
            , Css.color color
            ]
        ]
        [ notification alertMessage ]


type BannerType
    = Error -- exclamationPoint_svg
    | Neutral -- tip_svg
    | Success --  checkWhite_svg


icon : Assets assets -> BannerType -> Html msg
icon assets bannerType =
    let
        ( containerStyle, iconStyle, asset ) =
            case bannerType of
                Error ->
                    ( errorIcon
                    , errorIconImage
                    , assets.exclamationPoint_svg
                    )

                Neutral ->
                    ( neutralIcon
                    , neutralIconImage
                    , assets.tip_svg
                    )

                Success ->
                    ( successIcon
                    , successIconImage
                    , assets.checkWhite_svg
                    )
    in
    Html.div [ css containerStyle ]
        [ Html.decorativeImg
            [ css iconStyle
            , Attributes.src (AssetPath.url asset)
            ]
        ]


notification : String -> Html msg
notification message =
    Html.div
        [ css
            [ Css.fontSize (Css.px 20)
            , Css.fontWeight (Css.int 700)
            , Css.lineHeight (Css.px 25)
            , Css.maxWidth (Css.px 600)
            , Nri.Ui.Fonts.V1.baseFont
            ]
        ]
        [ Html.text message ]



-- ICON STYLES


errorIcon : List Css.Style
errorIcon =
    [ iconContainer
    , Css.backgroundColor Colors.purple
    ]


errorIconImage : List Css.Style
errorIconImage =
    [ Css.height (Css.px 25)
    ]


neutralIcon : List Css.Style
neutralIcon =
    [ iconContainer
    ]


neutralIconImage : List Css.Style
neutralIconImage =
    [ Css.height (Css.px 50)
    ]


successIcon : List Css.Style
successIcon =
    [ iconContainer
    , Css.backgroundColor Colors.green
    ]


successIconImage : List Css.Style
successIconImage =
    [ Css.height (Css.px 20)
    ]


iconContainer : Css.Style
iconContainer =
    Css.batch
        [ Css.borderRadius (Css.pct 50)
        , Css.displayFlex
        , Css.alignItems Css.center
        , Css.justifyContent Css.center
        , Css.width (Css.px 50)
        , Css.height (Css.px 50)
        , Css.marginRight (Css.px 20)
        , Css.flexShrink (Css.num 0)
        ]
