module Nri.Ui.BannerAlert.V3 exposing (error, neutral, success)

{-|

@docs error, neutral, success

-}

import Accessibility.Styled as Html exposing (Html)
import Css
import Css.Global
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1
import Nri.Ui.SpriteSheet exposing (bulb, checkmark, exclamationMark)
import Nri.Ui.Svg.V1 as NriSvg exposing (Svg)


{-| A banner to show error alerts
-}
error : String -> Html msg
error =
    banner
        { backgroundColor = Colors.purpleLight
        , color = Colors.purpleDark
        , icon =
            { backgroundColor = Colors.purple
            , height = Css.px 25
            , asset = exclamationMark
            }
        }


{-| A banner to show neutral alerts
-}
neutral : String -> Html msg
neutral =
    banner
        { backgroundColor = Colors.frost
        , color = Colors.navy
        , icon =
            { backgroundColor = Colors.navy
            , height = Css.px 50
            , asset = bulb
            }
        }


{-| A banner for success alerts
-}
success : String -> Html msg
success =
    banner
        { backgroundColor = Colors.greenLightest
        , color = Colors.greenDarkest
        , icon =
            { backgroundColor = Colors.green
            , height = Css.px 20
            , asset = checkmark
            }
        }


type alias Config =
    { color : Css.Color
    , backgroundColor : Css.Color
    , icon : IconConfig
    }


banner : Config -> String -> Html msg
banner config alertMessage =
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
            , Css.backgroundColor config.backgroundColor
            , Css.color config.color
            ]
        ]
        [ icon config.icon
        , notification alertMessage
        ]


type alias IconConfig =
    { backgroundColor : Css.Color
    , height : Css.Px
    , asset : Svg
    }


icon : IconConfig -> Html msg
icon config =
    Html.div
        [ css
            [ Css.boxSizing Css.borderBox
            , Css.borderRadius (Css.pct 50)
            , Css.color Colors.white
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.width (Css.px 50)
            , Css.height (Css.px 50)
            , Css.marginRight (Css.px 20)
            , Css.padding (Css.px 8)
            , Css.flexShrink (Css.num 0)
            , Css.backgroundColor config.backgroundColor
            ]
        ]
        [ Html.div
            [ css [ Css.height config.height ]
            ]
            [ NriSvg.toHtml config.asset ]
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
