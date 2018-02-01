module Nri.Ui.BannerAlert.V1
    exposing
        ( error
        , neutral
        , styles
        , success
        )

{-|

@docs error
@docs neutral
@docs styles
@docs success

-}

import Accessibility
import Css
import Css.Elements
import Html exposing (Html)
import Nri.Colors
import Nri.Fonts
import Nri.Ui.Styles.V1


{-| A banner to show error alerts
-}
error : String -> Html msg
error =
    banner Error


{-| A banner to show neutral alerts
-}
neutral : String -> Html msg
neutral =
    banner Neutral


{-| A banner for success alerts
-}
success : String -> Html msg
success =
    banner Success


banner : CssClasses -> String -> Html msg
banner bannerType alertMessage =
    Accessibility.div
        [ styles.class [ Banner, bannerType ]
        ]
        [ notification alertMessage
        ]


notification : String -> Html msg
notification message =
    styles.div AlertMessage [ Accessibility.text message ]


type CssClasses
    = AlertMessage
    | Banner
    | Error
    | Neutral
    | Success


{-| -}
styles : Nri.Ui.Styles.V1.Styles Never CssClasses b
styles =
    Nri.Ui.Styles.V1.styles "Nri-Ui-BannerAlert-"
        [ Css.class AlertMessage
            [ Css.fontSize (Css.px 20)
            , Css.fontWeight (Css.int 700)
            , Css.lineHeight (Css.px 25)
            , Css.maxWidth (Css.px 600)
            , Nri.Fonts.baseFont
            ]
        , Css.class Banner
            [ Css.alignItems Css.center
            , Css.displayFlex
            , Css.justifyContent Css.center
            , Css.padding (Css.px 20)
            , Css.width (Css.pct 100)
            , Css.children
                [ Css.Elements.button
                    [ Css.position Css.absolute
                    , Css.right (Css.px 15)
                    ]
                ]
            ]
        , Css.class Error
            [ Css.backgroundColor Nri.Colors.purpleLight
            , Css.color Nri.Colors.purpleDark
            ]
        , Css.class Neutral
            [ Css.backgroundColor Nri.Colors.frost
            , Css.color Nri.Colors.navy
            ]
        , Css.class Success
            [ Css.backgroundColor Nri.Colors.greenLightest
            , Css.color Nri.Colors.greenDarkest
            ]
        ]
