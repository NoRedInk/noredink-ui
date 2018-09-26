module Nri.Ui.BannerAlert.V1 exposing
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
import Css.Foreign exposing (Snippet, children, descendants, everything, selector)
import DEPRECATED.Css.File exposing (Stylesheet, compile, stylesheet)
import Html exposing (Html)
import Nri.Ui.Colors.V1
import Nri.Ui.Fonts.V1
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
        [ Css.Foreign.class AlertMessage
            [ Css.fontSize (Css.px 20)
            , Css.fontWeight (Css.int 700)
            , Css.lineHeight (Css.px 25)
            , Css.maxWidth (Css.px 600)
            , Nri.Ui.Fonts.V1.baseFont
            ]
        , Css.Foreign.class Banner
            [ Css.alignItems Css.center
            , Css.displayFlex
            , Css.justifyContent Css.center
            , Css.padding (Css.px 20)
            , Css.width (Css.pct 100)
            , Css.Foreign.children
                [ Css.Foreign.button
                    [ Css.position Css.absolute
                    , Css.right (Css.px 15)
                    ]
                ]
            ]
        , Css.Foreign.class Error
            [ Css.backgroundColor Nri.Ui.Colors.V1.purpleLight
            , Css.color Nri.Ui.Colors.V1.purpleDark
            ]
        , Css.Foreign.class Neutral
            [ Css.backgroundColor Nri.Ui.Colors.V1.frost
            , Css.color Nri.Ui.Colors.V1.navy
            ]
        , Css.Foreign.class Success
            [ Css.backgroundColor Nri.Ui.Colors.V1.greenLightest
            , Css.color Nri.Ui.Colors.V1.greenDarkest
            ]
        ]
