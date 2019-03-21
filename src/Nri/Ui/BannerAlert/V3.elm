module Nri.Ui.BannerAlert.V3 exposing
    ( error
    , neutral
    , success
    )

{-|

@docs error
@docs neutral
@docs success

-}

import Accessibility.Styled as Html exposing (Html)
import Css
import Css.Global
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1


{-| A banner to show error alerts
-}
error : String -> Html msg
error =
    banner
        { backgroundColor = Colors.purpleLight
        , color = Colors.purpleDark
        }


{-| A banner to show neutral alerts
-}
neutral : String -> Html msg
neutral =
    banner
        { backgroundColor = Colors.frost
        , color = Colors.navy
        }


{-| A banner for success alerts
-}
success : String -> Html msg
success =
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
