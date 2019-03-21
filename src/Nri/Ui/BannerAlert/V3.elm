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
import Css exposing (..)
import Css.Global exposing (Snippet, children, descendants, everything, selector)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1
import Nri.Ui.Fonts.V1


{-| A banner to show error alerts
-}
error : String -> Html msg
error =
    banner
        [ Css.backgroundColor Nri.Ui.Colors.V1.purpleLight
        , Css.color Nri.Ui.Colors.V1.purpleDark
        ]


{-| A banner to show neutral alerts
-}
neutral : String -> Html msg
neutral =
    banner
        [ Css.backgroundColor Nri.Ui.Colors.V1.frost
        , Css.color Nri.Ui.Colors.V1.navy
        ]


{-| A banner for success alerts
-}
success : String -> Html msg
success =
    banner
        [ Css.backgroundColor Nri.Ui.Colors.V1.greenLightest
        , Css.color Nri.Ui.Colors.V1.greenDarkest
        ]


banner : List Css.Style -> String -> Html msg
banner bannerType alertMessage =
    Html.div
        [ css
            ([ Css.alignItems Css.center
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
             ]
                ++ bannerType
            )
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
