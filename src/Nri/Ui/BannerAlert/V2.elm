module Nri.Ui.BannerAlert.V2 exposing
    ( error
    , neutral
    , success
    )

{-|

@docs error
@docs neutral
@docs success

-}

import Accessibility.Styled as Accessibility
import Css exposing (..)
import Css.Foreign exposing (Snippet, children, descendants, everything, selector)
import Html.Styled as Html exposing (Html)
import Nri.Ui.Colors.V1
import Nri.Ui.Fonts.V1


{-| A banner to show error alerts
-}
error : String -> Html msg
error =
    banner errorStyles


{-| A banner to show neutral alerts
-}
neutral : String -> Html msg
neutral =
    banner neutralStyles


{-| A banner for success alerts
-}
success : String -> Html msg
success =
    banner successStyles


banner : Css.Style -> String -> Html msg
banner bannerType alertMessage =
    Html.styled Accessibility.div
        [ bannerStyles, bannerType ]
        []
        [ notification alertMessage ]


notification : String -> Html msg
notification message =
    Html.styled Html.div [ alertMessageStyles ] [] [ Accessibility.text message ]


type CssClasses
    = AlertMessage
    | Banner
    | Error
    | Neutral
    | Success


alertMessageStyles : Style
alertMessageStyles =
    batch
        [ Css.fontSize (Css.px 20)
        , Css.fontWeight (Css.int 700)
        , Css.lineHeight (Css.px 25)
        , Css.maxWidth (Css.px 600)
        , Nri.Ui.Fonts.V1.baseFont
        ]


bannerStyles : Style
bannerStyles =
    batch
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


errorStyles : Style
errorStyles =
    batch
        [ Css.backgroundColor Nri.Ui.Colors.V1.purpleLight
        , Css.color Nri.Ui.Colors.V1.purpleDark
        ]


neutralStyles : Style
neutralStyles =
    batch
        [ Css.backgroundColor Nri.Ui.Colors.V1.frost
        , Css.color Nri.Ui.Colors.V1.navy
        ]


successStyles : Style
successStyles =
    batch
        [ Css.backgroundColor Nri.Ui.Colors.V1.greenLightest
        , Css.color Nri.Ui.Colors.V1.greenDarkest
        ]
