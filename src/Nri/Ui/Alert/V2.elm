module Nri.Ui.Alert.V2
    exposing
        ( error
        , success
        , tip
        , warning
        )

{-| UI components that highlight information to the user.

@docs error
@docs success
@docs tip
@docs warning

-}

import Accessibility.Styled as Html exposing (Html)
import Css
import Css.Foreign
import Html.Styled exposing (fromUnstyled)
import Html.Styled.Attributes exposing (css)
import Markdown
import Nri.Ui.AssetPath
import Nri.Ui.Colors.V1
import Nri.Ui.Icon.V3 as Icon


{-| -}
error : { r | attention_svg : Nri.Ui.AssetPath.Asset } -> String -> Html msg
error assets =
    alert (Icon.attention assets)
        Nri.Ui.Colors.V1.purple
        Nri.Ui.Colors.V1.purple


{-| -}
success : { r | checkmark : String } -> String -> Html msg
success assets =
    alert (Icon.checkMarkSvg assets)
        Nri.Ui.Colors.V1.green
        Nri.Ui.Colors.V1.greenDarkest


{-| -}
tip : { r | bulb : String } -> String -> Html msg
tip assets =
    alert (Icon.bulb assets)
        Nri.Ui.Colors.V1.white
        Nri.Ui.Colors.V1.navy


{-| -}
warning : { r | attention_svg : Nri.Ui.AssetPath.Asset } -> String -> Html msg
warning assets =
    alert (Icon.attention assets)
        Nri.Ui.Colors.V1.red
        Nri.Ui.Colors.V1.red


alert : Icon.IconType -> Css.ColorValue compatible -> Css.ColorValue compatible -> String -> Html msg
alert icon iconBackgroundColor color content =
    Html.div [ css alertStyles ]
        [ viewIcon icon iconBackgroundColor
        , viewAlertContent color content
        ]


viewIcon : Icon.IconType -> Css.ColorValue compatible -> Html msg
viewIcon icon iconBackgroundColor =
    Html.div
        [ css
            [ Css.backgroundColor iconBackgroundColor

            -- Positioning
            , Css.position Css.absolute
            , Css.left Css.zero
            , Css.top Css.zero

            -- Size
            , Css.borderRadius (Css.px 13)
            , Css.height (Css.px 25)
            , Css.width (Css.px 25)
            ]
        ]
        [ Icon.decorativeIcon icon ]


viewAlertContent : Css.ColorValue compatible -> String -> Html.Styled.Html msg
viewAlertContent color content =
    Html.div
        [ css
            [ Css.color color
            , Css.fontSize (Css.px 13)
            , Css.lineHeight (Css.num 1.2)
            , Css.listStyleType Css.none
            , Css.Foreign.descendants [ Css.Foreign.p [ Css.margin Css.zero ] ]
            ]
        ]
        (Markdown.toHtml Nothing content |> List.map fromUnstyled)


alertStyles : List Css.Style
alertStyles =
    [ Css.displayFlex
    , Css.overflow Css.hidden
    , Css.padding4 (Css.px 6) (Css.px 8) (Css.px 8) (Css.px 30)
    , Css.position Css.relative
    ]
