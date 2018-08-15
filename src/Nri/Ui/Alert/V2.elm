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
import Css.Foreign exposing (Snippet, children, descendants, everything, selector)
import Html as RootHtml
import Html.Styled exposing (fromUnstyled)
import Html.Styled.Attributes exposing (css)
import Markdown
import Nri.Ui.AssetPath
import Nri.Ui.Colors.V1
import Nri.Ui.Icon.V3 as Icon


error : { r | attention_svg : Nri.Ui.AssetPath.Asset } -> String -> Html msg
error assets =
    alert (Icon.attention assets)
        [ Css.color Nri.Ui.Colors.V1.purple
        , Css.after [ Css.backgroundColor Nri.Ui.Colors.V1.purple ]
        ]


success : { r | checkmark : String } -> String -> Html msg
success assets =
    alert (Icon.checkMarkSvg assets)
        [ Css.color Nri.Ui.Colors.V1.greenDarkest
        , Css.after [ Css.backgroundColor Nri.Ui.Colors.V1.green ]
        ]


tip : { r | bulb : String } -> String -> Html msg
tip assets =
    alert (Icon.bulb assets)
        [ Css.color Nri.Ui.Colors.V1.navy
        , Css.after [ Css.backgroundColor Nri.Ui.Colors.V1.white ]
        ]


warning : { r | attention_svg : Nri.Ui.AssetPath.Asset } -> String -> Html msg
warning assets =
    alert (Icon.attention assets)
        [ Css.color Nri.Ui.Colors.V1.red
        , Css.after [ Css.backgroundColor Nri.Ui.Colors.V1.red ]
        ]


alert icon styles content =
    Html.div [ css (alertStyles ++ styles) ]
        [ Icon.decorativeIcon icon
        , RootHtml.div [] (Markdown.toHtml Nothing content)
            |> fromUnstyled
        ]


alertStyles : List Css.Style
alertStyles =
    [ Css.displayFlex
    , Css.fontSize (Css.px 13)
    , Css.lineHeight (Css.num 1.2)
    , Css.listStyleType Css.none
    , Css.overflow Css.hidden
    , Css.padding4 (Css.px 6) (Css.px 8) (Css.px 8) (Css.px 30)
    , Css.position Css.relative
    , Css.Foreign.children
        [ Css.Foreign.div
            [ Css.Foreign.children
                [ Css.Foreign.p
                    [ Css.margin Css.zero
                    ]
                ]
            ]
        ]
    , Css.after
        [ Css.backgroundPosition Css.center
        , Css.backgroundRepeat Css.noRepeat
        , Css.borderRadius (Css.px 13)
        , Css.height (Css.px 25)
        , Css.left Css.zero
        , Css.position Css.absolute
        , Css.property "content" "\"\""
        , Css.top Css.zero
        , Css.width (Css.px 25)
        ]
    ]
