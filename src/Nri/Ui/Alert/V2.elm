module Nri.Ui.Alert.V2 exposing
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
import Css.Global
import Html.Styled exposing (fromUnstyled)
import Html.Styled.Attributes exposing (css)
import Markdown
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Icon.V3 as Icon


{-| -}
error : { r | exclamation : String } -> String -> Html msg
error assets content =
    alert
        [ iconContainer [ Css.color Colors.purple ]
            (Icon.decorativeIcon (Icon.exclamation assets))
        , viewAlertContent Colors.purpleDark content
        ]


{-| -}
success : { r | checkmark : String } -> String -> Html msg
success assets content =
    alert
        [ iconContainer
            [ Css.color Colors.white
            , Css.backgroundColor Colors.green
            , Css.Global.children [ Css.Global.svg [ Css.maxWidth (Css.px 12) ] ]
            ]
            (Icon.decorativeIcon (Icon.checkMarkSvg assets))
        , viewAlertContent Colors.greenDarkest content
        ]


{-| -}
tip : { r | bulb : String } -> String -> Html msg
tip assets content =
    alert
        [ iconContainer [ Css.color Colors.yellow ]
            (Icon.decorativeIcon (Icon.bulb assets))
        , viewAlertContent Colors.navy content
        ]


{-| -}
warning : { r | exclamation : String } -> String -> Html msg
warning assets content =
    alert
        [ iconContainer [ Css.color Colors.red ]
            (Icon.decorativeIcon (Icon.exclamation assets))
        , viewAlertContent Colors.red content
        ]


alert : List (Html msg) -> Html msg
alert =
    Nri.Ui.styled Html.div
        "Nri-Ui-Alert-V2__alert"
        [ Css.displayFlex
        , Css.justifyContent Css.start
        , Css.alignItems Css.center
        , Css.paddingTop (Css.px 6)
        , Css.paddingBottom (Css.px 8)
        ]
        []


iconContainer : List Css.Style -> Html msg -> Html msg
iconContainer styles icon =
    Nri.Ui.styled Html.div
        "Nri-Ui-Alert-V2__iconContainer"
        (styles
            ++ [ -- Content positioning
                 Css.displayFlex
               , Css.justifyContent Css.center
               , Css.alignItems Css.center
               , Css.marginRight (Css.px 5)

               -- Size
               , Css.borderRadius (Css.px 13)
               , Css.maxHeight (Css.px 20)
               , Css.maxWidth (Css.px 20)
               , Css.minHeight (Css.px 20)
               , Css.minWidth (Css.px 20)
               ]
        )
        []
        [ icon ]


viewAlertContent : Css.ColorValue compatible -> String -> Html.Styled.Html msg
viewAlertContent color content =
    Nri.Ui.styled Html.div
        "Nri-Ui-Alert-V2__viewAlertContent"
        [ Css.color color
        , Fonts.baseFont
        , Css.fontSize (Css.px 13)
        , Css.lineHeight (Css.num 1.2)
        , Css.listStyleType Css.none
        , Css.Global.descendants [ Css.Global.p [ Css.margin Css.zero ] ]
        ]
        []
        (Markdown.toHtml Nothing content |> List.map fromUnstyled)
