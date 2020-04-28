module Nri.Ui.Alert.V4 exposing
    ( error, success, tip, warning
    , somethingWentWrong
    )

{-|


# DEPRECATED: Use `Nri.Ui.Message.V1.tiny` instead


# Changes from V3:

  - Changes the error font color from `purpleDark` to `purple`
  - Adds `somethingWentWrong` error for displaying stacktraces


# About:

UI components that highlight information to the user.

@docs error, success, tip, warning
@docs somethingWentWrong

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
import Nri.Ui.SpriteSheet exposing (bulb, checkmark, exclamationMark)
import Nri.Ui.Svg.V1 as NriSvg exposing (Svg)


{-|


# DEPRECATED: Use `Nri.Ui.Message.V1.somethingWentWrong` instead

-}
somethingWentWrong : String -> Html msg
somethingWentWrong errorMessageForEngineers =
    Html.div []
        [ error "Sorry, something went wrong.  Please try again later."
        , Html.details []
            [ Html.summary
                [ css
                    [ Fonts.baseFont
                    , Css.fontSize (Css.px 14)
                    , Css.color Colors.gray45
                    ]
                ]
                [ Html.text "Details for NoRedInk engineers" ]
            , Html.code
                [ css
                    [ Css.display Css.block
                    , Css.whiteSpace Css.normal
                    , Css.overflowWrap Css.breakWord
                    , Css.color Colors.gray45
                    , Css.backgroundColor Colors.gray96
                    , Css.border3 (Css.px 1) Css.solid Colors.gray92
                    , Css.borderRadius (Css.px 3)
                    , Css.padding2 (Css.px 2) (Css.px 4)
                    , Css.fontSize (Css.px 12)
                    , Css.fontFamily Css.monospace
                    ]
                ]
                [ Html.text errorMessageForEngineers ]
            ]
        ]


{-|


# DEPRECATED: Use `Nri.Ui.Message.V1.tiny Error` instead

-}
error : String -> Html msg
error content =
    alert
        [ exclamation Colors.purple
        , alertString Colors.purple content
        ]


{-|


# DEPRECATED: Use `Nri.Ui.Message.V1.tiny Success` instead

-}
success : String -> Html msg
success content =
    alert
        [ iconContainer
            [ Css.color Colors.white
            , Css.backgroundColor Colors.green
            ]
            (Html.div
                [ css [ Css.width (Css.px 12), Css.marginTop (Css.px 1) ] ]
                [ NriSvg.toHtml checkmark ]
            )
        , alertString Colors.greenDarkest content
        ]


{-|


# DEPRECATED: Use `Nri.Ui.Message.V1.tiny Tip` instead

-}
tip : String -> Html msg
tip content =
    alert
        [ iconContainer [ Css.color Colors.yellow ] (NriSvg.toHtml bulb)
        , alertString Colors.navy content
        ]


{-|


# DEPRECATED: Use `Nri.Ui.Message.V1.tiny Alert` instead

-}
warning : String -> Html msg
warning content =
    alert
        [ exclamation Colors.red
        , alertString Colors.redDark content
        ]


alert : List (Html msg) -> Html msg
alert =
    Nri.Ui.styled Html.div
        "Nri-Ui-Alert-V3__alert"
        [ Css.displayFlex
        , Css.justifyContent Css.start
        , Css.alignItems Css.flexStart
        , Css.paddingTop (Css.px 6)
        , Css.paddingBottom (Css.px 8)
        ]
        []


exclamation : Css.Color -> Html msg
exclamation backgroundColor =
    iconContainer
        [ Css.color Colors.white
        , Css.backgroundColor backgroundColor
        ]
        (Html.div
            [ css [ Css.height (Css.px 13), Css.marginTop (Css.px 1) ] ]
            [ NriSvg.toHtml exclamationMark ]
        )


iconContainer : List Css.Style -> Html msg -> Html msg
iconContainer styles icon =
    Nri.Ui.styled Html.div
        "Nri-Ui-Alert-V4__iconContainer"
        (styles
            ++ [ -- Content positioning
                 Css.displayFlex
               , Css.alignItems Css.center
               , Css.justifyContent Css.center
               , Css.marginRight (Css.px 5)
               , Css.lineHeight (Css.px 13)
               , Css.flexShrink Css.zero

               -- Size
               , Css.borderRadius (Css.px 13)
               , Css.height (Css.px 20)
               , Css.width (Css.px 20)
               ]
        )
        []
        [ icon ]


alertString : Css.ColorValue compatible -> String -> Html msg
alertString color content =
    Nri.Ui.styled Html.div
        "Nri-Ui-Alert-V4--alert"
        [ Css.color color
        , Fonts.baseFont
        , Css.fontSize (Css.px 13)

        --, Css.lineHeight (Css.px 20)
        , Css.listStyleType Css.none

        -- This global selector and overrides are necessary due to
        -- old stylesheets used on the monolith that set the
        -- `.txt p { font-size: 18px; }` -- without these overrides,
        -- we may see giant ugly alerts.
        -- Remove these if you want to! but be emotionally prepped
        -- to deal with visual regressions. 🙏
        , Css.Global.descendants
            [ Css.Global.p
                [ Css.margin Css.zero

                --, Css.lineHeight (Css.px 20)
                , Css.fontSize (Css.px 13)
                , Fonts.baseFont
                ]
            ]
        ]
        []
        (Markdown.toHtml Nothing content |> List.map fromUnstyled)
