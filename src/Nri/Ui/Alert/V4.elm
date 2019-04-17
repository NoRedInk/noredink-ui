module Nri.Ui.Alert.V4 exposing
    ( error, success, tip, warning
    , somethingWentWrong
    )

{-|


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
import Nri.Ui.Icon.V3 as Icon
import Nri.Ui.SpriteSheet exposing (bulb, checkmark, exclamationMark)
import Nri.Ui.Svg.V1 as NriSvg exposing (Svg)


{-| -}
somethingWentWrong : String -> Html msg
somethingWentWrong errorMessageForEngineers =
    Html.div []
        [ error "Sorry, something went wrong.  Please try again later."
        , Html.details []
            [ Html.summary
                [ css
                    [ Fonts.baseFont
                    , Css.fontSize (Css.px 14)
                    , Css.color Colors.gray75
                    , Css.marginLeft (Css.px 25)
                    ]
                ]
                [ Html.text "Details for NoRedInk engineers" ]
            , Html.code
                [ css
                    [ Css.display Css.block
                    , Css.whiteSpace Css.normal
                    , Css.overflowWrap Css.breakWord
                    , Css.color Colors.red
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


{-| import Nri.Ui.Alert.V4 as Alert

    view =
        Alert.error "Some **awesome** message!"

-}
error : String -> Html msg
error content =
    alert
        [ exclamation Colors.purple
        , alertString Colors.purple content
        ]


{-| import Nri.Ui.Alert.V4 as Alert

    view =
        Alert.success "Some **awesome** message!"

-}
success : String -> Html msg
success content =
    alert
        [ iconContainer
            [ Css.color Colors.white
            , Css.backgroundColor Colors.green
            ]
            (Html.div
                [ css
                    [ Css.width (Css.px 12)
                    , Css.height (Css.px 12)
                    , Css.margin Css.auto
                    ]
                ]
                [ NriSvg.toHtml checkmark ]
            )
        , alertString Colors.greenDarkest content
        ]


{-| import Nri.Ui.Alert.V4 as Alert

    view =
        Alert.tip "Some **awesome** message!"

-}
tip : String -> Html msg
tip content =
    alert
        [ iconContainer [ Css.color Colors.yellow ] (NriSvg.toHtml bulb)
        , alertString Colors.navy content
        ]


{-| import Nri.Ui.Alert.V4 as Alert

    view =
        Alert.warning "Some **awesome** message!"

-}
warning : String -> Html msg
warning content =
    alert
        [ exclamation Colors.red
        , alertString Colors.red content
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
            [ css [ Css.marginTop (Css.px 1), Css.height (Css.px 13) ] ]
            [ NriSvg.toHtml exclamationMark ]
        )


iconContainer : List Css.Style -> Html msg -> Html msg
iconContainer styles icon =
    Nri.Ui.styled Html.div
        "Nri-Ui-Alert-V3__iconContainer"
        (styles
            ++ [ -- Content positioning
                 Css.marginTop (Css.px -2)
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


alertString : Css.ColorValue compatible -> String -> Html msg
alertString color content =
    Nri.Ui.styled Html.div
        "Nri-Ui-Alert-V4--alert"
        [ Css.color color
        , Fonts.baseFont
        , Css.fontSize (Css.px 13)
        , Css.lineHeight (Css.num 1.2)
        , Css.listStyleType Css.none
        , Css.Global.descendants [ Css.Global.p [ Css.margin Css.zero ] ]
        ]
        []
        (Markdown.toHtml Nothing content |> List.map fromUnstyled)
