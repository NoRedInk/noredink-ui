module Nri.Ui.BannerAlert.V6 exposing (alert, error, neutral, success)

{-|

@docs alert, error, neutral, success


# Post-release patches

  - adjusts link styles

Changes from V5:

  - takes HTML rather than BannerContent

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Widget as Widget
import Css
import Css.Global
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon


{-| A banner to show error alerts
-}
alert : List (Html msg) -> Maybe msg -> Html msg
alert =
    banner
        { backgroundColor = Colors.sunshine
        , color = Colors.navy
        , icon =
            icon
                { backgroundColor = Colors.ochre
                , height = Css.px 25
                , asset = UiIcon.attention
                }
        }


{-| A banner to show error alerts
-}
error : List (Html msg) -> Maybe msg -> Html msg
error =
    banner
        { backgroundColor = Colors.purpleLight
        , color = Colors.purpleDark
        , icon =
            icon
                { backgroundColor = Colors.purple
                , height = Css.px 25
                , asset = UiIcon.attention
                }
        }


{-| A banner to show neutral alerts
-}
neutral : List (Html msg) -> Maybe msg -> Html msg
neutral =
    banner
        { backgroundColor = Colors.frost
        , color = Colors.navy
        , icon =
            icon
                { backgroundColor = Colors.navy
                , height = Css.px 32
                , asset =
                    UiIcon.bulb
                        |> Svg.withColor Colors.mustard
                        |> Svg.withWidth (Css.px 34)
                        |> Svg.withHeight (Css.px 32)
                }
        }


{-| A banner for success alerts
-}
success : List (Html msg) -> Maybe msg -> Html msg
success =
    banner
        { backgroundColor = Colors.greenLightest
        , color = Colors.greenDarkest
        , icon =
            icon
                { backgroundColor = Colors.green
                , height = Css.px 20
                , asset = UiIcon.checkmark
                }
        }


banner :
    { color : Css.Color
    , backgroundColor : Css.Color
    , icon : Html Never
    }
    -> List (Html msg)
    -> Maybe msg
    -> Html msg
banner config bannerContent dismissMsg =
    let
        maybeDismissButton =
            case dismissMsg of
                Nothing ->
                    Html.text ""

                Just msg ->
                    dismissButton msg
    in
    Html.div
        [ css
            [ Css.displayFlex
            , Css.justifyContent Css.center
            , Css.alignItems Css.center
            , Css.backgroundColor config.backgroundColor
            , Css.color config.color
            ]
        ]
        [ Html.span
            [ css
                [ Css.alignItems Css.center
                , Css.displayFlex
                , Css.justifyContent Css.center
                , Css.padding (Css.px 20)
                , Css.width (Css.pct 100)
                , Css.Global.children
                    [ Css.Global.button
                        [ Css.position Css.relative
                        , Css.right (Css.px 15)
                        ]
                    ]
                ]
            ]
            [ Html.map never config.icon
            , notification bannerContent
            ]
        , maybeDismissButton
        ]


dismissButton : msg -> Html msg
dismissButton msg =
    Nri.Ui.styled Html.div
        "dismiss-button-container"
        [ Css.padding (Css.px 25)
        ]
        []
        [ Html.button
            [ Html.Styled.Events.onClick msg
            , Widget.label "Dismiss banner"
            , css
                [ Css.borderWidth Css.zero
                , Css.backgroundColor Css.unset
                , Css.color Colors.azure
                , Css.width (Css.px 30)
                , Css.height (Css.px 30)
                , Css.padding2 Css.zero (Css.px 7)
                , Css.cursor Css.pointer
                ]
            ]
            [ Svg.toHtml UiIcon.x
            ]
        ]


icon :
    { backgroundColor : Css.Color
    , height : Css.Px
    , asset : Svg
    }
    -> Html Never
icon config =
    Html.div
        [ css
            [ Css.boxSizing Css.borderBox
            , Css.borderRadius (Css.pct 50)
            , Css.color Colors.white
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.width (Css.px 50)
            , Css.height (Css.px 50)
            , Css.marginRight (Css.px 20)
            , Css.padding (Css.px 8)
            , Css.flexShrink (Css.num 0)
            , Css.backgroundColor config.backgroundColor
            ]
        ]
        [ Html.div
            [ css [ Css.height config.height ]
            ]
            [ Svg.toHtml config.asset ]
        ]


notification : List (Html msg) -> Html msg
notification =
    Nri.Ui.styled Html.div
        "banner-alert-notification"
        [ Css.fontSize (Css.px 20)
        , Css.fontWeight (Css.int 700)
        , Css.lineHeight (Css.px 27)
        , Css.maxWidth (Css.px 600)
        , Nri.Ui.Fonts.V1.baseFont
        , Css.Global.descendants
            [ Css.Global.a
                [ Css.textDecoration Css.none
                , Css.color Colors.azure
                , Css.borderBottom3 (Css.px 1) Css.solid Colors.azure
                , Css.visited
                    [ Css.color Colors.azure ]
                ]
            ]
        ]
        []
