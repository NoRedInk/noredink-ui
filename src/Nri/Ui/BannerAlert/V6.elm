module Nri.Ui.BannerAlert.V6 exposing
    ( alert, error, neutral, success
    , custom
    )

{-|

@docs alert, error, neutral, success
@docs custom


# Post-release patches

  - adjusts link styles
  - exposes `custom` banner-maker

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
alert content maybeDismiss =
    banner
        { backgroundColor = Colors.sunshine
        , color = Colors.navy
        , icon =
            inCircle
                { backgroundColor = Colors.ochre
                , color = Colors.white
                , height = Css.px 25
                , icon = UiIcon.attention
                }
        , content = content
        , dismiss = maybeDismiss
        }


{-| A banner to show error alerts
-}
error : List (Html msg) -> Maybe msg -> Html msg
error content maybeDismiss =
    banner
        { backgroundColor = Colors.purpleLight
        , color = Colors.purpleDark
        , icon =
            inCircle
                { backgroundColor = Colors.purple
                , color = Colors.white
                , height = Css.px 25
                , icon = UiIcon.attention
                }
        , content = content
        , dismiss = maybeDismiss
        }


{-| A banner to show neutral alerts
-}
neutral : List (Html msg) -> Maybe msg -> Html msg
neutral content maybeDismiss =
    banner
        { backgroundColor = Colors.frost
        , color = Colors.navy
        , icon =
            inCircle
                { backgroundColor = Colors.navy
                , color = Colors.mustard
                , height = Css.px 32
                , icon = UiIcon.bulb
                }
        , content = content
        , dismiss = maybeDismiss
        }


{-| A banner for success alerts
-}
success : List (Html msg) -> Maybe msg -> Html msg
success content maybeDismiss =
    banner
        { backgroundColor = Colors.greenLightest
        , color = Colors.greenDarkest
        , icon =
            inCircle
                { backgroundColor = Colors.green
                , color = Colors.white
                , height = Css.px 20
                , icon = UiIcon.checkmark
                }
        , content = content
        , dismiss = maybeDismiss
        }


{-| Use to construct a custom banner. Prefer to use a pre-made banner when possible.
-}
custom :
    { color : Css.Color
    , backgroundColor : Css.Color
    , icon : Svg
    , content : List (Html msg)
    , dismiss : Maybe msg
    }
    -> Html msg
custom config =
    banner
        { color = config.color
        , backgroundColor = config.backgroundColor
        , icon = Svg.toHtml config.icon
        , content = config.content
        , dismiss = config.dismiss
        }


banner :
    { color : Css.Color
    , backgroundColor : Css.Color
    , icon : Html Never
    , content : List (Html msg)
    , dismiss : Maybe msg
    }
    -> Html msg
banner config =
    let
        maybeDismissButton =
            case config.dismiss of
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
            [ iconContainer [ config.icon ]
                |> Html.map never
            , notification config.content
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


iconContainer : List (Html msg) -> Html msg
iconContainer =
    Html.div
        [ css
            [ Css.width (Css.px 50)
            , Css.height (Css.px 50)
            , Css.marginRight (Css.px 20)
            ]
        ]


inCircle :
    { backgroundColor : Css.Color
    , color : Css.Color
    , height : Css.Px
    , icon : Svg
    }
    -> Html msg
inCircle config =
    Html.div
        [ css
            [ Css.borderRadius (Css.pct 50)
            , Css.height (Css.pct 100)
            , Css.backgroundColor config.backgroundColor
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            ]
        ]
        [ config.icon
            |> Svg.withColor config.color
            |> Svg.withHeight config.height
            |> Svg.toHtml
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
