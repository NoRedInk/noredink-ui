module Nri.Ui.BannerAlert.V5 exposing (alert, error, neutral, success, LinkConfig, BannerContent(..), Target(..))

{-|

@docs alert, error, neutral, success, LinkConfig, BannerContent, Target

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
import Nri.Ui.SpriteSheet exposing (bulb, checkmark, exclamationMark, xSvg)
import Nri.Ui.Svg.V1 as NriSvg exposing (Svg)


{-| A type to capture either plain content, or a string which will include a url link.
-}
type BannerContent
    = Plain String
    | WithLink LinkConfig


{-| A banner to show error alerts
-}
alert : BannerContent -> Maybe msg -> Html msg
alert =
    banner
        { backgroundColor = Colors.sunshine
        , color = Colors.navy
        , icon =
            { backgroundColor = Colors.yellow
            , height = Css.px 25
            , asset = exclamationMark
            }
        }


{-| A banner to show error alerts
-}
error : BannerContent -> Maybe msg -> Html msg
error =
    banner
        { backgroundColor = Colors.purpleLight
        , color = Colors.purpleDark
        , icon =
            { backgroundColor = Colors.purple
            , height = Css.px 25
            , asset = exclamationMark
            }
        }


{-| A banner to show neutral alerts
-}
neutral : BannerContent -> Maybe msg -> Html msg
neutral =
    banner
        { backgroundColor = Colors.frost
        , color = Colors.navy
        , icon =
            { backgroundColor = Colors.navy
            , height = Css.px 32
            , asset = bulb
            }
        }


{-| A banner for success alerts
-}
success : BannerContent -> Maybe msg -> Html msg
success =
    banner
        { backgroundColor = Colors.greenLightest
        , color = Colors.greenDarkest
        , icon =
            { backgroundColor = Colors.green
            , height = Css.px 20
            , asset = checkmark
            }
        }


type alias StyleConfig =
    { color : Css.Color
    , backgroundColor : Css.Color
    , icon : IconConfig
    }


banner : StyleConfig -> BannerContent -> Maybe msg -> Html msg
banner config bannerContent dismissMsg =
    let
        maybeDismissButton =
            case dismissMsg of
                Nothing ->
                    Html.text ""

                Just msg ->
                    dismissButton msg

        alertMessage =
            case bannerContent of
                Plain string ->
                    Html.text string

                WithLink linkConfig ->
                    Html.div
                        [ css
                            [ Css.fontSize (Css.px 20)
                            , Css.fontWeight (Css.int 700)
                            , Css.lineHeight (Css.px 25)
                            , Css.maxWidth (Css.px 600)
                            , Nri.Ui.Fonts.V1.baseFont
                            ]
                        ]
                        [ Html.text linkConfig.prefixText
                        , Html.a
                            [ Attributes.href <| linkConfig.linkUrl
                            , targetToAttribute linkConfig.target
                            ]
                            [ Html.text linkConfig.linkText ]
                        , Html.text linkConfig.postfixText
                        ]
    in
    Html.div
        [ css
            [ Css.displayFlex
            , Css.justifyContent Css.center
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
            [ icon config.icon
            , notification alertMessage
            ]
        , Html.span
            [ css
                [ Css.display Css.inlineBlock
                , Css.float Css.right
                ]
            ]
            [ maybeDismissButton ]
        ]


dismissButton : msg -> Html msg
dismissButton msg =
    Nri.Ui.styled Html.div
        "dismiss-button-container"
        [ Css.position Css.relative
        , Css.top (Css.px 5)
        , Css.right Css.zero
        , Css.padding (Css.px 25)
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
            [ NriSvg.toHtml xSvg
            ]
        ]


type alias IconConfig =
    { backgroundColor : Css.Color
    , height : Css.Px
    , asset : Svg
    }


icon : IconConfig -> Html msg
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
            [ NriSvg.toHtml config.asset ]
        ]


notification : Html msg -> Html msg
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
        [ message ]


{-| Config describing a message containing an embedded link.
-}
type alias LinkConfig =
    { prefixText : String
    , linkText : String
    , linkUrl : String
    , postfixText : String
    , target : Target
    }


{-| The link target. Corresponds to values of "_blank" and "_self"
-}
type Target
    = Blank
    | Self


targetToAttribute : Target -> Html.Attribute msg
targetToAttribute linkTarget =
    case linkTarget of
        Blank ->
            Attributes.target "_blank"

        Self ->
            Attributes.target "_self"
