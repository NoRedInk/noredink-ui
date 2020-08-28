module Nri.Ui.Message.V2 exposing
    ( tiny, large, banner
    , Theme(..), Content(..), mapContent
    , Attribute
    , alert, alertDialog
    , onDismiss
    , somethingWentWrong
    )

{-| Changes from V1:

  - adds `alert`, `alertDialog` role attributes
  - rename BannerAttribute -> Attribute

@docs tiny, large, banner
@docs Theme, Content, mapContent

Attributes:

@docs Attribute
@docs alert, alertDialog
@docs onDismiss

@docs somethingWentWrong

-}

import Accessibility.Styled as Html exposing (..)
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Widget as Widget
import Css exposing (..)
import Css.Global
import Html.Styled exposing (styled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Markdown
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Svg.V1 as NriSvg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon


{-| `Error` / `Alert` / `Tip` / `Success`
-}
type Theme
    = Error
    | Alert
    | Tip
    | Success
    | Custom
        { color : Color
        , backgroundColor : Color
        , icon : Svg
        }


{-| Prefer using the simplest variant that meets your needs.

  - `Plain`: provide a plain-text string
  - `Markdown`: provide a string that will be rendered as markdown
  - `Html`: provide custom HTML

-}
type Content msg
    = Plain String
    | Markdown String
    | Html (List (Html msg))


{-| Transform the messages produced by some `Content`.
-}
mapContent : (a -> b) -> Content a -> Content b
mapContent f content =
    case content of
        Plain string ->
            Plain string

        Markdown string ->
            Markdown string

        Html html ->
            Html (List.map (Html.map f) html)


{-| PRIVATE
-}
contentToHtml : Content msg -> List (Html msg)
contentToHtml content =
    case content of
        Plain stringContent ->
            [ text stringContent ]

        Markdown markdownContent ->
            Markdown.toHtml Nothing markdownContent |> List.map fromUnstyled

        Html html ->
            html


{-| Shows a tiny alert message. We commonly use these for validation errors and small hints to users.

    import Nri.Ui.Message.V1 as Message

    view =
        Message.tiny Message.Tip (Message.Markdown "Don't tip too much, or your waitress will **fall over**!")

NOTE: When using a `Custom` theme, `tiny` ignores the custom `backgroundColor`.

-}
tiny : Theme -> Content msg -> Html msg
tiny theme content =
    let
        config =
            case theme of
                Error ->
                    { icon =
                        UiIcon.exclamation
                            |> NriSvg.withColor Colors.purple
                            |> NriSvg.withLabel "Error"
                    , fontColor = Colors.purple
                    }

                Alert ->
                    { icon =
                        UiIcon.exclamation
                            |> NriSvg.withColor Colors.red
                            |> NriSvg.withLabel "Alert"
                    , fontColor = Colors.redDark
                    }

                Tip ->
                    { icon =
                        UiIcon.bulb
                            |> NriSvg.withColor Colors.yellow
                            |> NriSvg.withLabel "Tip"
                    , fontColor = Colors.navy
                    }

                Success ->
                    { icon =
                        UiIcon.checkmarkInCircle
                            |> NriSvg.withColor Colors.green
                            |> NriSvg.withLabel "Success"
                    , fontColor = Colors.greenDarkest
                    }

                Custom customTheme ->
                    { icon = customTheme.icon
                    , fontColor = customTheme.color
                    }
    in
    Nri.Ui.styled div
        "Nri-Ui-Message-V1--tiny"
        [ displayFlex
        , justifyContent start
        , paddingTop (px 6)
        , paddingBottom (px 8)
        ]
        []
        [ styled div
            []
            []
            [ Nri.Ui.styled div
                "Nri-Ui-Message-V1--tinyIconContainer"
                [ -- Content positioning
                  displayFlex
                , alignItems center
                , justifyContent center
                , marginRight (px 5)
                , lineHeight (px 13)
                , flexShrink zero

                -- Size
                , borderRadius (px 13)
                , height (px 20)
                , width (px 20)
                ]
                []
                [ NriSvg.toHtml config.icon ]
            ]
        , styled div
            [ displayFlex
            , alignItems center
            ]
            []
            [ Nri.Ui.styled div
                "Nri-Ui-Message-V1--alert"
                [ color config.fontColor
                , Fonts.baseFont
                , fontSize (px 13)

                --, lineHeight (px 20)
                , listStyleType none

                -- This global selector and overrides are necessary due to
                -- old stylesheets used on the monolith that set the
                -- `.txt p { font-size: 18px; }` -- without these overrides,
                -- we may see giant ugly alerts.
                -- Remove these if you want to! but be emotionally prepped
                -- to deal with visual regressions. 🙏
                , Css.Global.descendants
                    [ Css.Global.p
                        [ margin zero

                        --, lineHeight (px 20)
                        , fontSize (px 13)
                        , Fonts.baseFont
                        ]
                    ]
                ]
                []
                (contentToHtml content)
            ]
        ]


{-| Shows a large alert or callout message. We commonly use these for highlighted tips, instructions, or asides in page copy.

    import Nri.Ui.Message.V1 as Message

    view =
        Message.large Message.Tip (Message.Plain "Two out of two parents agree: NoRedInk sounds like a fun place to work.")

-}
large : Theme -> Content msg -> Html msg
large theme content =
    let
        config =
            case theme of
                Error ->
                    { backgroundColor = Colors.purpleLight
                    , fontColor = Colors.purpleDark
                    , icon =
                        UiIcon.exclamation
                            |> NriSvg.withColor Colors.purple
                            |> NriSvg.withLabel "Error"
                    }

                Alert ->
                    { backgroundColor = Colors.sunshine
                    , fontColor = Colors.navy
                    , icon =
                        UiIcon.exclamation
                            |> NriSvg.withColor Colors.ochre
                            |> NriSvg.withLabel "Alert"
                    }

                Tip ->
                    { backgroundColor = Colors.sunshine
                    , fontColor = Colors.navy
                    , icon =
                        UiIcon.bulb
                            |> NriSvg.withColor Colors.navy
                            |> NriSvg.withLabel "Tip"
                    }

                Success ->
                    { backgroundColor = Colors.greenLightest
                    , fontColor = Colors.greenDarkest
                    , icon =
                        UiIcon.checkmarkInCircle
                            |> NriSvg.withColor Colors.green
                            |> NriSvg.withLabel "Success"
                    }

                Custom customTheme ->
                    { backgroundColor = customTheme.backgroundColor
                    , fontColor = customTheme.color
                    , icon = customTheme.icon
                    }
    in
    Nri.Ui.styled div
        "Nri-Ui-Message-V1--large"
        [ width (pct 100)
        , backgroundColor config.backgroundColor
        , Fonts.baseFont
        , fontSize (px 15)
        , lineHeight (px 21)
        , fontWeight (int 600)
        , boxSizing borderBox
        , padding (px 20)
        , borderRadius (px 8)
        , color config.fontColor
        , displayFlex
        , alignItems center
        , Css.Global.descendants
            [ Css.Global.a
                [ textDecoration none
                , color Colors.azure
                , borderBottom3 (px 1) solid Colors.azure
                , visited [ color Colors.azure ]
                ]
            ]
        ]
        []
        [ styled div
            [ width (px 35)
            , marginRight (px 10)
            ]
            []
            [ NriSvg.toHtml config.icon
            ]
        , styled div
            [ minWidth (px 100)
            , flexBasis (px 100)
            , flexGrow (int 1)
            ]
            []
            (contentToHtml content)
        ]


{-| Shows a banner alert message. This is even more prominent than `Message.large`.
We commonly use these for flash messages at the top of pages.

    import Nri.Ui.Message.V1 as Message

    view =
        Message.banner Message.Success
            (Message.Plain "John Jacob Jingleheimer Schmidt has been dropped from First Period English.")
            [ Message.alert ]

-}
banner : Theme -> Content msg -> List (Attribute msg) -> Html msg
banner theme content attr =
    let
        config =
            case theme of
                Error ->
                    { backgroundColor = Colors.purpleLight
                    , color = Colors.purpleDark
                    , icon =
                        UiIcon.exclamation
                            |> NriSvg.withColor Colors.purple
                            |> NriSvg.withLabel "Error"
                            |> NriSvg.toHtml
                    }

                Alert ->
                    { backgroundColor = Colors.sunshine
                    , color = Colors.navy
                    , icon =
                        UiIcon.exclamation
                            |> NriSvg.withColor Colors.ochre
                            |> NriSvg.withLabel "Alert"
                            |> NriSvg.toHtml
                    }

                Tip ->
                    { backgroundColor = Colors.frost
                    , color = Colors.navy
                    , icon =
                        inCircle
                            { backgroundColor = Colors.navy
                            , color = Colors.mustard
                            , height = Css.px 32
                            , icon = UiIcon.bulb
                            }
                    }

                Success ->
                    { backgroundColor = Colors.greenLightest
                    , color = Colors.greenDarkest
                    , icon =
                        UiIcon.checkmarkInCircle
                            |> NriSvg.withColor Colors.green
                            |> NriSvg.withLabel "Success"
                            |> NriSvg.toHtml
                    }

                Custom customTheme ->
                    { backgroundColor = customTheme.backgroundColor
                    , color = customTheme.color
                    , icon = NriSvg.toHtml customTheme.icon
                    }

        attributes =
            configFromAttributes attr
    in
    styled div
        [ displayFlex
        , justifyContent center
        , alignItems center
        , backgroundColor config.backgroundColor
        , color config.color
        ]
        (case attributes.role of
            Just AlertRole ->
                [ Role.alert ]

            Just AlertDialog ->
                [ Role.alertDialog ]

            Nothing ->
                []
        )
        [ styled span
            [ alignItems center
            , displayFlex
            , justifyContent center
            , padding (px 20)
            , width (Css.pct 100)
            , Css.Global.children
                [ Css.Global.button
                    [ position relative
                    , right (px 15)
                    ]
                ]
            ]
            []
            [ styled div
                [ width (px 50)
                , height (px 50)
                , marginRight (px 20)
                , -- NOTE: I think it's normally best to avoid relying on flexShrink (and use flexGrow/flexBasis) instead,
                  -- But using shrink here and on the next div lets us have the text content be centered rather than
                  -- left-aligned when the content is shorter than one line
                  flexShrink zero
                ]
                []
                [ config.icon ]
            , Nri.Ui.styled div
                "banner-alert-notification"
                [ fontSize (px 20)
                , fontWeight (int 700)
                , lineHeight (px 27)
                , maxWidth (px 600)
                , minWidth (px 100)
                , flexShrink (int 1)
                , Fonts.baseFont
                , Css.Global.descendants
                    [ Css.Global.a
                        [ textDecoration none
                        , color Colors.azure
                        , borderBottom3 (px 1) solid Colors.azure
                        , visited [ color Colors.azure ]
                        ]
                    ]
                ]
                []
                (contentToHtml content)
            ]
        , case attributes.onDismiss of
            Nothing ->
                text ""

            Just msg ->
                bannerDismissButton msg
        ]


{-| Shows an appropriate error message for when something unhandled happened.

    import Nri.Ui.Message.V1 as Message

    view maybeDetailedErrorMessage =
        viewMaybe Message.somethingWentWrong maybeDetailedErrorMessage

-}
somethingWentWrong : String -> Html msg
somethingWentWrong errorMessageForEngineers =
    div []
        [ tiny Error (Plain "Sorry, something went wrong.  Please try again later.")
        , details []
            [ summary
                [ css
                    [ Fonts.baseFont
                    , fontSize (px 14)
                    , color Colors.gray45
                    ]
                ]
                [ text "Details for NoRedInk engineers" ]
            , code
                [ css
                    [ display block
                    , whiteSpace normal
                    , overflowWrap breakWord
                    , color Colors.gray45
                    , backgroundColor Colors.gray96
                    , border3 (px 1) solid Colors.gray92
                    , borderRadius (px 3)
                    , padding2 (px 2) (px 4)
                    , fontSize (px 12)
                    , fontFamily monospace
                    ]
                ]
                [ text errorMessageForEngineers ]
            ]
        ]


{-| Adds a dismiss ("X" icon) to a message which will produce the given `msg` when clicked.
-}
onDismiss : msg -> Attribute msg
onDismiss msg =
    Attribute <|
        \config ->
            { config | onDismiss = Just msg }


{-| -}
alert : Attribute msg
alert =
    Attribute <|
        \config ->
            { config | role = Just AlertRole }


{-| -}
alertDialog : Attribute msg
alertDialog =
    Attribute <|
        \config ->
            { config | role = Just AlertDialog }



--
-- PRIVATE
--


{-| Construct an `Attribute` using a helper like `onDismiss` or `alert`.
-}
type Attribute msg
    = Attribute (BannerConfig msg -> BannerConfig msg)


{-| PRIVATE
-}
type alias BannerConfig msg =
    { onDismiss : Maybe msg
    , role : Maybe Role
    }


type Role
    = AlertRole
    | AlertDialog


{-| PRIVATE
-}
configFromAttributes : List (Attribute msg) -> BannerConfig msg
configFromAttributes attr =
    List.foldl (\(Attribute set) -> set)
        { onDismiss = Nothing
        , role = Nothing
        }
        attr


inCircle :
    { backgroundColor : Css.Color
    , color : Css.Color
    , height : Css.Px
    , icon : Svg
    }
    -> Html msg
inCircle config =
    styled div
        [ borderRadius (pct 50)
        , height (pct 100)
        , backgroundColor config.backgroundColor
        , displayFlex
        , alignItems center
        , justifyContent center
        ]
        []
        [ config.icon
            |> NriSvg.withColor config.color
            |> NriSvg.withHeight config.height
            |> NriSvg.toHtml
        ]


bannerDismissButton : msg -> Html msg
bannerDismissButton msg =
    Nri.Ui.styled div
        "dismiss-button-container"
        [ padding (px 25)
        ]
        []
        [ styled button
            [ borderWidth zero
            , backgroundColor unset
            , color Colors.azure
            , width (px 30)
            , height (px 30)
            , padding2 zero (px 7)
            , cursor pointer
            ]
            [ onClick msg
            , Widget.label "Dismiss banner"
            ]
            [ NriSvg.toHtml UiIcon.x
            ]
        ]
