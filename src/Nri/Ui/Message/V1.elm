module Nri.Ui.Message.V1 exposing
    ( tiny, banner
    , Theme(..), Content(..), mapContent, BannerAttribute
    , onDismiss
    , somethingWentWrong
    )

{-|

@docs tiny, banner
@docs Theme, Content, mapContent, BannerAttribute
@docs onDismiss

@docs somethingWentWrong

-}

import Accessibility.Styled as Html exposing (..)
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
import Nri.Ui.SpriteSheet as SpriteSheet
import Nri.Ui.Svg.V1 as NriSvg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon


{-| `Error` / `Warning` / `Tip` / `Success`
-}
type Theme
    = Error
    | Warning
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
        children =
            case theme of
                Error ->
                    [ exclamation Colors.purple
                    , alertString Colors.purple content
                    ]

                Warning ->
                    [ exclamation Colors.red
                    , alertString Colors.redDark content
                    ]

                Tip ->
                    [ iconContainer [ color Colors.yellow ] (NriSvg.toHtml SpriteSheet.bulb)
                    , alertString Colors.navy content
                    ]

                Success ->
                    [ iconContainer
                        [ color Colors.white
                        , backgroundColor Colors.green
                        ]
                        (div
                            [ css [ width (px 12), marginTop (px 1) ] ]
                            [ NriSvg.toHtml SpriteSheet.checkmark ]
                        )
                    , alertString Colors.greenDarkest content
                    ]

                Custom config ->
                    [ iconContainer [ color config.color ] (NriSvg.toHtml config.icon)
                    , alertString config.color content
                    ]
    in
    Nri.Ui.styled div
        "Nri-Ui-Message-V1--tiny"
        [ displayFlex
        , justifyContent start
        , alignItems flexStart
        , paddingTop (px 6)
        , paddingBottom (px 8)
        ]
        []
        children


{-| PRIVATE
-}
type BannerAttribute msg
    = BannerAttribute (BannerConfig msg -> BannerConfig msg)


{-| Adds a dismiss ("X" icon) to a banner which will produce the given `msg` when clicked.
-}
onDismiss : msg -> BannerAttribute msg
onDismiss msg =
    BannerAttribute <|
        \config ->
            { config | onDismiss = Just msg }


{-| PRIVATE
-}
type alias BannerConfig msg =
    { onDismiss : Maybe msg
    }


{-| PRIVATE
-}
bannerConfigFromAttributes : List (BannerAttribute msg) -> BannerConfig msg
bannerConfigFromAttributes attr =
    List.foldl (\(BannerAttribute set) -> set)
        { onDismiss = Nothing }
        attr


{-| Shows a banner alert message. This is even more prominent than `Message.large`.
We commonly use these for flash messages at the top of pages.

    import Nri.Ui.Message.V1 as Message

    view =
        Message.banner Message.Success (Message.Plain "John Jacob Jingleheimer Schmidt has been dropped from First Period English.")

-}
banner : Theme -> Content msg -> List (BannerAttribute msg) -> Html msg
banner theme content attr =
    let
        config =
            case theme of
                Error ->
                    { backgroundColor = Colors.purpleLight
                    , color = Colors.purpleDark
                    , icon =
                        inCircle
                            { backgroundColor = Colors.purple
                            , color = Colors.white
                            , height = Css.px 25
                            , icon = UiIcon.attention
                            }
                    }

                Warning ->
                    { backgroundColor = Colors.sunshine
                    , color = Colors.navy
                    , icon =
                        inCircle
                            { backgroundColor = Colors.ochre
                            , color = Colors.white
                            , height = Css.px 25
                            , icon = UiIcon.attention
                            }
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
                        inCircle
                            { backgroundColor = Colors.green
                            , color = Colors.white
                            , height = Css.px 20
                            , icon = UiIcon.checkmark
                            }
                    }

                Custom customTheme ->
                    { backgroundColor = customTheme.backgroundColor
                    , color = customTheme.color
                    , icon = NriSvg.toHtml customTheme.icon
                    }

        attributes =
            bannerConfigFromAttributes attr
    in
    styled div
        [ displayFlex
        , justifyContent center
        , alignItems center
        , backgroundColor config.backgroundColor
        , color config.color
        ]
        []
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
                ]
                []
                [ config.icon ]
            , Nri.Ui.styled div
                "banner-alert-notification"
                [ fontSize (px 20)
                , fontWeight (int 700)
                , lineHeight (px 27)
                , maxWidth (px 600)
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



--
-- PRIVATE
--


exclamation : Color -> Html msg
exclamation iconColor =
    iconContainer
        [ color Colors.white
        , backgroundColor iconColor
        ]
        (styled div
            [ height (px 13)
            , marginTop (px 1)
            ]
            []
            [ NriSvg.toHtml SpriteSheet.exclamationMark ]
        )


iconContainer : List Style -> Html msg -> Html msg
iconContainer styles icon =
    Nri.Ui.styled div
        "Nri-Ui-Message-V1--iconContainer"
        (styles
            ++ [ -- Content positioning
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
        )
        []
        [ icon ]


alertString : ColorValue compatible -> Content msg -> Html msg
alertString textColor content =
    Nri.Ui.styled div
        "Nri-Ui-Message-V1--alert"
        [ color textColor
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
