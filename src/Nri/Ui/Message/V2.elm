module Nri.Ui.Message.V2 exposing
    ( view
    , Attribute
    , tiny, large, banner
    , plaintext, markdown, html
    , tip, error, alert, success, customTheme
    , alertRole, alertDialogRole
    , onDismiss
    , somethingWentWrong
    )

{-| Changes from V1:

  - adds `alertRole`, `alertDialogRole` role attributes
  - rename BannerAttribute -> Attribute
  - accept Attributes on any Message type
  - :skull: remove mapContent
  - expose `plaintext`, `markdown`, and `html` Attribute helpers instead of having `Content(..)` in the view APIs
  - expose theme Attribute helpers instead of having `Theme(..)` in the view APIs
  - exposes a singular view function (tiny, large, and banner are now attributes)

@docs view

Attributes:

@docs Attribute
@docs tiny, large, banner
@docs plaintext, markdown, html
@docs tip, error, alert, success, customTheme
@docs alertRole, alertDialogRole
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
import Nri.Ui.ClickableSvg.V1 as ClickableSvg
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Svg.V1 as NriSvg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon


{-|

    view =
        Message.view
            [ Message.tip
            , Message.markdown "Don't tip too much, or your waitress will **fall over**!"
            ]

-}
view : List (Attribute msg) -> Html msg
view attributes_ =
    let
        attributes =
            configFromAttributes attributes_

        role =
            getRoleAttribute attributes.role

        html_ =
            contentToHtml attributes.content

        color_ =
            getColor attributes.size attributes.theme

        icon =
            getIcon attributes.size attributes.theme
    in
    case attributes.size of
        Tiny ->
            Nri.Ui.styled div
                "Nri-Ui-Message-V2--tiny"
                [ displayFlex
                , justifyContent start
                , paddingTop (px 6)
                , paddingBottom (px 8)
                , color color_
                , styleOverrides
                ]
                role
            <|
                viewTiny html_ attributes.onDismiss icon

        Large ->
            Nri.Ui.styled div
                "Nri-Ui-Message-V2--large"
                [ width (pct 100)
                , getBackgroundColor attributes.size attributes.theme
                , Fonts.baseFont
                , fontSize (px 15)
                , lineHeight (px 21)
                , fontWeight (int 600)
                , boxSizing borderBox
                , padding (px 20)
                , borderRadius (px 8)
                , color color_
                , displayFlex
                , alignItems center
                , styleOverrides
                ]
                role
            <|
                viewLarge html_ attributes.onDismiss icon

        Banner ->
            styled div
                [ displayFlex
                , justifyContent center
                , alignItems center
                , getBackgroundColor attributes.size attributes.theme
                , color color_
                , styleOverrides
                ]
                role
            <|
                viewBanner html_ attributes.onDismiss icon


{-| Shows an appropriate error message for when something unhandled happened.

    view maybeDetailedErrorMessage =
        viewMaybe Message.somethingWentWrong maybeDetailedErrorMessage

-}
somethingWentWrong : String -> Html msg
somethingWentWrong errorMessageForEngineers =
    div []
        [ view
            [ tiny
            , error
            , plaintext "Sorry, something went wrong.  Please try again later."
            ]
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


{-| Shows a tiny alert message. We commonly use these for validation errors and small hints to users.

    Message.view [ Message.tiny ]

This is the default size for a Message.

-}
tiny : Attribute msg
tiny =
    Attribute <| \config -> { config | size = Tiny }


{-| Shows a large alert or callout message. We commonly use these for highlighted tips, instructions, or asides in page copy.

    Message.view [ Message.large ]

-}
large : Attribute msg
large =
    Attribute <| \config -> { config | size = Large }


{-| Shows a banner alert message. This is even more prominent than `Message.large`.
We commonly use these for flash messages at the top of pages.

    Message.view [ Message.banner ]

-}
banner : Attribute msg
banner =
    Attribute <| \config -> { config | size = Banner }


{-| -}
plaintext : String -> Attribute msg
plaintext content =
    Attribute <| \config -> { config | content = Plain content }


{-| -}
markdown : String -> Attribute msg
markdown content =
    Attribute <| \config -> { config | content = Markdown content }


{-| -}
html : List (Html msg) -> Attribute msg
html content =
    Attribute <| \config -> { config | content = Html content }


{-| This is the default theme for a Message.
-}
tip : Attribute msg
tip =
    Attribute <| \config -> { config | theme = Tip }


{-| -}
error : Attribute msg
error =
    Attribute <| \config -> { config | theme = Error }


{-| -}
alert : Attribute msg
alert =
    Attribute <| \config -> { config | theme = Alert }


{-| -}
success : Attribute msg
success =
    Attribute <| \config -> { config | theme = Success }


{-| -}
customTheme : { color : Color, backgroundColor : Color, icon : Svg } -> Attribute msg
customTheme custom_ =
    Attribute <| \config -> { config | theme = Custom custom_ }


{-| Adds a dismiss ("X" icon) to a message which will produce the given `msg` when clicked.
-}
onDismiss : msg -> Attribute msg
onDismiss msg =
    Attribute <| \config -> { config | onDismiss = Just msg }


{-| -}
alertRole : Attribute msg
alertRole =
    Attribute <| \config -> { config | role = Just AlertRole }


{-| -}
alertDialogRole : Attribute msg
alertDialogRole =
    Attribute <| \config -> { config | role = Just AlertDialog }



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
    , content : Content msg
    , theme : Theme
    , size : Size
    }


{-| PRIVATE
-}
configFromAttributes : List (Attribute msg) -> BannerConfig msg
configFromAttributes attr =
    List.foldl (\(Attribute set) -> set)
        { onDismiss = Nothing
        , role = Nothing
        , content = Plain ""
        , theme = Tip
        , size = Tiny
        }
        attr



-- Size


type Size
    = Tiny
    | Large
    | Banner



-- Message contents


{-| Prefer using the simplest variant that meets your needs.

  - `Plain`: provide a plain-text string
  - `Markdown`: provide a string that will be rendered as markdown
  - `Html`: provide custom HTML

-}
type Content msg
    = Plain String
    | Markdown String
    | Html (List (Html msg))


contentToHtml : Content msg -> List (Html msg)
contentToHtml content =
    case content of
        Plain stringContent ->
            [ text stringContent ]

        Markdown markdownContent ->
            Markdown.toHtml Nothing markdownContent |> List.map fromUnstyled

        Html html_ ->
            html_



-- Themes


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


getColor : Size -> Theme -> Color
getColor size theme =
    case theme of
        Custom { color } ->
            color

        Error ->
            case size of
                Tiny ->
                    Colors.purple

                _ ->
                    Colors.purpleDark

        Alert ->
            case size of
                Tiny ->
                    Colors.redDark

                _ ->
                    Colors.navy

        Tip ->
            Colors.navy

        Success ->
            Colors.greenDarkest


getBackgroundColor : Size -> Theme -> Style
getBackgroundColor size theme =
    case ( size, theme ) of
        ( _, Custom { backgroundColor } ) ->
            Css.backgroundColor backgroundColor

        ( Tiny, _ ) ->
            Css.batch []

        ( Large, Tip ) ->
            Css.backgroundColor Colors.sunshine

        ( Banner, Tip ) ->
            Css.backgroundColor Colors.frost

        ( _, Error ) ->
            Css.backgroundColor Colors.purpleLight

        ( _, Alert ) ->
            Css.backgroundColor Colors.sunshine

        ( _, Success ) ->
            Css.backgroundColor Colors.greenLightest


getIcon : Size -> Theme -> Html msg
getIcon size theme =
    case theme of
        Error ->
            UiIcon.exclamation
                |> NriSvg.withColor Colors.purple
                |> NriSvg.withLabel "Error"
                |> NriSvg.toHtml

        Alert ->
            let
                color =
                    case size of
                        Tiny ->
                            Colors.red

                        _ ->
                            Colors.ochre
            in
            UiIcon.exclamation
                |> NriSvg.withColor color
                |> NriSvg.withLabel "Alert"
                |> NriSvg.toHtml

        Tip ->
            case size of
                Tiny ->
                    UiIcon.bulb
                        |> NriSvg.withColor Colors.yellow
                        |> NriSvg.withLabel "Tip"
                        |> NriSvg.toHtml

                Large ->
                    UiIcon.bulb
                        |> NriSvg.withColor Colors.navy
                        |> NriSvg.withLabel "Tip"
                        |> NriSvg.toHtml

                Banner ->
                    inCircle
                        { backgroundColor = Colors.navy
                        , color = Colors.mustard
                        , height = Css.px 32
                        , icon = UiIcon.bulb
                        }

        Success ->
            UiIcon.checkmarkInCircle
                |> NriSvg.withColor Colors.green
                |> NriSvg.withLabel "Success"
                |> NriSvg.toHtml

        Custom { icon } ->
            NriSvg.toHtml icon



-- Role


type Role
    = AlertRole
    | AlertDialog


getRoleAttribute : Maybe Role -> List (Html.Attribute msg)
getRoleAttribute role =
    case role of
        Just AlertRole ->
            [ Role.alert ]

        Just AlertDialog ->
            [ Role.alertDialog ]

        Nothing ->
            []



-- Rendering icon helpers


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



-- Views


viewTiny content onDismiss_ icon =
    [ viewTinyIcon icon
    , styled div
        [ displayFlex
        , alignItems center
        ]
        []
        [ Nri.Ui.styled div
            "Nri-Ui-Message-V2--alert"
            [ Fonts.baseFont
            , fontSize (px 13)
            , listStyleType none
            ]
            []
            content
        ]
    , case onDismiss_ of
        Nothing ->
            text ""

        Just msg ->
            tinyDismissButton msg
    ]


viewLarge content onDismiss_ icon =
    [ viewLargeIcon icon
    , styled div
        [ minWidth (px 100)
        , flexBasis (px 100)
        , flexGrow (int 1)
        ]
        []
        content
    , case onDismiss_ of
        Nothing ->
            text ""

        Just msg ->
            largeDismissButton msg
    ]


viewBanner content onDismiss_ icon =
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
        [ viewBannerIcon icon
        , Nri.Ui.styled div
            "banner-alert-notification"
            [ fontSize (px 20)
            , fontWeight (int 700)
            , lineHeight (px 27)
            , maxWidth (px 600)
            , minWidth (px 100)
            , flexShrink (int 1)
            , Fonts.baseFont
            ]
            []
            content
        ]
    , case onDismiss_ of
        Nothing ->
            text ""

        Just msg ->
            bannerDismissButton msg
    ]



-- Style overrides


styleOverrides : Style
styleOverrides =
    Css.Global.descendants
        [ Css.Global.a
            [ textDecoration none
            , color Colors.azure
            , borderBottom3 (px 1) solid Colors.azure
            , visited [ color Colors.azure ]
            ]
        , -- This global selector and overrides are necessary due to
          -- old stylesheets used on the monolith that set the
          -- `.txt p { font-size: 18px; }` -- without these overrides,
          -- we may see giant ugly alerts.
          -- Remove these if you want to! but be emotionally prepped
          -- to deal with visual regressions. 🙏
          Css.Global.p
            [ margin zero
            , fontSize (px 13)
            , Fonts.baseFont
            ]
        ]



-- Icons


viewTinyIcon : Html msg -> Html msg
viewTinyIcon icon =
    styled div
        []
        []
        [ Nri.Ui.styled div
            "Nri-Ui-Message-V2--tinyIconContainer"
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
            [ icon ]
        ]


viewLargeIcon : Html msg -> Html msg
viewLargeIcon icon =
    styled div
        [ width (px 35)
        , marginRight (px 10)
        ]
        []
        [ icon
        ]


viewBannerIcon : Html msg -> Html msg
viewBannerIcon icon =
    styled div
        [ width (px 50)
        , height (px 50)
        , marginRight (px 20)
        , -- NOTE: I think it's normally best to avoid relying on flexShrink (and use flexGrow/flexBasis) instead,
          -- But using shrink here and on the next div lets us have the text content be centered rather than
          -- left-aligned when the content is shorter than one line
          flexShrink zero
        ]
        []
        [ icon ]



-- Dismiss buttons


tinyDismissButton : msg -> Html msg
tinyDismissButton msg =
    Nri.Ui.styled div
        "dismiss-button-container"
        []
        []
        [ ClickableSvg.button "Dismiss message"
            UiIcon.x
            [ ClickableSvg.onClick msg
            , ClickableSvg.width (px 16)
            , ClickableSvg.height (px 16)
            , ClickableSvg.css
                [ Css.verticalAlign Css.middle
                , Css.marginLeft (Css.px 5)
                ]
            ]
        ]


largeDismissButton : msg -> Html msg
largeDismissButton msg =
    Nri.Ui.styled div
        "dismiss-button-container"
        [ padding2 Css.zero (px 20)
        ]
        []
        [ ClickableSvg.button "Dismiss message"
            UiIcon.x
            [ ClickableSvg.onClick msg
            , ClickableSvg.width (px 16)
            , ClickableSvg.height (px 16)
            ]
        ]


bannerDismissButton : msg -> Html msg
bannerDismissButton msg =
    Nri.Ui.styled div
        "dismiss-button-container"
        [ padding2 (px 30) (px 40)
        ]
        []
        [ ClickableSvg.button "Dismiss banner"
            UiIcon.x
            [ ClickableSvg.onClick msg
            , ClickableSvg.width (px 16)
            , ClickableSvg.height (px 16)
            ]
        ]
