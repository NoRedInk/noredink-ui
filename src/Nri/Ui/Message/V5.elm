module Nri.Ui.Message.V5 exposing
    ( somethingWentWrong
    , view, Attribute
    , icon, custom, testId, id
    , hideIconForMobile, hideIconFor
    , css, notMobileCss, mobileCss, quizEngineMobileCss
    , tiny, large, banner
    , paragraph, plaintext, markdown, html, httpError, codeDetails
    , tip, error, alert, success, customTheme
    , alertRole, statusRole
    , onDismiss
    )

{-| Changes from V4:

    - changes signature of `hideIconFor` to accept an `Nri.Ui.MediaQuery` constructor instead of `Css.Media.MediaQuery`

Changes from V3:

  - adds `statusRole`
  - removes `alertDialogRole`
  - moves custom attributes and role to a div containing the icon and text
  - makes this div programmatically focusable

Patch changes:

  - adds `notMobileCss`, `mobileCss`, `quizEngineMobileCss`
  - adds `hideIconForMobile` and `hideIconFor`
  - use `Shadows`
  - use internal `Content` module
  - make the tiny Message's icon size smaller

Changes from V2:

    - adds helpers: `custom`,`css`,`icon`,`testId`,`id`


# View

@docs somethingWentWrong
@docs view, Attribute
@docs icon, custom, testId, id


# CSS

@docs hideIconForMobile, hideIconFor
@docs css, notMobileCss, mobileCss, quizEngineMobileCss


## Size

@docs tiny, large, banner


## Content

@docs paragraph, plaintext, markdown, html, httpError, codeDetails


## Theme

@docs tip, error, alert, success, customTheme


## Role

@docs alertRole, statusRole


## Actions

@docs onDismiss

-}

import Accessibility.Styled as Html exposing (..)
import Accessibility.Styled.Key exposing (tabbable)
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Style exposing (invisibleStyle)
import Content
import Css exposing (..)
import Css.Global
import Html.Styled.Attributes as Attributes
import Http
import MarkdownStyles
import Nri.Ui
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.MediaQuery.V2 as MediaQuery exposing (MediaQuery)
import Nri.Ui.Shadows.V1 as Shadows
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

        backgroundColor_ =
            getBackgroundColor attributes.size attributes.theme

        color_ =
            getColor attributes.size attributes.theme

        icon_ =
            getIcon attributes.icon attributes.size attributes.theme

        baseStyles =
            [ Fonts.baseFont
            , color color_
            , boxSizing borderBox
            , Css.batch attributes.customStyles
            , MediaQuery.toStyle attributes.customResponsiveStyles
            , Css.Global.descendants
                [ Css.Global.a
                    [ color Colors.azure
                    , borderBottom3 (px 1) solid Colors.azure
                    , textDecoration none
                    , visited [ color Colors.azure ]
                    ]
                ]
            ]
    in
    case attributes.size of
        Tiny ->
            div
                [ ExtraAttributes.nriDescription "Nri-Ui-Message--tiny"
                , Attributes.css
                    (baseStyles
                        ++ [ paddingTop (px 6)
                           , paddingBottom (px 8)
                           ]
                    )
                ]
                [ div
                    [ Attributes.css
                        [ displayFlex
                        , alignItems center
                        ]
                    ]
                    [ div
                        ([ Attributes.css
                            [ displayFlex
                            , alignItems center
                            , fontSize (px 13)
                            ]
                         , tabbable False
                         ]
                            ++ role
                            ++ attributes.customAttributes
                        )
                        [ Nri.Ui.styled div
                            "Nri-Ui-Message--icon"
                            []
                            [ Attributes.css
                                [ displayFlex
                                , alignItems center
                                ]
                            ]
                            [ icon_
                            ]
                        , div [] attributes.content
                        ]
                    , case attributes.onDismiss of
                        Nothing ->
                            text ""

                        Just msg ->
                            tinyDismissButton msg
                    ]
                , case attributes.codeDetails of
                    Just details ->
                        viewCodeDetails details

                    Nothing ->
                        text ""
                ]

        Large ->
            div
                [ ExtraAttributes.nriDescription "Nri-Ui-Message-large"
                , Attributes.css
                    (baseStyles
                        ++ [ -- Box
                             borderRadius (px 8)
                           , backgroundColor_
                           , Shadows.low
                           ]
                    )
                ]
                [ div
                    [ Attributes.css
                        [ fontSize (px 15)
                        , fontWeight (int 600)
                        , lineHeight (px 21)
                        , padding (px 20)
                        , MediaQuery.fromList
                            [ MediaQuery.mobile
                                [ padding (px 15)
                                ]
                            ]
                        ]
                    ]
                    [ div
                        [ Attributes.css
                            [ displayFlex
                            , alignItems center
                            ]
                        ]
                        [ div
                            ([ Attributes.css
                                [ displayFlex
                                , alignItems center
                                , property "width" "fit-content"
                                ]
                             , tabbable False
                             ]
                                ++ role
                                ++ attributes.customAttributes
                            )
                            [ icon_
                            , div
                                [ Attributes.css
                                    [ minWidth (px 100)
                                    , flexBasis (px 100)
                                    , flexGrow (int 1)
                                    ]
                                ]
                                attributes.content
                            ]
                        , div [ Attributes.css [ flexGrow (int 1) ] ] []
                        , case attributes.onDismiss of
                            Nothing ->
                                text ""

                            Just msg ->
                                largeDismissButton msg
                        ]
                    , case attributes.codeDetails of
                        Just details ->
                            viewCodeDetails details

                        Nothing ->
                            text ""
                    ]
                ]

        Banner ->
            div
                [ ExtraAttributes.nriDescription "Nri-Ui-Message-banner"
                , Attributes.css
                    (baseStyles
                        ++ [ backgroundColor_
                           ]
                    )
                ]
                [ div
                    [ Attributes.css
                        [ fontSize (px 20)
                        , fontWeight (int 700)
                        , lineHeight (num 1.4)
                        , padding (px 20)
                        , MediaQuery.fromList
                            [ MediaQuery.mobile
                                [ fontSize (px 15)
                                , fontWeight (int 600)
                                , padding (px 15)
                                ]
                            ]
                        ]
                    ]
                    [ div
                        [ Attributes.css
                            [ displayFlex
                            , alignItems center
                            ]
                        ]
                        [ div [ Attributes.css [ flexGrow (int 1) ] ] []
                        , div
                            ([ Attributes.css
                                [ displayFlex
                                , alignItems center
                                , property "width" "fit-content"
                                ]
                             , tabbable False
                             ]
                                ++ role
                                ++ attributes.customAttributes
                            )
                            [ icon_
                            , Nri.Ui.styled div
                                "banner-alert-notification"
                                [ fontSize (px 20)
                                , fontWeight (int 700)
                                , lineHeight (num 1.4)
                                , maxWidth (px 600)
                                , minWidth (px 100)
                                , flexShrink (int 1)
                                , Fonts.baseFont
                                , MediaQuery.fromList
                                    [ MediaQuery.mobile
                                        [ fontSize (px 15)
                                        , fontWeight (int 600)
                                        ]
                                    ]
                                ]
                                []
                                attributes.content
                            ]
                        , div [ Attributes.css [ flexGrow (int 1) ] ] []
                        , case attributes.onDismiss of
                            Nothing ->
                                text ""

                            Just msg ->
                                bannerDismissButton msg
                        ]
                    , case attributes.codeDetails of
                        Just details ->
                            viewCodeDetails details

                        Nothing ->
                            text ""
                    ]
                ]


{-| Shows an appropriate error message for when something unhandled happened.

    view maybeDetailedErrorMessage =
        viewMaybe Message.somethingWentWrong maybeDetailedErrorMessage

-}
somethingWentWrong : String -> Html msg
somethingWentWrong errorMessageForEngineers =
    view
        [ tiny
        , error
        , alertRole
        , plaintext somethingWentWrongMessage
        , codeDetails errorMessageForEngineers
        ]


somethingWentWrongMessage : String
somethingWentWrongMessage =
    "Sorry, something went wrong.  Please try again later."


viewCodeDetails : String -> Html msg
viewCodeDetails errorMessageForEngineers =
    details []
        [ summary
            [ Attributes.css
                [ Fonts.baseFont
                , fontSize (px 14)
                , color Colors.gray45
                ]
            ]
            [ text "Details for NoRedInk engineers" ]
        , code
            [ Attributes.css
                [ display block
                , whiteSpace preWrap
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


{-| Provide a plain-text string.
-}
plaintext : String -> Attribute msg
plaintext =
    Attribute << Content.plaintext


{-| Provide a plain-text string that will be put into a paragraph tag, with the default margin removed.
-}
paragraph : String -> Attribute msg
paragraph =
    Attribute << Content.paragraph


{-| Provide a string that will be rendered as markdown.
-}
markdown : String -> Attribute msg
markdown content =
    Attribute <|
        \config ->
            { config
                | content = Content.markdownContent content
                , customStyles = MarkdownStyles.anchorAndButton ++ config.customStyles ++ MediaQuery.toStyles config.customResponsiveStyles
            }


{-| Provide a list of custom HTML.
-}
html : List (Html msg) -> Attribute msg
html =
    Attribute << Content.html


{-| Provide an HTTP error, which will be translated to user-friendly text.
-}
httpError : Http.Error -> Attribute msg
httpError error_ =
    let
        ( codeDetails_, content ) =
            case error_ of
                Http.BadUrl url ->
                    ( Just ("Bad url: " ++ url)
                    , [ text somethingWentWrongMessage ]
                    )

                Http.Timeout ->
                    ( Nothing
                    , [ text "This request took too long to complete." ]
                    )

                Http.NetworkError ->
                    ( Nothing
                    , [ text "Something went wrong, and we think the problem is probably with your internet connection." ]
                    )

                Http.BadStatus 401 ->
                    ( Nothing
                    , [ text "You were logged out. Please log in again to continue working." ]
                    )

                Http.BadStatus 404 ->
                    ( Nothing
                    , [ text "We couldn’t find that!" ]
                    )

                Http.BadStatus status ->
                    ( Just ("Bad status: " ++ String.fromInt status)
                    , [ text somethingWentWrongMessage ]
                    )

                Http.BadBody body ->
                    ( Just body
                    , [ text somethingWentWrongMessage ]
                    )
    in
    Attribute <| \config -> { config | content = content, codeDetails = codeDetails_ }


{-| Details for Engineers

Will be rendered in a closed details box

-}
codeDetails : String -> Attribute msg
codeDetails code =
    Attribute <| \config -> { config | codeDetails = Just code }


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
customTheme : { color : Color, backgroundColor : Color } -> Attribute msg
customTheme custom_ =
    Attribute <| \config -> { config | theme = Custom custom_ }


{-| -}
icon : Svg -> Attribute msg
icon icon_ =
    Attribute <| \config -> { config | icon = Just icon_ }


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

-}
custom : List (Html.Attribute Never) -> Attribute msg
custom attributes =
    Attribute <|
        \config ->
            { config
                | customAttributes = List.append config.customAttributes attributes
            }


{-| -}
testId : String -> Attribute msg
testId id_ =
    custom [ ExtraAttributes.testId id_ ]


{-| -}
id : String -> Attribute msg
id id_ =
    custom [ Attributes.id id_ ]


{-| -}
hideIconForMobile : Attribute msg
hideIconForMobile =
    hideIconFor MediaQuery.mobile


{-| -}
hideIconFor : (List Style -> MediaQuery properties) -> Attribute msg
hideIconFor mq =
    responsiveCss
        (mq
            [ Css.Global.descendants
                [ ExtraAttributes.nriDescriptionSelector messageIconDescription
                    [ invisibleStyle
                    ]
                ]
            ]
        )


{-| -}
css : List Style -> Attribute msg
css styles =
    Attribute <|
        \config ->
            { config
                | customStyles = List.append config.customStyles styles
            }


{-| -}
responsiveCss : MediaQuery properties -> Attribute msg
responsiveCss mq =
    Attribute <|
        \config ->
            { config
                | customResponsiveStyles = MediaQuery.on mq config.customResponsiveStyles
            }


{-| Equivalent to:

    Message.css
        [ MediaQuery.fromList
            [ MediaQuery.not MediaQuery.mobile [ styles ]
            ]
        ]

-}
notMobileCss : List Style -> Attribute msg
notMobileCss =
    responsiveCss << MediaQuery.not MediaQuery.mobile


{-| Equivalent to:

    Message.css
        [ MediaQuery.fromList
            [ MediaQuery.mobile [ styles ]
            ]
        ]

-}
mobileCss : List Style -> Attribute msg
mobileCss =
    responsiveCss << MediaQuery.mobile


{-| Equivalent to:

    Message.css
        [ MediaQuery.fromList
            [ MediaQuery.quizEngineMobile [ styles ]
            ]
        ]

-}
quizEngineMobileCss : List Style -> Attribute msg
quizEngineMobileCss =
    responsiveCss << MediaQuery.quizEngineMobile


{-| Adds a dismiss ("X" icon) to a message which will produce the given `msg` when clicked.
-}
onDismiss : msg -> Attribute msg
onDismiss msg =
    Attribute <| \config -> { config | onDismiss = Just msg }


{-| Use this attribute when a user's immediate attention on the Message is required.

For example, use this attribute when:

>   - An invalid value was entered into a form field
>   - The user's login session is about to expire
>   - The connection to the server was lost, local changes will not be saved

-- Excerpted from [Using the alert role MDN docs](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_alert_role)

-}
alertRole : Attribute msg
alertRole =
    Attribute <| \config -> { config | role = Just AlertRole }


{-| Use this attribute when a user's immediate attention on the Message is NOT required.

This means that a `status` Message has a lower priority than an `alert` Message.

-}
statusRole : Attribute msg
statusRole =
    Attribute <| \config -> { config | role = Just StatusRole }



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
    , content : List (Html msg)
    , codeDetails : Maybe String
    , theme : Theme
    , size : Size
    , icon : Maybe Svg
    , customAttributes : List (Html.Attribute Never)
    , customStyles : List Style
    , customResponsiveStyles : MediaQuery.ResponsiveStyles
    }


{-| PRIVATE
-}
configFromAttributes : List (Attribute msg) -> BannerConfig msg
configFromAttributes attr =
    List.foldl (\(Attribute set) -> set)
        { onDismiss = Nothing
        , role = Nothing
        , content = []
        , codeDetails = Nothing
        , theme = Tip
        , size = Tiny
        , icon = Nothing
        , customAttributes = []
        , customStyles = []
        , customResponsiveStyles = MediaQuery.init
        }
        attr



-- Size


type Size
    = Tiny
    | Large
    | Banner



-- Themes


{-| `Error` / `Alert` / `Tip` / `Success`
-}
type Theme
    = Error
    | Alert
    | Tip
    | Success
    | Custom { color : Color, backgroundColor : Color }


getColor : Size -> Theme -> Color
getColor size theme =
    case theme of
        Custom { color } ->
            color

        Error ->
            case size of
                Tiny ->
                    Colors.purpleDark

                _ ->
                    Colors.purpleDark

        Alert ->
            case size of
                Tiny ->
                    Colors.redDark

                _ ->
                    Colors.redDark

        Tip ->
            Colors.navy

        Success ->
            Colors.greenDarkest


getBackgroundColor : Size -> Theme -> Style
getBackgroundColor size theme =
    case ( size, theme ) of
        ( Tiny, _ ) ->
            Css.batch []

        ( Large, Tip ) ->
            Css.backgroundColor Colors.sunshine

        ( Banner, Tip ) ->
            Css.backgroundColor Colors.sunshine

        ( _, Error ) ->
            Css.backgroundColor Colors.purpleLight

        ( _, Alert ) ->
            Css.backgroundColor Colors.sunshine

        ( _, Success ) ->
            Css.backgroundColor Colors.greenLightest

        ( _, Custom { backgroundColor } ) ->
            Css.backgroundColor backgroundColor


getIcon : Maybe Svg -> Size -> Theme -> Html msg
getIcon customIcon size theme =
    let
        ( iconSize, marginRight ) =
            case size of
                Tiny ->
                    ( px 18, Css.marginRight (Css.px 5) )

                Large ->
                    ( px 35, Css.marginRight (Css.px 10) )

                Banner ->
                    ( px 35, Css.marginRight (Css.px 10) )
    in
    case ( customIcon, theme ) of
        ( Nothing, Error ) ->
            UiIcon.exclamation
                |> NriSvg.withColor Colors.purple
                |> NriSvg.withWidth iconSize
                |> NriSvg.withHeight iconSize
                |> NriSvg.withCss [ marginRight, Css.flexShrink Css.zero ]
                |> NriSvg.withLabel "Error"
                |> NriSvg.toHtml

        ( Nothing, Alert ) ->
            let
                color =
                    case size of
                        Tiny ->
                            Colors.red

                        _ ->
                            Colors.red
            in
            UiIcon.exclamation
                |> NriSvg.withColor color
                |> NriSvg.withWidth iconSize
                |> NriSvg.withHeight iconSize
                |> NriSvg.withCss [ marginRight, Css.flexShrink Css.zero ]
                |> NriSvg.withLabel "Alert"
                |> NriSvg.toHtml

        ( Nothing, Tip ) ->
            case size of
                Tiny ->
                    div
                        [ Attributes.css
                            [ borderRadius (pct 50)
                            , height (px 18)
                            , width (px 18)
                            , Css.marginRight (Css.px 5)
                            , backgroundColor Colors.navy
                            , displayFlex
                            , Css.flexShrink Css.zero
                            , alignItems center
                            , justifyContent center
                            ]
                        , ExtraAttributes.nriDescription messageIconDescription
                        ]
                        [ UiIcon.baldBulb
                            |> NriSvg.withColor Colors.mustard
                            |> NriSvg.withWidth (Css.px 13)
                            |> NriSvg.withHeight (Css.px 13)
                            |> NriSvg.toHtml
                        ]

                Large ->
                    div
                        [ Attributes.css
                            [ borderRadius (pct 50)
                            , height (px 35)
                            , width (px 35)
                            , Css.marginRight (Css.px 10)
                            , backgroundColor Colors.navy
                            , displayFlex
                            , Css.flexShrink Css.zero
                            , alignItems center
                            , justifyContent center
                            ]
                        , ExtraAttributes.nriDescription messageIconDescription
                        ]
                        [ UiIcon.sparkleBulb
                            |> NriSvg.withColor Colors.mustard
                            |> NriSvg.withWidth (Css.px 22)
                            |> NriSvg.withHeight (Css.px 22)
                            |> NriSvg.toHtml
                        ]

                Banner ->
                    div
                        [ Attributes.css
                            [ borderRadius (pct 50)
                            , height (px 35)
                            , width (px 35)
                            , Css.marginRight (Css.px 10)
                            , backgroundColor Colors.navy
                            , displayFlex
                            , Css.flexShrink Css.zero
                            , alignItems center
                            , justifyContent center
                            , MediaQuery.fromList
                                [ MediaQuery.mobile
                                    [ height (px 35)
                                    , width (px 35)
                                    ]
                                ]
                            ]
                        , ExtraAttributes.nriDescription messageIconDescription
                        ]
                        [ UiIcon.sparkleBulb
                            |> NriSvg.withColor Colors.mustard
                            |> NriSvg.withWidth (Css.px 22)
                            |> NriSvg.withHeight (Css.px 22)
                            |> NriSvg.toHtml
                        ]

        ( Nothing, Success ) ->
            UiIcon.checkmarkInCircle
                |> NriSvg.withColor Colors.greenDark
                |> NriSvg.withWidth iconSize
                |> NriSvg.withHeight iconSize
                |> NriSvg.withCss [ marginRight, Css.flexShrink Css.zero ]
                |> NriSvg.withLabel "Success"
                |> NriSvg.toHtml

        ( Just icon_, _ ) ->
            icon_
                |> NriSvg.withWidth iconSize
                |> NriSvg.withHeight iconSize
                |> NriSvg.withCss [ marginRight, Css.flexShrink Css.zero ]
                |> NriSvg.toHtml

        ( Nothing, Custom _ ) ->
            Html.text ""


messageIconDescription : String
messageIconDescription =
    "Nri-Ui-Message-icon"



-- Role


type Role
    = AlertRole
    | StatusRole


getRoleAttribute : Maybe Role -> List (Html.Attribute Never)
getRoleAttribute role =
    case role of
        Just AlertRole ->
            [ Role.alert ]

        Just StatusRole ->
            [ Role.status ]

        Nothing ->
            []



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
            , ClickableSvg.exactWidth 16
            , ClickableSvg.exactHeight 16
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
        [ padding2 zero (px 20)
        , displayFlex
        , MediaQuery.fromList
            [ MediaQuery.mobile
                [ padding4 (px 10) zero (px 10) (px 15)
                ]
            ]
        ]
        []
        [ ClickableSvg.button "Dismiss message"
            UiIcon.x
            [ ClickableSvg.onClick msg
            , ClickableSvg.exactWidth 16
            , ClickableSvg.exactHeight 16
            ]
        ]


bannerDismissButton : msg -> Html msg
bannerDismissButton msg =
    Nri.Ui.styled div
        "dismiss-button-container"
        [ padding2 zero (px 20)
        , displayFlex
        , MediaQuery.fromList
            [ MediaQuery.mobile
                [ padding4 (px 10) zero (px 10) (px 15)
                ]
            ]
        ]
        []
        [ ClickableSvg.button "Dismiss banner"
            UiIcon.x
            [ ClickableSvg.onClick msg
            , ClickableSvg.exactWidth 16
            , ClickableSvg.exactHeight 16
            ]
        ]
