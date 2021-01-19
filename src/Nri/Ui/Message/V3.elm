module Nri.Ui.Message.V3 exposing
    ( somethingWentWrong
    , view, Attribute
    , custom, css, testId, id
    , tiny, large, banner
    , plaintext, markdown, html
    , tip, error, alert, success, customTheme
    , alertRole, alertDialogRole
    , onDismiss
    )

{-| Changes from V2:

    - adds `custom`
    - adds `testId`, and `id` helpers
    - adds `css` helper


# View

@docs somethingWentWrong
@docs view, Attribute
@docs custom, css, testId, id


## Size

@docs tiny, large, banner


## Content

@docs plaintext, markdown, html


## Theme

@docs tip, error, alert, success, customTheme


## Role

@docs alertRole, alertDialogRole


## Actions

@docs onDismiss

-}

import Accessibility.Styled as Html exposing (..)
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Widget as Widget
import Css exposing (..)
import Css.Global
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (onClick)
import Markdown
import Nri.Ui
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
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

        backgroundColor_ =
            getBackgroundColor attributes.size attributes.theme

        color_ =
            getColor attributes.size attributes.theme

        icon =
            getIcon attributes.size attributes.theme

        ( description, containerStyles ) =
            case attributes.size of
                Tiny ->
                    ( "Nri-Ui-Message--tiny"
                    , [ displayFlex
                      , justifyContent start
                      , alignItems center
                      , paddingTop (px 6)
                      , paddingBottom (px 8)
                      , fontSize (px 13)
                      ]
                    )

                Large ->
                    ( "Nri-Ui-Message-large"
                    , [ displayFlex
                      , alignItems center

                      -- Box
                      , borderRadius (px 8)
                      , padding (px 20)
                      , backgroundColor_

                      -- Fonts
                      , fontSize (px 15)
                      , fontWeight (int 600)
                      , lineHeight (px 21)
                      ]
                    )

                Banner ->
                    ( "Nri-Ui-Message-banner"
                    , [ displayFlex
                      , justifyContent center
                      , alignItems center
                      , backgroundColor_

                      -- Fonts
                      , fontSize (px 20)
                      , fontWeight (int 700)
                      , lineHeight (px 27)
                      ]
                    )
    in
    div
        ([ ExtraAttributes.nriDescription description
         , Attributes.css
            [ Fonts.baseFont
            , color color_
            , boxSizing borderBox
            , styleOverrides
            , Css.batch containerStyles
            , Css.batch attributes.customStyles
            ]
         ]
            ++ role
            ++ attributes.customAttributes
        )
        (case attributes.size of
            Tiny ->
                [ Nri.Ui.styled div "Nri-Ui-Message--icon" [ alignSelf flexStart ] [] [ icon ]
                , div [] html_
                , case attributes.onDismiss of
                    Nothing ->
                        text ""

                    Just msg ->
                        tinyDismissButton msg
                ]

            Large ->
                [ icon
                , div
                    [ Attributes.css
                        [ minWidth (px 100)
                        , flexBasis (px 100)
                        , flexGrow (int 1)
                        ]
                    ]
                    html_
                , case attributes.onDismiss of
                    Nothing ->
                        text ""

                    Just msg ->
                        largeDismissButton msg
                ]

            Banner ->
                [ span
                    [ Attributes.css
                        [ alignItems center
                        , displayFlex
                        , justifyContent center
                        , padding (px 20)
                        , width (Css.pct 100)
                        ]
                    ]
                    [ icon
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
                        html_
                    ]
                , case attributes.onDismiss of
                    Nothing ->
                        text ""

                    Just msg ->
                        bannerDismissButton msg
                ]
        )


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
            , alertRole
            , plaintext "Sorry, something went wrong.  Please try again later."
            ]
        , details []
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
css : List Style -> Attribute msg
css styles =
    Attribute <|
        \config ->
            { config
                | customStyles = List.append config.customStyles styles
            }


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


{-| Use this attribute when (1) a user's immediate attention on the Message is required,
(2) the Message contains interactible elements, and (3) you've correctly set up the Message to be
modal (i.e., you've set up tab-wrapping, the body's overflow is hidden, the user
can't interact with elements apart from the Message's contents...)

When you use this role, verify that you are using it correctly using [this
MDN article](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_alertdialog_role).

-}
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
    , customAttributes : List (Html.Attribute Never)
    , customStyles : List Style
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
        , customAttributes = []
        , customStyles = []
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

        ( _, Custom { backgroundColor } ) ->
            Css.backgroundColor backgroundColor


getIcon : Size -> Theme -> Html msg
getIcon size theme =
    let
        ( iconSize, marginRight ) =
            case size of
                Tiny ->
                    ( px 20, Css.marginRight (Css.px 5) )

                Large ->
                    ( px 35, Css.marginRight (Css.px 10) )

                Banner ->
                    ( px 50, Css.marginRight (Css.px 20) )
    in
    case theme of
        Error ->
            UiIcon.exclamation
                |> NriSvg.withColor Colors.purple
                |> NriSvg.withWidth iconSize
                |> NriSvg.withHeight iconSize
                |> NriSvg.withCss [ marginRight, Css.flexShrink Css.zero ]
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
                |> NriSvg.withWidth iconSize
                |> NriSvg.withHeight iconSize
                |> NriSvg.withCss [ marginRight, Css.flexShrink Css.zero ]
                |> NriSvg.withLabel "Alert"
                |> NriSvg.toHtml

        Tip ->
            case size of
                Tiny ->
                    UiIcon.bulb
                        |> NriSvg.withColor Colors.yellow
                        |> NriSvg.withWidth iconSize
                        |> NriSvg.withHeight iconSize
                        |> NriSvg.withCss [ marginRight, Css.flexShrink Css.zero ]
                        |> NriSvg.withLabel "Tip"
                        |> NriSvg.toHtml

                Large ->
                    UiIcon.bulb
                        |> NriSvg.withColor Colors.navy
                        |> NriSvg.withWidth iconSize
                        |> NriSvg.withHeight iconSize
                        |> NriSvg.withCss [ marginRight, Css.flexShrink Css.zero ]
                        |> NriSvg.withLabel "Tip"
                        |> NriSvg.toHtml

                Banner ->
                    div
                        [ Attributes.css
                            [ borderRadius (pct 50)
                            , height (px 50)
                            , width (px 50)
                            , Css.marginRight (Css.px 20)
                            , backgroundColor Colors.navy
                            , displayFlex
                            , Css.flexShrink Css.zero
                            , alignItems center
                            , justifyContent center
                            ]
                        ]
                        [ UiIcon.bulb
                            |> NriSvg.withColor Colors.mustard
                            |> NriSvg.withWidth (Css.px 32)
                            |> NriSvg.withHeight (Css.px 32)
                            |> NriSvg.toHtml
                        ]

        Success ->
            UiIcon.checkmarkInCircle
                |> NriSvg.withColor Colors.green
                |> NriSvg.withWidth iconSize
                |> NriSvg.withHeight iconSize
                |> NriSvg.withCss [ marginRight, Css.flexShrink Css.zero ]
                |> NriSvg.withLabel "Success"
                |> NriSvg.toHtml

        Custom { icon } ->
            icon
                |> NriSvg.withWidth iconSize
                |> NriSvg.withHeight iconSize
                |> NriSvg.withCss [ marginRight, Css.flexShrink Css.zero ]
                |> NriSvg.toHtml



-- Role


type Role
    = AlertRole
    | AlertDialog


getRoleAttribute : Maybe Role -> List (Html.Attribute Never)
getRoleAttribute role =
    case role of
        Just AlertRole ->
            [ Role.alert ]

        Just AlertDialog ->
            [ Role.alertDialog ]

        Nothing ->
            []



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
        [ padding2 Css.zero (px 20)
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
        [ padding2 (px 30) (px 40) ]
        []
        [ ClickableSvg.button "Dismiss banner"
            UiIcon.x
            [ ClickableSvg.onClick msg
            , ClickableSvg.exactWidth 16
            , ClickableSvg.exactHeight 16
            ]
        ]
