module Nri.Ui.Button.V2
    exposing
        ( ButtonConfig
        , ButtonContent
        , ButtonDeprecatedConfig
        , ButtonSize(..)
        , ButtonState(..)
        , ButtonStyle(..)
        , LinkConfig
        , ToggleButtonConfig
        , button
        , buttonDeprecated
        , copyToClipboard
        , customButton
        , delete
        , link
        , linkExternal
        , linkExternalWithTracking
        , linkSpa
        , linkWithMethod
        , linkWithTracking
        , styles
        , submit
        , toggleButton
        )

{-| Common NoRedInk buttons. For accessibility purposes, buttons that perform an
action on the current page should be HTML `<button>` elements and are created here
with `*Button` functions. Buttons that take the user to a new page should be
HTML `<a>` elements and are created here with `*Link` functions. Both versions
should be able to use the same CSS class in all cases.

There will generally be a `*Button` and `*Link` version of each button style.
(These will be created as they are needed.)


## Required styles

@docs styles


## Common configs

@docs ButtonSize, ButtonStyle, ButtonState


## `<button>` Buttons

@docs ButtonConfig, ButtonDeprecatedConfig, button, buttonDeprecated, customButton, delete, copyToClipboard, ToggleButtonConfig, toggleButton


## `<a>` Buttons

@docs LinkConfig, link, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking


## `<input>` Buttons

@docs submit

-}

import Accessibility exposing (..)
import Accessibility.Role as Role
import Accessibility.Widget as Widget
import Css exposing (..)
import Css.Foreign exposing (Snippet, adjacentSiblings, children, class, descendants, each, everything, media, selector, withClass)
import EventExtras
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Styled
import Json.Decode
import Nri.Ui.AssetPath as AssetPath exposing (Asset)
import Nri.Ui.Colors.Extra exposing (withAlpha)
import Nri.Ui.Colors.V1
import Nri.Ui.Fonts.V1
import Nri.Ui.Icon.V2 as Icon exposing (IconType, decorativeIcon, icon)
import Nri.Ui.Styles.V1 as Styles


{-| Sizes for buttons and links that have button classes

NOTE: if you add a size here, you need to add Css styles using the sizeStyle helper in the styles definition.

TODO: don't use TinyDeprecated. It is deprecated.

-}
type ButtonSize
    = TinyDeprecated
    | Small
    | Medium
    | Large


{-| Styleguide-approved styles for your buttons!

Note on borderless buttons:
A borderless button that performs an action on the current page
This button is intended to look like a link.
Only use a borderless button when the clickable text in question follows the same layout/margin/padding as a bordered button

-}
type ButtonStyle
    = Primary
    | Secondary
    | Borderless
    | Danger
    | Premium
    | Active -- NOTE: this is a one-off hack; do not use


{-| Describes the state of a button. Has consequences for appearance and disabled attribute.

  - Enabled: An enabled button. Takes the appearance of ButtonStyle
  - Unfulfilled: A button which appears with the InactiveColors palette but is not disabled.
  - Disabled: A button which appears with the InactiveColors palette and is disabled.
  - Error: A button which appears with the ErrorColors palette and is disabled.
  - Loading: A button which appears with the LoadingColors palette and is disabled
  - Success: A button which appears with the InactiveColors palette and is disabled

-}
type ButtonState
    = Enabled
    | Unfulfilled
    | Disabled
    | Error
    | Loading
    | Success


{-| Buttons can be a clickable thing with a given action
-}
type alias ButtonDeprecatedConfig msg =
    { label : String
    , icon : Maybe IconType
    , onClick : msg
    , size : ButtonSize
    , style : ButtonStyle
    , disabled : Bool
    , width : Maybe Int
    }


{-| The part of a button that remains constant through different button states
-}
type alias ButtonConfig msg =
    { onClick : msg
    , size : ButtonSize
    , style : ButtonStyle
    , width : Maybe Int
    }


{-| ButtonContent, often changes based on ButtonState. For example, a button in the "Success"
state may have a different label than a button in the "Error" state
-}
type alias ButtonContent =
    { label : String
    , state : ButtonState
    , icon : Maybe IconType
    }


{-| A delightful button which can trigger an effect when clicked!

This button will trigger the passed-in message if the button state is:

  - Enabled
  - Unfulfilled

This button will be Disabled if the button state is:

  - Disabled
  - Error
  - Loading
  - Success

-}
button : ButtonConfig msg -> ButtonContent -> Html msg
button config content =
    customButton [] config content


{-| Exactly the same as button but you can pass in a list of attributes
-}
customButton : List (Html.Attribute msg) -> ButtonConfig msg -> ButtonContent -> Html msg
customButton attributes config content =
    let
        buttonStyle =
            case content.state of
                Enabled ->
                    styleToColorPalette config.style

                Disabled ->
                    InactiveColors

                Error ->
                    ErrorColors

                Unfulfilled ->
                    InactiveColors

                Loading ->
                    LoadingColors

                Success ->
                    SuccessColors

        disabled =
            case content.state of
                Enabled ->
                    False

                Disabled ->
                    True

                Error ->
                    True

                Unfulfilled ->
                    False

                Loading ->
                    True

                Success ->
                    True
    in
    Html.button
        ([ buttonClass config.size config.width buttonStyle
         , onClick config.onClick
         , Html.Attributes.disabled disabled
         , Html.Attributes.type_ "button"
         , widthStyle config.width
         ]
            ++ attributes
        )
        [ viewLabel content.icon content.label ]


widthStyle : Maybe Int -> Html.Attribute msg
widthStyle width =
    width
        |> Maybe.map (\w -> [ ( "width", toString w ++ "px" ) ])
        |> Maybe.withDefault []
        |> style


{-| DEPRECATED: prefer the new Button Api, which allows for stateful buttons

A button that triggers some action when clicked, if not disabled

-}
buttonDeprecated : ButtonDeprecatedConfig msg -> Html msg
buttonDeprecated config =
    let
        widthStyle =
            config.width
                |> Maybe.map (\w -> [ ( "width", toString w ++ "px" ) ])
                |> Maybe.withDefault []
                |> style

        classStyle =
            if config.disabled then
                InactiveColors
            else
                styleToColorPalette config.style
    in
    Html.button
        [ buttonClass config.size config.width classStyle
        , onClick config.onClick
        , Html.Attributes.disabled config.disabled
        , widthStyle
        ]
        [ viewLabel config.icon config.label ]



-- COPY TO CLIPBOARD BUTTON


{-| Config for copyToClipboard
-}
type alias CopyToClipboardConfig =
    { size : ButtonSize
    , style : ButtonStyle
    , copyText : String
    , buttonLabel : String
    , withIcon : Bool
    , width : Maybe Int
    }


{-| See ui/src/Page/Teach/Courses/Assignments/index.coffee
You will need to hook this up to clipboard.js
-}
copyToClipboard : { r | teach_assignments_copyWhite_svg : Asset } -> CopyToClipboardConfig -> Html msg
copyToClipboard assets config =
    let
        maybeIcon =
            if config.withIcon then
                Just (Icon.copy assets)
            else
                Nothing
    in
    Html.button
        [ buttonClass config.size config.width (styleToColorPalette config.style)
        , styles.class [ CopyToClipboard ]
        , Widget.label "Copy URL to clipboard"
        , attribute "data-clipboard-text" config.copyText
        , widthStyle config.width
        ]
        [ viewLabel maybeIcon config.buttonLabel ]



-- DELETE BUTTON


type alias DeleteButtonConfig msg =
    { label : String
    , onClick : msg
    }


{-| A delete button (blue X)
-}
delete : DeleteButtonConfig msg -> Html msg
delete config =
    Html.button
        [ styles.class [ Delete ]
        , onClick config.onClick
        , type_ "button"
        , -- reference: https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_button_role#Labeling_buttons
          Widget.label config.label
        ]
        []



-- TOGGLE BUTTON


{-| Buttons can be toggled into a pressed state and back again.
-}
type alias ToggleButtonConfig msg =
    { label : String
    , onSelect : msg
    , onDeselect : msg
    , pressed : Bool
    }


{-| -}
toggleButton : ToggleButtonConfig msg -> Html msg
toggleButton config =
    Html.button
        (if config.pressed then
            [ onClick config.onDeselect
            , Widget.pressed <| Just True
            , styles.class [ Button, Toggled, SizeStyle Medium ]

            -- reference: https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_button_role#Labeling_buttons
            , Role.button

            -- Note: setting type: 'button' removes the default behavior of submit
            -- equivalent to preventDefaultBehavior = false
            -- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button#attr-name
            , type_ "button"
            ]
         else
            [ onClick config.onSelect
            , Widget.pressed <| Just False
            , buttonClass Medium Nothing SecondaryColors
            , Role.button
            , type_ "button"
            ]
        )
        [ viewLabel Nothing config.label ]



-- SUBMIT BUTTON


{-| Inputs can be a clickable thing used in a form
-}
type alias InputConfig =
    { content : Html Never
    , name : String
    , size : ButtonSize
    , style : ButtonStyle
    , value : String
    }


{-| A submit input that overrides the form's method to PUT
-}
submit : InputConfig -> Html c
submit config =
    Html.button
        [ buttonClass config.size Nothing (styleToColorPalette config.style)
        , name config.name
        , type_ "submit"
        , value config.value
        ]
        [ Html.map never config.content
        ]



-- LINKS THAT LOOK LIKE BUTTONS


{-| Links are clickable things with a url.

NOTE: Links do not support two-line labels.

-}
type alias LinkConfig =
    { label : String
    , icon : Maybe IconType
    , url : String
    , size : ButtonSize
    , style : ButtonStyle
    , width : Maybe Int
    }


{-| Wrap some text so it looks like a button, but actually is wrapped in an anchor to
some url
-}
link : LinkConfig -> Html msg
link =
    linkBase <| [ Html.Attributes.target "_self" ]


{-| Use this link for routing within a single page app.

This will make a normal <a> tag, but change the onClick behavior to avoid reloading the page.

See <https://github.com/elm-lang/html/issues/110> for details on this implementation.

-}
linkSpa :
    (route -> String)
    -> (route -> msg)
    ->
        { label : String
        , icon : Maybe IconType
        , size : ButtonSize
        , style : ButtonStyle
        , width : Maybe Int
        , route : route
        }
    -> Html.Styled.Html msg
linkSpa toUrl toMsg config =
    linkBase
        [ EventExtras.onClickPreventDefaultForLinkWithHref (toMsg config.route)
        ]
        { label = config.label
        , icon = config.icon
        , size = config.size
        , style = config.style
        , width = config.width
        , url = toUrl config.route
        }
        |> Html.Styled.fromUnstyled


{-| Wrap some text so it looks like a button, but actually is wrapped in an anchor to
some url and have it open to an external site
-}
linkExternal : LinkConfig -> Html msg
linkExternal =
    linkBase <| [ Html.Attributes.target "_blank" ]


{-| Wrap some text so it looks like a button, but actually is wrapped in an anchor to
some url, and it's an HTTP request (Rails includes JS to make this use the given HTTP method)
-}
linkWithMethod : String -> LinkConfig -> Html msg
linkWithMethod method =
    linkBase <| [ attribute "data-method" method ]


{-| Wrap some text so it looks like a button, but actually is wrapped in an anchor to some url.
This should only take in messages that result in a Msg that triggers Analytics.trackAndRedirect. For buttons that trigger other effects on the page, please use Nri.Button.button instead
-}
linkWithTracking : msg -> LinkConfig -> Html msg
linkWithTracking onTrack =
    linkBase <|
        [ Html.Events.onWithOptions "click"
            { stopPropagation = False
            , preventDefault = True
            }
            (Json.Decode.succeed onTrack)
        ]


{-| Wrap some text so it looks like a button, but actually is wrapped in an anchor to some url and have it open to an external site

This should only take in messages that result in tracking events. For buttons that trigger other effects on the page, please use Nri.Ui.Button.V2.button instead

-}
linkExternalWithTracking : msg -> LinkConfig -> Html msg
linkExternalWithTracking onTrack =
    linkBase <|
        [ Html.Attributes.target "_blank"
        , onClick onTrack
        ]


{-| Helper function for building links with an arbitrary number of Attributes
-}
linkBase : List (Html.Attribute msg) -> LinkConfig -> Html msg
linkBase extraAttrs config =
    let
        widthAttributes =
            Maybe.map
                (\width ->
                    [ style
                        [ ( "width", toString width ++ "px" )
                        , ( "max-width", "100%" )
                        ]
                    ]
                )
                config.width
                |> Maybe.withDefault []
    in
    Html.a
        (buttonClass config.size config.width (styleToColorPalette config.style)
            :: styles.class [ IsLink ]
            :: Html.Attributes.href config.url
            :: widthAttributes
            ++ extraAttrs
        )
        [ viewLabel config.icon config.label ]



-- HELPERS


type ColorPalette
    = PrimaryColors
    | SecondaryColors
    | BorderlessColors
    | DangerColors
    | PremiumColors
    | ActiveColors -- TODO: merge with Toggled
    | InactiveColors
    | LoadingColors
    | SuccessColors
    | ErrorColors


styleToColorPalette : ButtonStyle -> ColorPalette
styleToColorPalette style =
    case style of
        Primary ->
            PrimaryColors

        Secondary ->
            SecondaryColors

        Borderless ->
            BorderlessColors

        Danger ->
            DangerColors

        Premium ->
            PremiumColors

        Active ->
            ActiveColors


buttonClass : ButtonSize -> Maybe Int -> ColorPalette -> Html.Attribute msg
buttonClass size width colorPalette =
    styles.class <|
        List.concat
            [ [ Button
              , SizeStyle size
              , ColorsStyle colorPalette
              ]
            , case width of
                Nothing ->
                    []

                Just _ ->
                    [ ExplicitWidth ]
            ]


viewLabel : Maybe IconType -> String -> Html msg
viewLabel icn label =
    case icn of
        Nothing ->
            Html.text label

        Just iconType ->
            span [ styles.class [ LabelIconAndText ] ]
                [ decorativeIcon iconType
                , Html.text label
                ]



-- STYLES


type CssClasses
    = Button
    | IsLink
    | ExplicitWidth
    | SizeStyle ButtonSize
    | ColorsStyle ColorPalette
    | LabelIconAndText
    | Toggled
    | Delete
    | CopyToClipboard
    | NewStyle


{-| TODO: move this to elm-css?
Cross-browser support for linear gradient backgrounds.

Falls back to the top color if gradients are not supported.

-}
linearGradient : ( Css.ColorValue compatible1, Css.ColorValue compatible2 ) -> Css.Style
linearGradient ( top, bottom ) =
    Css.batch
        [ Css.property "background" top.value -- Old browsers
        , Css.property "background" ("-moz-linear-gradient(top," ++ top.value ++ " 0%," ++ bottom.value ++ " 100%)") -- FF3.6+
        , Css.property "background" ("-webkit-gradient(linear,left top,left bottom,color-stop(0%," ++ top.value ++ "),color-stop(100%," ++ bottom.value ++ "))") -- Chrome, Safari 4+
        , Css.property "background" ("-webkit-linear-gradient(top," ++ top.value ++ " 0%," ++ bottom.value ++ " 100%)") -- Chrome 10+, Safari 5.1+
        , Css.property "background" ("-o-linear-gradient(top," ++ top.value ++ " 0%," ++ bottom.value ++ " 100%)") -- Opera 11.10+
        , Css.property "background" ("-ms-linear-gradient(top," ++ top.value ++ " 0%," ++ bottom.value ++ " 100%)") -- IE10+
        , Css.property "background" ("linear,to bottom," ++ top.value ++ " 0%," ++ bottom.value ++ " 100%") -- W3C
        ]


{-| Required CSS styles for `Nri.Ui.Button.V2`.
-}
styles : Styles.StylesWithAssets Never CssClasses msg { r | icons_xBlue_svg : Asset }
styles =
    let
        sizeStyle size config =
            Css.Foreign.class (SizeStyle size)
                [ fontSize (px config.fontSize)
                , borderRadius (px 8)
                , Css.height (px config.height)
                , lineHeight (px config.lineHeight)
                , padding2 zero (px config.sidePadding)
                , boxSizing borderBox
                , minWidth (px config.minWidth)
                , borderWidth (px 1)
                , borderBottomWidth (px config.shadowHeight)
                , Css.Foreign.withClass ExplicitWidth
                    [ padding2 zero (px 4)
                    , boxSizing borderBox
                    ]
                , Css.Foreign.withClass IsLink
                    [ lineHeight (px config.height)
                    ]
                , Css.Foreign.descendants
                    [ Css.Foreign.img
                        [ Css.height (px config.imageHeight)
                        , marginRight (px <| config.imageHeight / 6)
                        , position relative
                        , bottom (px 2)
                        , verticalAlign middle
                        ]
                    , Css.Foreign.svg
                        [ Css.height (px config.imageHeight) |> important
                        , Css.width (px config.imageHeight) |> important
                        , marginRight (px <| config.imageHeight / 6)
                        , position relative
                        , bottom (px 2)
                        , verticalAlign middle
                        ]
                    , Css.Foreign.svg
                        [ Css.important <| Css.height (px config.imageHeight)
                        , Css.important <| Css.width auto
                        , maxWidth (px (config.imageHeight * 1.25))
                        , paddingRight (px <| config.imageHeight / 6)
                        , position relative
                        , bottom (px 2)
                        , verticalAlign middle
                        ]
                    ]

                -- Borderless buttons get bigger icons
                , Css.Foreign.withClass (ColorsStyle BorderlessColors)
                    [ Css.Foreign.descendants
                        [ Css.Foreign.img
                            [ Css.height (px (config.imageHeight * 1.6))
                            , marginRight (px (config.imageHeight * 1.6 / 6))
                            ]
                        , Css.Foreign.svg
                            [ Css.height (px (config.imageHeight * 1.6)) |> important
                            , Css.width (px (config.imageHeight * 1.6)) |> important
                            , marginRight (px (config.imageHeight * 1.6 / 6))
                            ]
                        , Css.Foreign.svg
                            [ Css.important <| Css.height (px (config.imageHeight * 1.6))
                            , Css.important <| Css.width auto
                            , maxWidth (px (config.imageHeight * 1.25))
                            , paddingRight (px (config.imageHeight * 1.6 / 6))
                            , position relative
                            , bottom (px 2)
                            ]
                        ]
                    ]
                ]

        styleStyle style config =
            Css.Foreign.class (ColorsStyle style)
                [ color config.text
                , backgroundColor config.background
                , fontWeight (int 700)
                , textAlign center
                , case config.border of
                    Nothing ->
                        borderStyle none

                    Just color ->
                        Css.batch
                            [ borderColor color
                            , borderStyle solid
                            ]
                , borderBottomStyle solid
                , borderBottomColor config.shadow
                , fontStyle normal
                , Css.hover
                    [ color config.text
                    , backgroundColor config.hover
                    , Css.disabled
                        [ backgroundColor config.background
                        ]
                    ]
                , Css.visited
                    [ color config.text
                    ]
                ]
    in
    Styles.stylesWithAssets "Nri-Ui-Button-V2-" <|
        \assets ->
            [ Css.Foreign.class Button
                [ cursor pointer
                , display inlineBlock
                , -- Specifying the font can and should go away after bootstrap is removed from application.css
                  Nri.Ui.Fonts.V1.baseFont
                , textOverflow ellipsis
                , overflow Css.hidden
                , textDecoration none
                , Css.property "background-image" "none"
                , textShadow none
                , Css.property "transition" "all 0.2s"
                , Css.hover
                    [ textDecoration none
                    ]
                , Css.disabled
                    [ cursor notAllowed
                    ]
                , Css.Foreign.withClass IsLink
                    [ whiteSpace noWrap
                    ]
                ]
            , sizeStyle TinyDeprecated
                { fontSize = 13
                , height = 25
                , lineHeight = 25
                , sidePadding = 8
                , minWidth = 50
                , imageHeight = 0
                , shadowHeight = 0
                }
            , sizeStyle Small
                { fontSize = 15
                , height = 36
                , lineHeight = 15
                , sidePadding = 16
                , imageHeight = 15
                , shadowHeight = 2
                , minWidth = 75
                }
            , sizeStyle Medium
                { fontSize = 17
                , height = 45
                , lineHeight = 19
                , sidePadding = 16
                , imageHeight = 15
                , shadowHeight = 3
                , minWidth = 100
                }
            , sizeStyle Large
                { fontSize = 20
                , height = 56
                , lineHeight = 22
                , sidePadding = 16
                , imageHeight = 20
                , shadowHeight = 4
                , minWidth = 200
                }
            , styleStyle PrimaryColors
                { background = Nri.Ui.Colors.V1.azure
                , hover = Nri.Ui.Colors.V1.azureDark
                , text = Nri.Ui.Colors.V1.white
                , border = Nothing
                , shadow = Nri.Ui.Colors.V1.azureDark
                }
            , styleStyle SecondaryColors
                { background = Nri.Ui.Colors.V1.white
                , hover = Nri.Ui.Colors.V1.glacier
                , text = Nri.Ui.Colors.V1.azure
                , border = Just <| Nri.Ui.Colors.V1.azure
                , shadow = Nri.Ui.Colors.V1.azure
                }
            , styleStyle BorderlessColors
                { background = transparent
                , hover = transparent
                , text = Nri.Ui.Colors.V1.azure
                , border = Nothing
                , shadow = transparent
                }
            , Css.Foreign.class (ColorsStyle BorderlessColors)
                [ Css.hover
                    [ textDecoration underline
                    , Css.disabled
                        [ textDecoration none
                        ]
                    ]
                ]
            , styleStyle DangerColors
                { background = Nri.Ui.Colors.V1.red
                , hover = Nri.Ui.Colors.V1.redDark
                , text = Nri.Ui.Colors.V1.white
                , border = Nothing
                , shadow = Nri.Ui.Colors.V1.redDark
                }
            , styleStyle PremiumColors
                { background = Nri.Ui.Colors.V1.yellow
                , hover = Nri.Ui.Colors.V1.ochre
                , text = Nri.Ui.Colors.V1.navy
                , border = Nothing
                , shadow = Nri.Ui.Colors.V1.ochre
                }
            , styleStyle LoadingColors
                { background = Nri.Ui.Colors.V1.glacier
                , hover = Nri.Ui.Colors.V1.glacier
                , text = Nri.Ui.Colors.V1.navy
                , border = Nothing
                , shadow = Nri.Ui.Colors.V1.glacier
                }
            , styleStyle ErrorColors
                { background = Nri.Ui.Colors.V1.purple
                , hover = Nri.Ui.Colors.V1.purple
                , text = Nri.Ui.Colors.V1.white
                , border = Nothing
                , shadow = Nri.Ui.Colors.V1.purple
                }
            , styleStyle InactiveColors
                { background = Nri.Ui.Colors.V1.gray92
                , hover = Nri.Ui.Colors.V1.gray92
                , text = Nri.Ui.Colors.V1.gray45
                , border = Nothing
                , shadow = Nri.Ui.Colors.V1.gray92
                }
            , styleStyle ActiveColors
                { background = Nri.Ui.Colors.V1.glacier
                , hover = Nri.Ui.Colors.V1.glacier
                , text = Nri.Ui.Colors.V1.navy
                , border = Nothing
                , shadow = Nri.Ui.Colors.V1.gray92
                }
            , Css.Foreign.class (ColorsStyle ActiveColors)
                [ Css.boxShadow none
                , Css.boxShadow5 inset zero (px 3) zero (withAlpha 0.2 Nri.Ui.Colors.V1.gray20)
                , border3 (px 1) solid Nri.Ui.Colors.V1.azure
                ]
            , styleStyle SuccessColors
                { background = Nri.Ui.Colors.V1.greenDark
                , hover = Nri.Ui.Colors.V1.greenDark
                , text = Nri.Ui.Colors.V1.white
                , border = Nothing
                , shadow = Nri.Ui.Colors.V1.greenDark
                }
            , Css.Foreign.class (ColorsStyle PremiumColors)
                [ Css.property "box-shadow" "none"
                , marginBottom zero
                ]
            , Css.Foreign.class (ColorsStyle InactiveColors)
                [ Css.property "box-shadow" "none"
                , Css.property "border" "none"
                , marginBottom zero
                ]
            , Css.Foreign.class (ColorsStyle SuccessColors)
                [ Css.property "box-shadow" "none"
                , Css.property "border" "none"
                , marginBottom zero
                ]
            , Css.Foreign.class (ColorsStyle ErrorColors)
                [ Css.property "box-shadow" "none"
                , Css.property "border" "none"
                , marginBottom zero
                ]
            , Css.Foreign.class (ColorsStyle LoadingColors)
                [ Css.property "box-shadow" "none"
                , Css.property "border" "none"
                , marginBottom zero
                ]
            , Css.Foreign.class Delete
                [ display inlineBlock
                , backgroundImage (url <| AssetPath.url assets.icons_xBlue_svg)
                , backgroundRepeat noRepeat
                , backgroundColor transparent
                , backgroundPosition center
                , backgroundSize contain
                , Css.property "border" "none"
                , Css.width (px 15)
                , Css.height (px 15)
                , margin2 zero (px 6)
                , cursor pointer
                ]
            , Css.Foreign.class Toggled
                [ color Nri.Ui.Colors.V1.gray20
                , backgroundColor Nri.Ui.Colors.V1.glacier
                , boxShadow5 inset zero (px 3) zero (withAlpha 0.2 Nri.Ui.Colors.V1.gray20)
                , border3 (px 1) solid Nri.Ui.Colors.V1.azure
                , fontWeight bold
                ]
            ]
