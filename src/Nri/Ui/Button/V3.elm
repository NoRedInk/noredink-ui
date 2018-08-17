module Nri.Ui.Button.V3
    exposing
        ( ButtonConfig
        , ButtonContent
        , ButtonSize(..)
        , ButtonState(..)
        , ButtonStyle(..)
        , LinkConfig
        , ToggleButtonConfig
        , button
        , copyToClipboard
        , customButton
        , delete
        , link
        , linkExternal
        , linkExternalWithTracking
        , linkSpa
        , linkWithMethod
        , linkWithTracking
        , toggleButton
        )

{-|


# Changes from V2:

  - Uses Html.Styled
  - Removes buttonDeprecated
  - Removes Tiny size
  - Removes one-off Active hack
  - Removes "submit" button - we just used that for forms that were partially in Elm


# About:

Common NoRedInk buttons. For accessibility purposes, buttons that perform an
action on the current page should be HTML `<button>` elements and are created here
with `*Button` functions. Buttons that take the user to a new page should be
HTML `<a>` elements and are created here with `*Link` functions. Both versions
should be able to use the same CSS class in all cases.

There will generally be a `*Button` and `*Link` version of each button style.
(These will be created as they are needed.)


## Common configs

@docs ButtonSize, ButtonStyle, ButtonState, ButtonContent


## `<button>` Buttons

@docs ButtonConfig, button, customButton, delete, copyToClipboard, ToggleButtonConfig, toggleButton


## `<a>` Buttons

@docs LinkConfig, link, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking

-}

-- import EventExtras

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Widget as Widget
import Css exposing (..)
import Css.Foreign
import EventExtras
import Html.Styled as Html
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events as Events exposing (onClick)
import Json.Decode
import Markdown.Block
import Markdown.Inline
import Nri.Ui
import Nri.Ui.AssetPath as AssetPath exposing (Asset)
import Nri.Ui.Colors.Extra exposing (withAlpha)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1
import Nri.Ui.Icon.V3 as Icon exposing (IconType, decorativeIcon, icon)


{-| Sizes for buttons and links that have button classes
-}
type ButtonSize
    = Small
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
customButton : List (Attribute msg) -> ButtonConfig msg -> ButtonContent -> Html msg
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
    Nri.Ui.styled Html.button
        "Nri-Button-V3-CustomButton"
        (buttonStyles config.size config.width buttonStyle False)
        ([ onClick config.onClick
         , Attributes.disabled disabled
         , Attributes.type_ "button"
         , widthStyle config.width
         ]
            ++ attributes
        )
        (viewLabel content.icon content.label)


widthStyle : Maybe Int -> Attribute msg
widthStyle width =
    width
        |> Maybe.map (\w -> [ ( "width", toString w ++ "px" ) ])
        |> Maybe.withDefault []
        |> style



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
    Nri.Ui.styled Html.button
        "Nri-Ui-Button-V3-copyToClipboard"
        (buttonStyles config.size config.width (styleToColorPalette config.style) False)
        [ Widget.label "Copy URL to clipboard"
        , attribute "data-clipboard-text" config.copyText
        , widthStyle config.width
        ]
        (viewLabel maybeIcon config.buttonLabel)



-- DELETE BUTTON


type alias DeleteButtonConfig msg =
    { label : String
    , onClick : msg
    }


{-| A delete button (blue X)
-}
delete : { r | x : String } -> DeleteButtonConfig msg -> Html msg
delete assets config =
    Nri.Ui.styled Html.button
        "Nri-Button-V3-delete"
        [ display inlineBlock
        , backgroundRepeat noRepeat
        , backgroundColor transparent
        , backgroundPosition center
        , backgroundSize contain
        , Css.property "border" "none"
        , Css.width (px 15)
        , Css.height (px 15)
        , padding zero
        , margin2 zero (px 6)
        , cursor pointer
        , color Colors.azure
        ]
        [ onClick config.onClick
        , type_ "button"
        , -- reference: https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_button_role#Labeling_buttons
          Widget.label config.label
        ]
        [ Icon.icon
            { alt = "Delete"
            , icon = Icon.xSvg assets
            }
        ]



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
    let
        toggledStyles =
            if config.pressed then
                [ color Colors.gray20
                , backgroundColor Colors.glacier
                , boxShadow5 inset zero (px 3) zero (withAlpha 0.2 Colors.gray20)
                , border3 (px 1) solid Colors.azure
                , fontWeight bold
                ]
            else
                []
    in
    Nri.Ui.styled Html.button
        "Nri-Ui-Button-V3-toggleButton"
        (buttonStyles Medium Nothing SecondaryColors False
            ++ toggledStyles
        )
        (if config.pressed then
            [ onClick config.onDeselect
            , Widget.pressed <| Just True

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
            , Role.button
            , type_ "button"
            ]
        )
        (viewLabel Nothing config.label)



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
    linkBase <| [ Attributes.target "_self" ]


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
    -> Html msg
linkSpa toUrl toMsg config =
    linkBase
        [ EventExtras.onClickPreventDefaultForLinkWithHref (toMsg config.route)
            |> Attributes.fromUnstyled
        ]
        { label = config.label
        , icon = config.icon
        , size = config.size
        , style = config.style
        , width = config.width
        , url = toUrl config.route
        }


{-| Wrap some text so it looks like a button, but actually is wrapped in an anchor to
some url and have it open to an external site
-}
linkExternal : LinkConfig -> Html msg
linkExternal =
    linkBase <| [ Attributes.target "_blank" ]


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
        [ Events.onWithOptions "click"
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
        [ Attributes.target "_blank"
        , onClick onTrack
        ]


{-| Helper function for building links with an arbitrary number of Attributes
-}
linkBase : List (Attribute msg) -> LinkConfig -> Html msg
linkBase extraAttrs config =
    Nri.Ui.styled Html.a
        "Nri-Button-V3-linkBase"
        (buttonStyles config.size config.width (styleToColorPalette config.style) True
            ++ [ whiteSpace noWrap
               ]
        )
        (Attributes.href config.url
            :: extraAttrs
        )
        (viewLabel config.icon config.label)



-- HELPERS


type ColorPalette
    = PrimaryColors
    | SecondaryColors
    | BorderlessColors
    | DangerColors
    | PremiumColors
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


buttonStyles : ButtonSize -> Maybe Int -> ColorPalette -> Bool -> List Style
buttonStyles size width colorPalette isLink =
    List.concat
        [ buttonStyle
        , colorStyle colorPalette
        , sizeStyle size width isLink
        ]


viewLabel : Maybe IconType -> String -> List (Html msg)
viewLabel icn label =
    case icn of
        Nothing ->
            renderMarkdown label

        Just iconType ->
            [ span
                []
                (decorativeIcon iconType
                    :: renderMarkdown label
                )
            ]


renderMarkdown : String -> List (Html msg)
renderMarkdown markdown =
    case Markdown.Block.parse Nothing markdown of
        -- It seems to be always first wrapped in a `Paragraph` and never directly a `PlainInline`
        [ Markdown.Block.Paragraph _ inlines ] ->
            List.map Markdown.Inline.toHtml inlines
                |> List.map Html.fromUnstyled

        _ ->
            [ Html.text markdown ]



-- STYLES


buttonStyle : List Style
buttonStyle =
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
    , boxShadow none
    , border zero
    , marginBottom zero
    , Css.hover
        [ textDecoration none
        ]
    , Css.disabled
        [ cursor notAllowed
        ]
    ]


colorStyle : ColorPalette -> List Style
colorStyle colorPalette =
    let
        ( config, additionalStyles ) =
            case colorPalette of
                PrimaryColors ->
                    ( { background = Colors.azure
                      , hover = Colors.azureDark
                      , text = Colors.white
                      , border = Nothing
                      , shadow = Colors.azureDark
                      }
                    , []
                    )

                SecondaryColors ->
                    ( { background = Colors.white
                      , hover = Colors.glacier
                      , text = Colors.azure
                      , border = Just <| Colors.azure
                      , shadow = Colors.azure
                      }
                    , []
                    )

                BorderlessColors ->
                    ( { background = rgba 0 0 0 0
                      , hover = rgba 0 0 0 0
                      , text = Colors.azure
                      , border = Nothing
                      , shadow = rgba 0 0 0 0
                      }
                    , [ Css.hover
                            [ textDecoration underline
                            , Css.disabled
                                [ textDecoration none
                                ]
                            ]
                      ]
                      -- TODO: Weird case with borderless
                      -- , [ Css.Foreign.descendants
                      --         [ Css.Foreign.img
                      --             [ Css.height (px (config.imageHeight * 1.6))
                      --             , marginRight (px (config.imageHeight * 1.6 / 6))
                      --             ]
                      --         , Css.Foreign.svg
                      --             [ Css.height (px (config.imageHeight * 1.6)) |> important
                      --             , Css.width (px (config.imageHeight * 1.6)) |> important
                      --             , marginRight (px (config.imageHeight * 1.6 / 6))
                      --             ]
                      --         , Css.Foreign.svg
                      --             [ Css.important <| Css.height (px (config.imageHeight * 1.6))
                      --             , Css.important <| Css.width auto
                      --             , maxWidth (px (config.imageHeight * 1.25))
                      --             , paddingRight (px (config.imageHeight * 1.6 / 6))
                      --             , position relative
                      --             , bottom (px 2)
                      --             ]
                      --         ]
                      --   ]
                    )

                DangerColors ->
                    ( { background = Colors.red
                      , hover = Colors.redDark
                      , text = Colors.white
                      , border = Nothing
                      , shadow = Colors.redDark
                      }
                    , []
                    )

                PremiumColors ->
                    ( { background = Colors.yellow
                      , hover = Colors.ochre
                      , text = Colors.navy
                      , border = Nothing
                      , shadow = Colors.ochre
                      }
                    , []
                    )

                InactiveColors ->
                    ( { background = Colors.gray92
                      , hover = Colors.gray92
                      , text = Colors.gray45
                      , border = Nothing
                      , shadow = Colors.gray92
                      }
                    , []
                    )

                LoadingColors ->
                    ( { background = Colors.glacier
                      , hover = Colors.glacier
                      , text = Colors.navy
                      , border = Nothing
                      , shadow = Colors.glacier
                      }
                    , []
                    )

                SuccessColors ->
                    ( { background = Colors.greenDark
                      , hover = Colors.greenDark
                      , text = Colors.white
                      , border = Nothing
                      , shadow = Colors.greenDark
                      }
                    , []
                    )

                ErrorColors ->
                    ( { background = Colors.purple
                      , hover = Colors.purple
                      , text = Colors.white
                      , border = Nothing
                      , shadow = Colors.purple
                      }
                    , []
                    )
    in
    [ batch additionalStyles
    , color config.text
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


sizeStyle : ButtonSize -> Maybe Int -> Bool -> List Style
sizeStyle size width isLink =
    let
        config =
            case size of
                Small ->
                    { fontSize = 15
                    , height = 36
                    , lineHeight = 15
                    , sidePadding = 16
                    , imageHeight = 15
                    , shadowHeight = 2
                    , minWidth = 75
                    }

                Medium ->
                    { fontSize = 17
                    , height = 45
                    , lineHeight = 19
                    , sidePadding = 16
                    , imageHeight = 15
                    , shadowHeight = 3
                    , minWidth = 100
                    }

                Large ->
                    { fontSize = 20
                    , height = 56
                    , lineHeight = 22
                    , sidePadding = 16
                    , imageHeight = 20
                    , shadowHeight = 4
                    , minWidth = 200
                    }

        widthAttributes =
            case width of
                Just pxWidth ->
                    batch
                        [ maxWidth (pct 100)
                        , Css.width (px <| toFloat pxWidth)
                        ]

                Nothing ->
                    batch
                        [ padding2 zero (px config.sidePadding)
                        , minWidth (px config.minWidth)
                        ]

        lineHeightPx =
            if isLink then
                config.height
            else
                config.lineHeight
    in
    [ fontSize (px config.fontSize)
    , borderRadius (px 8)
    , Css.height (px config.height)
    , lineHeight (px lineHeightPx)
    , boxSizing borderBox
    , borderWidth (px 1)
    , borderBottomWidth (px config.shadowHeight)
    , widthAttributes
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
    ]
