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
        , submit
        , toggleButton
        )

{-|


# Changes from V2:

  - Uses Html.Styled
  - Removes buttonDeprecated
  - Removes Tiny size
  - Removes one-off Active hack


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


## `<input>` Buttons

@docs submit

-}

-- import EventExtras

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Widget as Widget
import Css exposing (..)
import Css.Foreign
import Html.Styled as Html
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events as Events exposing (onClick)
import Json.Decode
import Markdown.Block
import Markdown.Inline
import Nri.Ui
import Nri.Ui.AssetPath as AssetPath exposing (Asset)
import Nri.Ui.Colors.V1
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
        "nri-button-v3"
        (buttonStyles config.size config.width buttonStyle)
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
        (buttonStyles config.size config.width (styleToColorPalette config.style))
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
delete : DeleteButtonConfig msg -> Html msg
delete config =
    Html.button
        [ --styles.class [ Delete ] -- TODO
          onClick config.onClick
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
    Nri.Ui.styled Html.button
        "Nri-Ui-Button-V3-toggleButton"
        (buttonStyles Medium Nothing SecondaryColors)
        (if config.pressed then
            [ onClick config.onDeselect
            , Widget.pressed <| Just True

            -- , styles.class [ Button, Toggled, SizeStyle Medium ] -- TODO
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


{-| A submit input that overrides the form's method to PUT

TODO: Should this exist?

-}
submit : InputConfig -> Html c
submit config =
    Nri.Ui.styled Html.button
        "Nri-Ui-Button-V3-submit"
        (buttonStyles config.size Nothing (styleToColorPalette config.style))
        [ name config.name
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
        [-- EventExtras.onClickPreventDefaultForLinkWithHref (toMsg config.route) TODO
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
    let
        widthAttributes =
            Maybe.map
                (\widthAttr ->
                    [ Css.width (px <| toFloat widthAttr)
                    , maxWidth (pct 100)
                    ]
                )
                config.width
                |> Maybe.withDefault []
    in
    Nri.Ui.styled Html.a
        "Nri-Button-V3-linkBase"
        (buttonStyles config.size config.width (styleToColorPalette config.style)
            ++ widthAttributes
        )
        -- :: styles.class [ IsLink ] -- TODO
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


buttonStyles : ButtonSize -> Maybe Int -> ColorPalette -> List Style
buttonStyles size width colorPalette =
    List.concat
        [ colorStyle colorPalette
        ]


viewLabel : Maybe IconType -> String -> List (Html msg)
viewLabel icn label =
    case icn of
        Nothing ->
            renderMarkdown label

        Just iconType ->
            [ span
                [-- styles.class [ LabelIconAndText ]  TODO
                ]
                (decorativeIcon iconType
                    :: renderMarkdown label
                )
            ]


{-| TODO: does this have a styled version?
-}
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
    , Css.hover
        [ textDecoration none
        ]
    , Css.disabled
        [ cursor notAllowed
        ]

    -- , Css.Foreign.withClass IsLink -- TODO
    --     [ whiteSpace noWrap
    --     ]
    ]


colorStyle : ColorPalette -> List Style
colorStyle colorPalette =
    let
        config =
            case colorPalette of
                PrimaryColors ->
                    { background = Nri.Ui.Colors.V1.azure
                    , hover = Nri.Ui.Colors.V1.azureDark
                    , text = Nri.Ui.Colors.V1.white
                    , border = Nothing
                    , shadow = Nri.Ui.Colors.V1.azureDark
                    }

                SecondaryColors ->
                    { background = Nri.Ui.Colors.V1.white
                    , hover = Nri.Ui.Colors.V1.glacier
                    , text = Nri.Ui.Colors.V1.azure
                    , border = Just <| Nri.Ui.Colors.V1.azure
                    , shadow = Nri.Ui.Colors.V1.azure
                    }

                BorderlessColors ->
                    { background = rgba 0 0 0 0
                    , hover = rgba 0 0 0 0
                    , text = Nri.Ui.Colors.V1.azure
                    , border = Nothing
                    , shadow = rgba 0 0 0 0
                    }

                DangerColors ->
                    { background = Nri.Ui.Colors.V1.red
                    , hover = Nri.Ui.Colors.V1.redDark
                    , text = Nri.Ui.Colors.V1.white
                    , border = Nothing
                    , shadow = Nri.Ui.Colors.V1.redDark
                    }

                PremiumColors ->
                    { background = Nri.Ui.Colors.V1.yellow
                    , hover = Nri.Ui.Colors.V1.ochre
                    , text = Nri.Ui.Colors.V1.navy
                    , border = Nothing
                    , shadow = Nri.Ui.Colors.V1.ochre
                    }

                InactiveColors ->
                    { background = Nri.Ui.Colors.V1.gray92
                    , hover = Nri.Ui.Colors.V1.gray92
                    , text = Nri.Ui.Colors.V1.gray45
                    , border = Nothing
                    , shadow = Nri.Ui.Colors.V1.gray92
                    }

                LoadingColors ->
                    { background = Nri.Ui.Colors.V1.glacier
                    , hover = Nri.Ui.Colors.V1.glacier
                    , text = Nri.Ui.Colors.V1.navy
                    , border = Nothing
                    , shadow = Nri.Ui.Colors.V1.glacier
                    }

                SuccessColors ->
                    { background = Nri.Ui.Colors.V1.greenDark
                    , hover = Nri.Ui.Colors.V1.greenDark
                    , text = Nri.Ui.Colors.V1.white
                    , border = Nothing
                    , shadow = Nri.Ui.Colors.V1.greenDark
                    }

                ErrorColors ->
                    { background = Nri.Ui.Colors.V1.purple
                    , hover = Nri.Ui.Colors.V1.purple
                    , text = Nri.Ui.Colors.V1.white
                    , border = Nothing
                    , shadow = Nri.Ui.Colors.V1.purple
                    }
    in
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


sizeStyle : ButtonSize -> List Style
sizeStyle size =
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
    in
    [ fontSize (px config.fontSize)
    , borderRadius (px 8)
    , Css.height (px config.height)
    , lineHeight (px config.lineHeight)
    , padding2 zero (px config.sidePadding)
    , boxSizing borderBox
    , minWidth (px config.minWidth)
    , borderWidth (px 1)
    , borderBottomWidth (px config.shadowHeight)

    -- , Css.Foreign.withClass ExplicitWidth -- TODO
    --     [ padding2 zero (px 4)
    --     , boxSizing borderBox
    --     ]
    -- , Css.Foreign.withClass IsLink -- TODO
    --     [ lineHeight (px config.height)
    --     ]
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

    -- type CssClasses
    --     = IsLink
    --     | ExplicitWidth
    --     | SizeStyle ButtonSize
    --     | LabelIconAndText
    --     | Toggled
    --     | Delete
    --     | NewStyle
    -- styles : Styles.StylesWithAssets Never CssClasses msg { r | icons_xBlue_svg : Asset }
    -- styles =
    -- let
    -- Borderless buttons get bigger icons
    -- , Css.Foreign.withClass (ColorsStyle BorderlessColors)
    --     [ Css.Foreign.descendants
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
    -- ]
    ]



-- in
-- Styles.stylesWithAssets "Nri-Ui-Button-V2-" <|
--     \assets ->
--         , Css.Foreign.class (ColorsStyle BorderlessColors)
--             [ Css.hover
--                 [ textDecoration underline
--                 , Css.disabled
--                     [ textDecoration none
--                     ]
--                 ]
--             ]
--         , Css.Foreign.class (ColorsStyle PremiumColors)
--             [ Css.property "box-shadow" "none"
--             , marginBottom zero
--             ]
--         , Css.Foreign.class (ColorsStyle InactiveColors)
--             [ Css.property "box-shadow" "none"
--             , Css.property "border" "none"
--             , marginBottom zero
--             ]
--         , Css.Foreign.class (ColorsStyle SuccessColors)
--             [ Css.property "box-shadow" "none"
--             , Css.property "border" "none"
--             , marginBottom zero
--             ]
--         , Css.Foreign.class (ColorsStyle ErrorColors)
--             [ Css.property "box-shadow" "none"
--             , Css.property "border" "none"
--             , marginBottom zero
--             ]
--         , Css.Foreign.class (ColorsStyle LoadingColors)
--             [ Css.property "box-shadow" "none"
--             , Css.property "border" "none"
--             , marginBottom zero
--             ]
--         , Css.Foreign.class Delete
--             [ display inlineBlock
--             , backgroundImage (url <| AssetPath.url assets.icons_xBlue_svg)
--             , backgroundRepeat noRepeat
--             , backgroundColor transparent
--             , backgroundPosition center
--             , backgroundSize contain
--             , Css.property "border" "none"
--             , Css.width (px 15)
--             , Css.height (px 15)
--             , margin2 zero (px 6)
--             , cursor pointer
--             ]
--         , Css.Foreign.class Toggled
--             [ color Nri.Ui.Colors.V1.gray20
--             , backgroundColor Nri.Ui.Colors.V1.glacier
--             , boxShadow5 inset zero (px 3) zero (withAlpha 0.2 Nri.Ui.Colors.V1.gray20)
--             , border3 (px 1) solid Nri.Ui.Colors.V1.azure
--             , fontWeight bold
--             ]
--         ]
