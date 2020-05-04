module Nri.Ui.Button.V8 exposing
    ( ButtonSize(..), ButtonWidth(..), ButtonStyle(..), ButtonState(..)
    , button
    , delete
    , toggleButton
    , link, linkSpa
    , linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking
    )

{-|


# Changes from V7:

  - Removes dependency on Icon that makes versioned assets hard to work with
  - Removes copyToClipboard, which has additional dependencies and also isn't broadly used
  - Removes customButton from what's exposed, as it's not in use
  - Removes Borderless style (use ClickableText instead)


# About:

Common NoRedInk buttons. For accessibility purposes, buttons that perform an
action on the current page should be HTML `<button>` elements and are created here
with `*Button` functions. Buttons that take the user to a new page should be
HTML `<a>` elements and are created here with `*Link` functions. Both versions
should be able to use the same CSS class in all cases.

There will generally be a `*Button` and `*Link` version of each button style.
(These will be created as they are needed.)

In general a button should never truncate or obscure its contents. This could
make it difficult or impossible for a student or teacher to use the site, so in
general choose buttons that grow to fit their contents. It is better to risk
weird layout than to block users. Might this be a golden rule? Of course there
may be exceptions, for example if button content is supplied by an end-user.


## Common configs

@docs ButtonSize, ButtonWidth, ButtonStyle, ButtonState


## `<button>` Buttons

@docs button
@docs delete
@docs toggleButton


## `<a>` Buttons

@docs link, linkSpa
@docs linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking

-}

import Accessibility.Styled as Html exposing (Attribute, Html)
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Widget as Widget
import Css exposing (Style)
import Css.Global
import EventExtras
import Html as RootHtml
import Html.Styled as Styled
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Decode
import Markdown.Block
import Markdown.Inline
import Nri.Ui
import Nri.Ui.AssetPath as AssetPath exposing (Asset)
import Nri.Ui.Colors.Extra as ColorsExtra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1
import Nri.Ui.Svg.V1 as NriSvg exposing (Svg)
import Svg
import Svg.Attributes


{-| Sizes for buttons and links that have button classes
-}
type ButtonSize
    = Small
    | Medium
    | Large


{-| Width sizing behavior for buttons.

`WidthExact Int` defines a size in `px` for the button's total width, and
`WidthUnbounded` leaves the maxiumum width unbounded (there is a minimum width).

-}
type ButtonWidth
    = WidthExact Int
    | WidthUnbounded
    | WidthFillContainer


{-| Styleguide-approved styles for your buttons!
-}
type ButtonStyle
    = Primary
    | Secondary
    | Danger
    | Premium


{-| Describes the state of a button. Has consequences for appearance and disabled attribute.

  - Enabled: An enabled button. Takes the appearance of ButtonStyle
  - Unfulfilled: A button which appears with the InactiveColors palette but is not disabled.
  - Disabled: A button which appears with the InactiveColors palette and is disabled.
  - Error: A button which appears with the ErrorColors palette and is disabled.
  - Loading: A button which appears with the LoadingColors palette and is disabled
  - Success: A button which appears with the SuccessColors palette and is disabled

-}
type ButtonState
    = Enabled
    | Unfulfilled
    | Disabled
    | Error
    | Loading
    | Success


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
button :
    { onClick : msg
    , size : ButtonSize
    , style : ButtonStyle
    , width : ButtonWidth
    }
    ->
        { label : String
        , state : ButtonState
        , icon : Maybe Svg
        }
    -> Html msg
button config content =
    let
        buttonStyle_ =
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
        (styledName "customButton")
        [ buttonStyles config.size config.width buttonStyle_ ]
        [ Events.onClick config.onClick
        , Attributes.disabled disabled
        , Attributes.type_ "button"
        ]
        [ viewLabel content.icon content.label ]



-- DELETE BUTTON


{-| A delete button (blue X)
-}
delete : { label : String, onClick : msg } -> Html msg
delete config =
    Nri.Ui.styled Html.button
        (styledName "delete")
        [ Css.display Css.inlineBlock
        , Css.backgroundRepeat Css.noRepeat
        , Css.backgroundColor Css.transparent
        , Css.backgroundPosition Css.center
        , Css.backgroundSize Css.contain
        , Css.border Css.zero
        , Css.width (Css.px 15)
        , Css.height (Css.px 15)
        , Css.padding Css.zero
        , Css.margin2 Css.zero (Css.px 6)
        , Css.cursor Css.pointer
        , Css.color Colors.azure
        ]
        [ Events.onClick config.onClick
        , Attributes.type_ "button"
        , -- reference: https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_button_role#Labeling_buttons
          Widget.label config.label
        ]
        [ Svg.svg [ Svg.Attributes.viewBox "0 0 25 25" ]
            [ Svg.title [] [ RootHtml.text "Delete" ]
            , Svg.path
                [ Svg.Attributes.fill "#146aff" -- TODO: this should be azure, but css colors aren't extractable afaik
                , Svg.Attributes.d "M1.067 6.015c-1.423-1.422-1.423-3.526 0-4.948 1.422-1.423 3.526-1.423 4.948 0l6.371 6.37 6.371-6.37c1.422-1.423 3.783-1.423 5.176 0 1.423 1.422 1.423 3.782 0 5.176l-6.37 6.37 6.37 6.372c1.423 1.422 1.423 3.526 0 4.948-1.422 1.423-3.526 1.423-4.948 0l-6.371-6.37-6.371 6.37c-1.422 1.423-3.783 1.423-5.176 0-1.423-1.422-1.423-3.782 0-5.176l6.37-6.143-6.37-6.599z"
                ]
                []
            ]
            |> Styled.fromUnstyled
        ]



-- TOGGLE BUTTON


{-| A button that can be toggled into a pressed state and back again.
-}
toggleButton :
    { label : String
    , onSelect : msg
    , onDeselect : msg
    , pressed : Bool
    }
    -> Html msg
toggleButton config =
    let
        toggledStyles =
            if config.pressed then
                Css.batch
                    [ Css.color Colors.gray20
                    , Css.backgroundColor Colors.glacier
                    , Css.boxShadow5 Css.inset Css.zero (Css.px 3) Css.zero (ColorsExtra.withAlpha 0.2 Colors.gray20)
                    , Css.border3 (Css.px 1) Css.solid Colors.azure
                    , Css.fontWeight Css.bold
                    ]

            else
                Css.batch
                    []
    in
    Nri.Ui.styled Html.button
        (styledName "toggleButton")
        [ buttonStyles Medium WidthUnbounded SecondaryColors
        , toggledStyles
        ]
        [ Events.onClick
            (if config.pressed then
                config.onDeselect

             else
                config.onSelect
            )
        , Widget.pressed <| Just config.pressed

        -- reference: https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_button_role#Labeling_buttons
        , Role.button

        -- Note: setting type: 'button' removes the default behavior of submit
        -- equivalent to preventDefaultBehavior = false
        -- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button#attr-name
        , Attributes.type_ "button"
        ]
        [ viewLabel Nothing config.label ]



-- LINKS THAT LOOK LIKE BUTTONS


{-| Wrap some text so it looks like a button, but actually is wrapped in an anchor to
some url.

NOTE: Links do not support two-line labels.

-}
link :
    { label : String
    , icon : Maybe Svg
    , url : String
    , size : ButtonSize
    , style : ButtonStyle
    , width : ButtonWidth
    }
    -> Html msg
link =
    linkBase "link" [ Attributes.target "_self" ]


{-| Use this link for routing within a single page app.

This will make a normal <a> tag, but change the Events.onClick behavior to avoid reloading the page.

See <https://github.com/elm-lang/html/issues/110> for details on this implementation.

-}
linkSpa :
    (route -> String)
    -> (route -> msg)
    ->
        { label : String
        , icon : Maybe Svg
        , size : ButtonSize
        , style : ButtonStyle
        , width : ButtonWidth
        , route : route
        }
    -> Html msg
linkSpa toUrl toMsg config =
    linkBase
        "linkSpa"
        [ EventExtras.onClickPreventDefaultForLinkWithHref (toMsg config.route)
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
linkExternal :
    { label : String
    , icon : Maybe Svg
    , url : String
    , size : ButtonSize
    , style : ButtonStyle
    , width : ButtonWidth
    }
    -> Html msg
linkExternal =
    linkBase "linkExternal" [ Attributes.target "_blank" ]


{-| Wrap some text so it looks like a button, but actually is wrapped in an anchor to
some url, and it's an HTTP request (Rails includes JS to make this use the given HTTP method)
-}
linkWithMethod :
    String
    ->
        { label : String
        , icon : Maybe Svg
        , url : String
        , size : ButtonSize
        , style : ButtonStyle
        , width : ButtonWidth
        }
    -> Html msg
linkWithMethod method =
    linkBase "linkWithMethod" [ Attributes.attribute "data-method" method ]


{-| Wrap some text so it looks like a button, but actually is wrapped in an anchor to some url.
This should only take in messages that result in a Msg that triggers Analytics.trackAndRedirect. For buttons that trigger other effects on the page, please use Nri.Button.button instead
-}
linkWithTracking :
    msg
    ->
        { label : String
        , icon : Maybe Svg
        , url : String
        , size : ButtonSize
        , style : ButtonStyle
        , width : ButtonWidth
        }
    -> Html msg
linkWithTracking onTrack =
    linkBase
        "linkWithTracking"
        [ Events.preventDefaultOn "click"
            (Json.Decode.succeed ( onTrack, True ))
        ]


{-| Wrap some text so it looks like a button, but actually is wrapped in an anchor to some url and have it open to an external site

This should only take in messages that result in tracking events. For buttons that trigger other effects on the page, please use Nri.Ui.Button.V2.button instead

-}
linkExternalWithTracking :
    msg
    ->
        { label : String
        , icon : Maybe Svg
        , url : String
        , size : ButtonSize
        , style : ButtonStyle
        , width : ButtonWidth
        }
    -> Html msg
linkExternalWithTracking onTrack =
    linkBase
        "linkExternalWithTracking"
        [ Attributes.target "_blank"
        , EventExtras.onClickForLinkWithHref onTrack
        ]


{-| Helper function for building links with an arbitrary number of Attributes
-}
linkBase :
    String
    -> List (Attribute msg)
    ->
        { label : String
        , icon : Maybe Svg
        , url : String
        , size : ButtonSize
        , style : ButtonStyle
        , width : ButtonWidth
        }
    -> Html msg
linkBase linkFunctionName extraAttrs config =
    Nri.Ui.styled Styled.a
        (styledName linkFunctionName)
        [ buttonStyles config.size config.width (styleToColorPalette config.style) ]
        (Attributes.href config.url
            :: extraAttrs
        )
        [ viewLabel config.icon config.label ]



-- HELPERS


type ColorPalette
    = PrimaryColors
    | SecondaryColors
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

        Danger ->
            DangerColors

        Premium ->
            PremiumColors


buttonStyles : ButtonSize -> ButtonWidth -> ColorPalette -> Style
buttonStyles size width colorPalette =
    Css.batch
        [ buttonStyle
        , colorStyle colorPalette
        , sizeStyle size width
        ]


viewLabel : Maybe Svg -> String -> Html msg
viewLabel maybeSvg label =
    Nri.Ui.styled Html.span
        "button-label-span"
        [ Css.overflow Css.hidden -- Keep scrollbars out of our button
        , Css.overflowWrap Css.breakWord -- Ensure that words that exceed the button width break instead of disappearing
        , Css.padding2 (Css.px 2) Css.zero -- Without a bit of bottom padding, text that extends below the baseline, like "g" gets cut off
        ]
        []
        (case maybeSvg of
            Nothing ->
                renderMarkdown label

            Just svg ->
                NriSvg.toHtml svg :: renderMarkdown label
        )


renderMarkdown : String -> List (Html msg)
renderMarkdown markdown =
    case Markdown.Block.parse Nothing markdown of
        -- It seems to be always first wrapped in a `Paragraph` and never directly a `PlainInline`
        [ Markdown.Block.Paragraph _ inlines ] ->
            List.map (Markdown.Inline.toHtml >> Styled.fromUnstyled) inlines

        _ ->
            [ Html.text markdown ]



-- STYLES


buttonStyle : Style
buttonStyle =
    Css.batch
        [ Css.cursor Css.pointer
        , -- Specifying the font can and should go away after bootstrap is removed from application.css
          Nri.Ui.Fonts.V1.baseFont
        , Css.textOverflow Css.ellipsis
        , Css.overflow Css.hidden
        , Css.textDecoration Css.none
        , Css.backgroundImage Css.none
        , Css.textShadow Css.none
        , Css.property "transition" "background-color 0.2s, color 0.2s, box-shadow 0.2s, border 0.2s, border-width 0s"
        , Css.boxShadow Css.none
        , Css.border Css.zero
        , Css.marginBottom Css.zero
        , Css.hover [ Css.textDecoration Css.none ]
        , Css.disabled [ Css.cursor Css.notAllowed ]
        , Css.display Css.inlineFlex
        , Css.alignItems Css.center
        , Css.justifyContent Css.center
        ]


colorStyle : ColorPalette -> Style
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
    Css.batch
        [ Css.batch additionalStyles
        , Css.color config.text
        , Css.backgroundColor config.background
        , Css.fontWeight (Css.int 700)
        , Css.textAlign Css.center
        , case config.border of
            Nothing ->
                Css.borderStyle Css.none

            Just color ->
                Css.batch
                    [ Css.borderColor color
                    , Css.borderStyle Css.solid
                    ]
        , Css.borderBottomStyle Css.solid
        , Css.borderBottomColor config.shadow
        , Css.fontStyle Css.normal
        , Css.hover
            [ Css.color config.text
            , Css.backgroundColor config.hover
            , Css.disabled [ Css.backgroundColor config.background ]
            ]
        , Css.visited [ Css.color config.text ]
        ]


sizeStyle : ButtonSize -> ButtonWidth -> Style
sizeStyle size width =
    let
        config =
            case size of
                Small ->
                    { fontSize = 15
                    , height = 36
                    , imageHeight = 15
                    , shadowHeight = 2
                    , minWidth = 75
                    }

                Medium ->
                    { fontSize = 17
                    , height = 45
                    , imageHeight = 15
                    , shadowHeight = 3
                    , minWidth = 100
                    }

                Large ->
                    { fontSize = 20
                    , height = 56
                    , imageHeight = 20
                    , shadowHeight = 4
                    , minWidth = 200
                    }

        sizingAttributes =
            let
                verticalPaddingPx =
                    2
            in
            [ Css.minHeight (Css.px config.height)
            , Css.paddingTop (Css.px verticalPaddingPx)
            , Css.paddingBottom (Css.px verticalPaddingPx)
            ]

        widthAttributes =
            case width of
                WidthExact pxWidth ->
                    [ Css.maxWidth (Css.pct 100)
                    , Css.width (Css.px <| toFloat pxWidth)
                    , Css.paddingRight (Css.px 4)
                    , Css.paddingLeft (Css.px 4)
                    ]

                WidthUnbounded ->
                    [ Css.paddingLeft (Css.px 16)
                    , Css.paddingRight (Css.px 16)
                    , Css.minWidth (Css.px config.minWidth)
                    ]

                WidthFillContainer ->
                    [ Css.paddingLeft (Css.px 16)
                    , Css.paddingRight (Css.px 16)
                    , Css.minWidth (Css.px config.minWidth)
                    , Css.width (Css.pct 100)
                    ]

        lineHeightPx =
            case size of
                Small ->
                    15

                Medium ->
                    19

                Large ->
                    22
    in
    Css.batch
        [ Css.fontSize (Css.px config.fontSize)
        , Css.borderRadius (Css.px 8)
        , Css.lineHeight (Css.px lineHeightPx)
        , Css.boxSizing Css.borderBox
        , Css.borderWidth (Css.px 1)
        , Css.borderBottomWidth (Css.px config.shadowHeight)
        , Css.batch sizingAttributes
        , Css.batch widthAttributes
        , Css.Global.descendants
            [ Css.Global.img
                [ Css.height (Css.px config.imageHeight)
                , Css.marginRight (Css.px <| config.imageHeight / 6)
                , Css.position Css.relative
                , Css.bottom (Css.px 2)
                , Css.verticalAlign Css.middle
                ]
            , Css.Global.svg
                [ Css.height (Css.px config.imageHeight) |> Css.important
                , Css.width (Css.px config.imageHeight) |> Css.important
                , Css.marginRight (Css.px <| config.imageHeight / 6)
                , Css.position Css.relative
                , Css.bottom (Css.px 2)
                , Css.verticalAlign Css.middle
                ]
            , Css.Global.svg
                [ Css.important <| Css.height (Css.px config.imageHeight)
                , Css.important <| Css.width Css.auto
                , Css.maxWidth (Css.px (config.imageHeight * 1.25))
                , Css.paddingRight (Css.px <| config.imageHeight / 6)
                , Css.position Css.relative
                , Css.bottom (Css.px 2)
                , Css.verticalAlign Css.middle
                ]
            ]
        ]


styledName : String -> String
styledName suffix =
    "Nri-Ui-Button-V7-" ++ suffix
