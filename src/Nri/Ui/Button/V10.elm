module Nri.Ui.Button.V10 exposing
    ( button, link
    , Attribute
    , onClick
    , href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking
    , small, medium, large, modal
    , exactWidth, boundedWidth, unboundedWidth, fillContainerWidth
    , primary, secondary, tertiary, danger, premium
    , enabled, unfulfilled, disabled, error, loading, success
    , icon, custom, nriDescription, testId, id
    , hideIconForMobile, hideIconFor
    , css, notMobileCss, mobileCss, quizEngineMobileCss
    , delete
    , toggleButton
    )

{-| Notes for V11:

The next version of `Button` should add a `hideTextForMobile` helper.
This will require adding a selector for the text. We are not making this change in V10, as
adding a span around the text could potentially lead to regressions.


# Patch changes:

  - uses ClickableAttributes
  - adds `nriDescription`, `testId`, and `id` helpers
  - adds `modal` helper, an alias for `large` size
  - adds `notMobileCss`, `mobileCss`, `quizEngineMobileCss`
  - adds `hideIconForMobile` and `hideIconFor`
  - support 'disabled' links according to [Scott O'Hara's disabled links](https://www.scottohara.me/blog/2021/05/28/disabled-links.html) article
  - adds `tertiary` style


# Changes from V9:

  - Explicitly zeroes out all margin
  - adds `css` helper


# Create a button or link

@docs button, link
@docs Attribute


## Behavior

@docs onClick
@docs href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking


## Sizing

@docs small, medium, large, modal
@docs exactWidth, boundedWidth, unboundedWidth, fillContainerWidth


## Change the color scheme

@docs primary, secondary, tertiary, danger, premium


## Change the state (buttons only)

@docs enabled, unfulfilled, disabled, error, loading, success


## Customization

@docs icon, custom, nriDescription, testId, id


### CSS

@docs hideIconForMobile, hideIconFor
@docs css, notMobileCss, mobileCss, quizEngineMobileCss


# Commonly-used buttons

@docs delete
@docs toggleButton

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Role as Role
import ClickableAttributes exposing (ClickableAttributes)
import Css exposing (Style)
import Css.Global
import Css.Media exposing (MediaQuery)
import Html.Styled as Styled
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Markdown.Block
import Markdown.Inline
import Nri.Ui
import Nri.Ui.Colors.Extra as ColorsExtra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Fonts.V1
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Svg.V1 as NriSvg exposing (Svg)
import Svg
import Svg.Attributes
import TransparentColor


styledName : String -> String
styledName suffix =
    "Nri-Ui-Button-V10-" ++ suffix


{-|

    Button.button "My great button!"
        [ Button.onClick ()
        , Button.enabled
        ]

By default, the button is enabled, Medium sized, with primary colors, and an unbounded width.

-}
button : String -> List (Attribute msg) -> Html msg
button name attributes =
    (label name :: attributes)
        |> List.foldl (\(Attribute attribute) b -> attribute b) build
        |> renderButton


{-|

    Button.link "My great link!"
        [ Button.href "My href"
        , Button.secondary
        ]

By default, the link is Medium sized, with primary colors, and an unbounded width.

-}
link : String -> List (Attribute msg) -> Html msg
link name attributes =
    (label name :: attributes)
        |> List.foldl (\(Attribute attribute) l -> attribute l) build
        |> renderLink


{-| -}
label : String -> Attribute msg
label label_ =
    set (\attributes -> { attributes | label = label_ })


{-| -}
icon : Svg -> Attribute msg
icon icon_ =
    set (\attributes -> { attributes | icon = Just icon_ })


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying Button styles change.
Instead, please use the `css` helper.

-}
custom : List (Html.Attribute msg) -> Attribute msg
custom attributes =
    set
        (\config ->
            { config
                | customAttributes = List.append config.customAttributes attributes
            }
        )


{-| -}
nriDescription : String -> Attribute msg
nriDescription description =
    custom [ ExtraAttributes.nriDescription description ]


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
hideIconFor : MediaQuery -> Attribute msg
hideIconFor mediaQuery =
    css
        [ Css.Media.withMedia [ mediaQuery ]
            [ Css.Global.descendants
                [ Css.Global.selector "[role=img]"
                    [ Css.display Css.none
                    ]
                ]
            ]
        ]


{-| -}
css : List Style -> Attribute msg
css styles =
    set
        (\config ->
            { config
                | customStyles = List.append config.customStyles styles
            }
        )


{-| Equivalent to:

    Button.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.notMobile ] styles ]

-}
notMobileCss : List Style -> Attribute msg
notMobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.notMobile ] styles ]


{-| Equivalent to:

    Button.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.mobile ] styles ]

-}
mobileCss : List Style -> Attribute msg
mobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.mobile ] styles ]


{-| Equivalent to:

    Button.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.quizEngineMobile ] styles ]

-}
quizEngineMobileCss : List Style -> Attribute msg
quizEngineMobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.quizEngineMobile ] styles ]



-- LINKING, CLICKING, and TRACKING BEHAVIOR


setClickableAttributes :
    (ClickableAttributes String msg -> ClickableAttributes String msg)
    -> Attribute msg
setClickableAttributes apply =
    set
        (\attributes ->
            { attributes | clickableAttributes = apply attributes.clickableAttributes }
        )


{-| -}
onClick : msg -> Attribute msg
onClick msg =
    setClickableAttributes (ClickableAttributes.onClick msg)


{-| -}
href : String -> Attribute msg
href url =
    setClickableAttributes (ClickableAttributes.href url)


{-| Use this link for routing within a single page app.

This will make a normal <a> tag, but change the Events.onClick behavior to avoid reloading the page.

See <https://github.com/elm-lang/html/issues/110> for details on this implementation.

-}
linkSpa : String -> Attribute msg
linkSpa url =
    setClickableAttributes (ClickableAttributes.linkSpa url)


{-| -}
linkWithMethod : { method : String, url : String } -> Attribute msg
linkWithMethod config =
    setClickableAttributes (ClickableAttributes.linkWithMethod config)


{-| -}
linkWithTracking : { track : msg, url : String } -> Attribute msg
linkWithTracking config =
    setClickableAttributes (ClickableAttributes.linkWithTracking config)


{-| -}
linkExternal : String -> Attribute msg
linkExternal url =
    setClickableAttributes (ClickableAttributes.linkExternal url)


{-| -}
linkExternalWithTracking : { track : msg, url : String } -> Attribute msg
linkExternalWithTracking config =
    setClickableAttributes (ClickableAttributes.linkExternalWithTracking config)



-- BUTTON SIZING


{-| -}
small : Attribute msg
small =
    set (\attributes -> { attributes | size = Small })


{-| -}
medium : Attribute msg
medium =
    set (\attributes -> { attributes | size = Medium })


{-| -}
large : Attribute msg
large =
    set (\attributes -> { attributes | size = Large })


{-| Alias for Button.large
-}
modal : Attribute msg
modal =
    large



-- BUTTON WIDTH


type ButtonWidth
    = WidthExact Int
    | WidthUnbounded
    | WidthFillContainer
    | WidthBounded { min : Int, max : Int }


{-| Sizes for buttons and links that have button classes
-}
type ButtonSize
    = Small
    | Medium
    | Large


{-| Define a size in `px` for the button's total width.
-}
exactWidth : Int -> Attribute msg
exactWidth inPx =
    set (\attributes -> { attributes | width = WidthExact inPx })


{-| Leave the maxiumum width unbounded (there is a minimum width).
-}
unboundedWidth : Attribute msg
unboundedWidth =
    set (\attributes -> { attributes | width = WidthUnbounded })


{-| Make a button that is at least `min` large, and which will grow with
its content up to `max`. Both bounds are inclusive (`min <= actual value <=
max`.)
-}
boundedWidth : { min : Int, max : Int } -> Attribute msg
boundedWidth bounds =
    set (\attributes -> { attributes | width = WidthBounded bounds })


{-| -}
fillContainerWidth : Attribute msg
fillContainerWidth =
    set (\attributes -> { attributes | width = WidthFillContainer })



-- COLOR SCHEMES


{-| -}
primary : Attribute msg
primary =
    set
        (\attributes ->
            { attributes | style = primaryColors }
        )


{-| -}
secondary : Attribute msg
secondary =
    set
        (\attributes ->
            { attributes | style = secondaryColors }
        )


{-| -}
tertiary : Attribute msg
tertiary =
    set
        (\attributes ->
            { attributes | style = tertiaryColors }
        )


{-| -}
danger : Attribute msg
danger =
    set
        (\attributes ->
            { attributes
                | style =
                    { background = Colors.red
                    , hover = Colors.redDark
                    , text = Colors.white
                    , border = Nothing
                    , shadow = Colors.redDark
                    }
            }
        )


{-| -}
premium : Attribute msg
premium =
    set
        (\attributes ->
            { attributes
                | style =
                    { background = Colors.yellow
                    , hover = Colors.ochre
                    , text = Colors.navy
                    , border = Nothing
                    , shadow = Colors.ochre
                    }
            }
        )



-- BUTTON STATE


type ButtonState
    = Enabled
    | Unfulfilled
    | Disabled
    | Error
    | Loading
    | Success


isDisabled : ButtonState -> Bool
isDisabled state =
    case state of
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


{-| -}
enabled : Attribute msg
enabled =
    set (\attributes -> { attributes | state = Enabled })


{-| Shows inactive styles.
-}
unfulfilled : Attribute msg
unfulfilled =
    set (\attributes -> { attributes | state = Unfulfilled })


{-| Shows inactive styling.

If a button, this attribute will disable it as you'd expect.

If a link, this attribute will follow the pattern laid out in [Scott O'Hara's disabled links](https://www.scottohara.me/blog/2021/05/28/disabled-links.html) article,
and essentially make the anchor a disabled placeholder.

_Caveat!_

The styleguide example will NOT work correctly because of <https://github.com/elm/browser/issues/34>, which describes a problem where "a tags without href generate a navigation event".

In most cases, if you're not using Browser.application, disabled links should work just fine.

-}
disabled : Attribute msg
disabled =
    set (\attributes -> { attributes | state = Disabled })


{-| Shows error styling. If a button, this attribute will disable it.
-}
error : Attribute msg
error =
    set (\attributes -> { attributes | state = Error })


{-| Shows loading styling. If a button, this attribute will disable it.
-}
loading : Attribute msg
loading =
    set (\attributes -> { attributes | state = Loading })


{-| Shows success styling. If a button, this attribute will disable it.
-}
success : Attribute msg
success =
    set (\attributes -> { attributes | state = Success })


{-| -}
type Attribute msg
    = Attribute (ButtonOrLink msg -> ButtonOrLink msg)



-- INTERNALS


set :
    (ButtonOrLinkAttributes msg -> ButtonOrLinkAttributes msg)
    -> Attribute msg
set with =
    Attribute (\(ButtonOrLink config) -> ButtonOrLink (with config))


build : ButtonOrLink msg
build =
    ButtonOrLink
        { clickableAttributes = ClickableAttributes.init
        , size = Medium
        , style = primaryColors
        , width = WidthUnbounded
        , label = ""
        , state = Enabled
        , icon = Nothing
        , customAttributes = []
        , customStyles = []
        }


type ButtonOrLink msg
    = ButtonOrLink (ButtonOrLinkAttributes msg)


type alias ButtonOrLinkAttributes msg =
    { clickableAttributes : ClickableAttributes String msg
    , size : ButtonSize
    , style : ColorPalette
    , width : ButtonWidth
    , label : String
    , state : ButtonState
    , icon : Maybe Svg
    , customAttributes : List (Html.Attribute msg)
    , customStyles : List Style
    }


renderButton : ButtonOrLink msg -> Html msg
renderButton ((ButtonOrLink config) as button_) =
    let
        buttonStyle_ =
            getColorPalette button_
    in
    Nri.Ui.styled Html.button
        (styledName "customButton")
        [ buttonStyles config.size config.width buttonStyle_ config.customStyles
        , Css.focus [ Css.outline Css.none, FocusRing.boxShadows [] ]
        ]
        (ClickableAttributes.toButtonAttributes config.clickableAttributes
            ++ Attributes.disabled (isDisabled config.state)
            :: Attributes.type_ "button"
            :: Attributes.class "custom-focus-ring"
            :: config.customAttributes
        )
        [ viewLabel config.size config.icon config.label ]


renderLink : ButtonOrLink msg -> Html msg
renderLink ((ButtonOrLink config) as link_) =
    let
        colorPalette =
            getColorPalette link_

        ( linkFunctionName, attributes ) =
            ClickableAttributes.toLinkAttributes
                { routeToString = identity
                , isDisabled = isDisabled config.state
                }
                config.clickableAttributes
    in
    Nri.Ui.styled Styled.a
        (styledName linkFunctionName)
        [ buttonStyles config.size config.width colorPalette config.customStyles
        , Css.focus [ Css.outline Css.none, FocusRing.boxShadows [] ]
        ]
        (Attributes.class "custom-focus-ring"
            :: attributes
            ++ config.customAttributes
        )
        [ viewLabel config.size config.icon config.label ]



-- DELETE BUTTON


{-| DEPRECATED: this should be removed in Button.V11.
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
          Aria.label config.label
        ]
        [ Svg.svg [ Svg.Attributes.viewBox "0 0 25 25" ]
            [ Svg.title [] [ Styled.toUnstyled (Styled.text "Delete") ]
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
        toggledBoxShadow =
            "inset 0 3px 0 "
                ++ (Colors.gray20
                        |> ColorsExtra.fromCssColor
                        |> TransparentColor.fromColor (TransparentColor.customOpacity 0.2)
                        |> TransparentColor.toRGBAString
                   )

        toggledStyles =
            if config.pressed then
                Css.batch
                    [ Css.color Colors.gray20
                    , Css.backgroundColor Colors.glacier
                    , Css.focus
                        [ Css.outline Css.none
                        , FocusRing.boxShadows [ toggledBoxShadow ]
                        ]
                    , Css.border3 (Css.px 1) Css.solid Colors.azure
                    , Css.fontWeight Css.bold
                    ]

            else
                Css.batch
                    []
    in
    Nri.Ui.styled Html.button
        (styledName "toggleButton")
        [ buttonStyles Medium WidthUnbounded secondaryColors []
        , toggledStyles
        , Css.verticalAlign Css.middle
        ]
        [ Events.onClick
            (if config.pressed then
                config.onDeselect

             else
                config.onSelect
            )
        , Aria.pressed <| Just config.pressed

        -- reference: https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_button_role#Labeling_buttons
        , Role.button

        -- Note: setting type: 'button' removes the default behavior of submit
        -- equivalent to preventDefaultBehavior = false
        -- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button#attr-name
        , Attributes.type_ "button"
        , Attributes.class "custom-focus-ring"
        ]
        [ viewLabel Medium Nothing config.label ]


buttonStyles : ButtonSize -> ButtonWidth -> ColorPalette -> List Style -> Style
buttonStyles size width colors customStyles =
    Css.batch
        [ buttonStyle
        , sizeStyle size width
        , colorStyle colors
        , Css.batch customStyles
        ]


viewLabel : ButtonSize -> Maybe Svg -> String -> Html msg
viewLabel size maybeSvg label_ =
    let
        { fontAndIconSize } =
            sizeConfig size
    in
    Nri.Ui.styled Html.span
        "button-label-span"
        [ Css.overflow Css.hidden -- Keep scrollbars out of our button
        , Css.overflowWrap Css.breakWord -- Ensure that words that exceed the button width break instead of disappearing
        , Css.padding2 (Css.px 2) Css.zero -- Without a bit of bottom padding, text that extends below the baseline, like "g" gets cut off
        , Css.displayFlex
        , Css.alignItems Css.center
        ]
        []
        (case maybeSvg of
            Nothing ->
                renderMarkdown label_

            Just svg ->
                (svg
                    |> NriSvg.withWidth fontAndIconSize
                    |> NriSvg.withHeight fontAndIconSize
                    |> NriSvg.withCss
                        [ Css.flexShrink Css.zero
                        , Css.marginRight (Css.px 5)
                        ]
                    |> NriSvg.toHtml
                )
                    :: renderMarkdown label_
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
        , Css.margin Css.zero
        , Css.hover [ Css.textDecoration Css.none ]
        , Css.disabled [ Css.cursor Css.notAllowed ]
        , Css.Global.withAttribute "aria-disabled=true" [ Css.cursor Css.notAllowed ]
        , Css.display Css.inlineFlex
        , Css.alignItems Css.center
        , Css.justifyContent Css.center
        ]



-- COLORS


type alias ColorPalette =
    { background : Css.Color
    , hover : Css.Color
    , text : Css.Color
    , border : Maybe Css.Color
    , shadow : Css.Color
    }


primaryColors : ColorPalette
primaryColors =
    { background = Colors.azure
    , hover = Colors.azureDark
    , text = Colors.white
    , border = Nothing
    , shadow = Colors.azureDark
    }


secondaryColors : ColorPalette
secondaryColors =
    { background = Colors.white
    , hover = Colors.glacier
    , text = Colors.azure
    , border = Just <| Colors.azure
    , shadow = Colors.azure
    }


tertiaryColors : ColorPalette
tertiaryColors =
    { background = Colors.white
    , hover = Colors.frost
    , text = Colors.navy
    , border = Just <| Colors.gray75
    , shadow = Colors.gray75
    }


getColorPalette : ButtonOrLink msg -> ColorPalette
getColorPalette (ButtonOrLink config) =
    case config.state of
        Enabled ->
            config.style

        Disabled ->
            { background = Colors.gray92
            , hover = Colors.gray92
            , text = Colors.gray45
            , border = Nothing
            , shadow = Colors.gray92
            }

        Error ->
            { background = Colors.purple
            , hover = Colors.purple
            , text = Colors.white
            , border = Nothing
            , shadow = Colors.purple
            }

        Unfulfilled ->
            { background = Colors.gray92
            , hover = Colors.gray92
            , text = Colors.gray45
            , border = Nothing
            , shadow = Colors.gray92
            }

        Loading ->
            { background = Colors.glacier
            , hover = Colors.glacier
            , text = Colors.navy
            , border = Nothing
            , shadow = Colors.glacier
            }

        Success ->
            { background = Colors.greenDark
            , hover = Colors.greenDark
            , text = Colors.white
            , border = Nothing
            , shadow = Colors.greenDark
            }


colorStyle : ColorPalette -> Style
colorStyle colorPalette =
    Css.batch
        [ Css.color colorPalette.text
        , Css.backgroundColor colorPalette.background
        , Css.fontWeight (Css.int 700)
        , Css.textAlign Css.center
        , case colorPalette.border of
            Nothing ->
                Css.borderStyle Css.none

            Just color ->
                Css.batch
                    [ Css.borderColor color
                    , Css.borderStyle Css.solid
                    ]
        , Css.borderBottomStyle Css.solid
        , Css.borderBottomColor colorPalette.shadow
        , Css.fontStyle Css.normal
        , Css.hover
            [ Css.color colorPalette.text
            , Css.backgroundColor colorPalette.hover
            , Css.disabled [ Css.backgroundColor colorPalette.background ]
            ]
        , Css.visited [ Css.color colorPalette.text ]
        ]


sizeConfig : ButtonSize -> { fontAndIconSize : Css.Px, height : number, shadowHeight : number, minWidth : number }
sizeConfig size =
    case size of
        Small ->
            { fontAndIconSize = Css.px 15
            , height = 36
            , shadowHeight = 2
            , minWidth = 75
            }

        Medium ->
            { fontAndIconSize = Css.px 15
            , height = 45
            , shadowHeight = 3
            , minWidth = 100
            }

        Large ->
            { fontAndIconSize = Css.px 20
            , height = 56
            , shadowHeight = 4
            , minWidth = 200
            }


sizeStyle : ButtonSize -> ButtonWidth -> Style
sizeStyle size width =
    let
        config =
            sizeConfig size

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

                WidthBounded { min, max } ->
                    [ Css.maxWidth (Css.px (toFloat max))
                    , Css.minWidth (Css.px (toFloat min))
                    , Css.paddingRight (Css.px 16)
                    , Css.paddingLeft (Css.px 16)
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
        [ Css.fontSize config.fontAndIconSize
        , Css.borderRadius (Css.px 8)
        , Css.lineHeight (Css.px lineHeightPx)
        , Css.boxSizing Css.borderBox
        , Css.borderWidth (Css.px 1)
        , Css.borderBottomWidth (Css.px config.shadowHeight)
        , Css.batch sizingAttributes
        , Css.batch widthAttributes
        ]
