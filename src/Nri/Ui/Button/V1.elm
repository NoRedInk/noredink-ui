module Nri.Ui.Button.V1 exposing
    ( styles
    , ButtonContent
    , ButtonSize(..)
    , ButtonState(..)
    , ButtonStyle(..)
    , button
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

@docs ButtonContent
@docs ButtonSize
@docs ButtonState
@docs ButtonStyle


## `<button>` Buttons

@docs button

-}

import Accessibility exposing (..)
import Css exposing (..)
import Css.Foreign exposing (Snippet, children, descendants, everything, selector)
import DEPRECATED.Css.File exposing (Stylesheet, compile, stylesheet)
import DEPRECATED.Nri.Ui.Styles.V1
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Nri.Ui.Colors.V1
import Nri.Ui.Fonts.V1


{-| Sizes for buttons that have button classes

NOTE: if you add a size here, you need to add Css styles using the sizeStyle helper in the styles definition.

-}
type ButtonSize
    = Tiny
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


{-| Describes the state of a button. Has consequences for appearance and disabled attribute.

  - Enabled: An enabled button. Takes the appearance of ButtonStyle
  - Unfulfilled: A button which appears with the InactiveColors palette but is not disabled.
  - Disabled: A button which appears with the InactiveColors palette and is disabled.
  - Error: A button which appears with the ErrorColors palette and is not disabled.
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
    }


{-| ButtonContent, often changes based on ButtonState.
For example, a button in the "Success" state
may have a different label than a button in the "Error" state
-}
type alias ButtonContent =
    { label : String
    , state : ButtonState
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
        [ styles.class
            [ Button
            , SizeStyle config.size
            , ColorsStyle buttonStyle
            ]
        , onClick config.onClick
        , Html.Attributes.disabled disabled
        , Html.Attributes.type_ "button"
        ]
        [ span []
            [ Html.text content.label
            ]
        ]



-- HELPERS


type ColorPalette
    = PrimaryColors
    | SecondaryColors
    | BorderlessColors
    | DangerColors
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



-- STYLES


type CssClasses
    = Button
    | SizeStyle ButtonSize
    | ColorsStyle ColorPalette


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


{-| Required CSS styles for `Nri.Ui.Button`.
-}
styles : DEPRECATED.Nri.Ui.Styles.V1.Styles Never CssClasses msg
styles =
    let
        newSizeStyle size config =
            Css.Foreign.class (SizeStyle size)
                [ fontSize (px config.fontSize)
                , borderRadius (px 8)
                , Css.height (px config.height)
                , lineHeight (px config.lineHeight)
                , padding2 zero (px config.sidePadding)
                , borderWidth (px 1)
                , borderBottomWidth (px config.shadowHeight)
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
                , borderColor config.border
                , borderStyle solid
                , textAlign center
                , fontStyle config.fontStyle
                , Css.hover
                    [ color config.text
                    , backgroundColor config.hover
                    , if config.border == config.background then
                        borderColor config.hover

                      else
                        Css.batch []
                    , Css.disabled
                        [ backgroundColor config.background
                        , borderColor config.border
                        ]
                    ]
                , Css.visited
                    [ color config.text
                    ]
                ]

        newStyleStyle style config =
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
    DEPRECATED.Nri.Ui.Styles.V1.styles "Nri-Ui-Button-"
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
            ]
        , newSizeStyle Small
            { fontSize = 15
            , height = 36
            , lineHeight = 15
            , sidePadding = 16
            , imageHeight = 15
            , shadowHeight = 2
            }
        , newSizeStyle Medium
            { fontSize = 17
            , height = 45
            , lineHeight = 19
            , sidePadding = 16
            , imageHeight = 15
            , shadowHeight = 3
            }
        , newSizeStyle Large
            { fontSize = 20
            , height = 56
            , lineHeight = 22
            , sidePadding = 16
            , imageHeight = 20
            , shadowHeight = 4
            }
        , newStyleStyle PrimaryColors
            { background = Nri.Ui.Colors.V1.azure
            , hover = hex "0040ad" -- TODO: Add Nri.Ui.Colors.V1.azureDarkened20Percent
            , text = Nri.Ui.Colors.V1.white
            , border = Nothing
            , shadow = Nri.Ui.Colors.V1.azureDark
            }
        , newStyleStyle SecondaryColors
            { background = Nri.Ui.Colors.V1.white
            , hover = Nri.Ui.Colors.V1.glacier
            , text = Nri.Ui.Colors.V1.azure
            , border = Just Nri.Ui.Colors.V1.azure
            , shadow = Nri.Ui.Colors.V1.azure
            }
        , newStyleStyle BorderlessColors
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
        , styleStyle InactiveColors
            { background = Nri.Ui.Colors.V1.gray75
            , hover = Nri.Ui.Colors.V1.gray75
            , text = Nri.Ui.Colors.V1.white
            , border = Nri.Ui.Colors.V1.gray75
            , fontStyle = normal
            }
        , styleStyle LoadingColors
            { background = Nri.Ui.Colors.V1.frost
            , hover = Nri.Ui.Colors.V1.frost
            , text = Nri.Ui.Colors.V1.gray45
            , border = Nri.Ui.Colors.V1.frost
            , fontStyle = normal
            }
        , styleStyle SuccessColors
            { background = Nri.Ui.Colors.V1.green
            , hover = Nri.Ui.Colors.V1.green
            , text = Nri.Ui.Colors.V1.white
            , border = Nri.Ui.Colors.V1.green
            , fontStyle = normal
            }
        , newStyleStyle DangerColors
            { background = Nri.Ui.Colors.V1.red
            , hover = hex "c00039" -- TODO: use Nri.Ui.Colors.V1.redDarkened20Percent
            , text = Nri.Ui.Colors.V1.white
            , border = Nothing
            , shadow = Nri.Ui.Colors.V1.redDark
            }
        , styleStyle ErrorColors
            { background = Nri.Ui.Colors.V1.purple
            , hover = Nri.Ui.Colors.V1.purpleDark
            , text = Nri.Ui.Colors.V1.white
            , border = Nri.Ui.Colors.V1.purple
            , fontStyle = normal
            }
        ]
