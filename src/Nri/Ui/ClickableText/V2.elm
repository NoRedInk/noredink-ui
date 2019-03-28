module Nri.Ui.ClickableText.V2 exposing
    ( ButtonConfig, button
    , LinkConfig, link
    , Size(..)
    )

{-|


# Changes from V1

  - Removes dependency on Icon that makes versioned assets hard to work with
  - Fixes vertical alignment of icons
  - Inlines configs to make parsing documentation easier


# About:

ClickableText looks different from Nri.Ui.Button in that it displays without margin or padding.
ClickableText has the suave, traditional look of a "link"!

For accessibility purposes, buttons that perform an action on the current page should be HTML `<button>`
elements and are created here with `*Button` functions. Buttons that take the user to a new page should be
HTML `<a>` elements and are created here with `*Link` functions.


# `<button>` creators

@docs ButtonConfig, button


# `<a>` creators

@docs LinkConfig, link


# Config

@docs Size

-}

import Css
import Css.Global
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1
import Nri.Ui.Icon.V4 as Icon exposing (IconType)


{-| Sizes for the button
-}
type Size
    = Small
    | Medium
    | Large


{-| Config for the button
-}
type alias ButtonConfig msg =
    { label : String
    , size : Size
    , icon : Maybe IconType
    , onClick : msg
    }


{-| Creates a `<button>` element
-}
button : ButtonConfig msg -> Html msg
button config =
    let
        fontSize =
            sizeToPx config.size
    in
    Nri.Ui.styled Html.button
        (dataDescriptor "button")
        (clickableTextStyles fontSize)
        [ Events.onClick config.onClick
        ]
        [ icon fontSize config.icon
        , text config.label
        ]


{-| Config for the link
-}
type alias LinkConfig =
    { label : String
    , size : Size
    , icon : Maybe IconType
    , url : String
    }


{-| Creates a `<a>` element
-}
link : LinkConfig -> List (Attribute msg) -> Html msg
link config additionalAttributes =
    let
        fontSize =
            sizeToPx config.size
    in
    Nri.Ui.styled Html.a
        (dataDescriptor "link")
        (clickableTextStyles fontSize)
        (Attributes.href config.url
            :: additionalAttributes
        )
        [ icon fontSize config.icon
        , text config.label
        ]


clickableTextStyles : Css.Px -> List Css.Style
clickableTextStyles fontSize =
    [ Css.cursor Css.pointer
    , -- Specifying the font can and should go away after bootstrap is removed from application.css
      Nri.Ui.Fonts.V1.baseFont
    , Css.backgroundImage Css.none
    , Css.textShadow Css.none
    , Css.boxShadow Css.none
    , Css.border Css.zero
    , Css.disabled [ Css.cursor Css.notAllowed ]
    , Css.color Colors.azure
    , Css.backgroundColor Css.transparent
    , Css.fontWeight (Css.int 600)
    , Css.textAlign Css.left
    , Css.borderStyle Css.none
    , Css.textDecoration Css.none
    , Css.hover [ Css.textDecoration Css.underline ]
    , Css.fontSize fontSize
    , Css.padding Css.zero
    ]


icon : Css.Px -> Maybe IconType -> Html msg
icon fontSize maybeIcon =
    case maybeIcon of
        Just iconType ->
            -- TODO: We should never use an image here, only SVG
            Nri.Ui.styled Html.span
                (dataDescriptor "icon-holder")
                [ Css.height fontSize
                , Css.width fontSize
                , Css.display Css.inlineBlock
                , Css.marginRight (Css.px 5)
                ]
                []
                [ Icon.decorativeIcon iconType ]

        Nothing ->
            text ""


sizeToPx : Size -> Css.Px
sizeToPx size =
    case size of
        Small ->
            Css.px 15

        Medium ->
            Css.px 17

        Large ->
            Css.px 20


dataDescriptor : String -> String
dataDescriptor descriptor =
    "clickable-text-v2-" ++ descriptor
