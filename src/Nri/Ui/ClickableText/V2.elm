module Nri.Ui.ClickableText.V2 exposing
    ( button
    , link
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

@docs button


# `<a>` creators

@docs link


# Config

@docs Size

-}

import Css
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1
import Nri.Ui.Svg.V1 as NriSvg exposing (Svg)


{-| Sizes for the button
-}
type Size
    = Small
    | Medium
    | Large


{-| Creates a `<button>` element
-}
button :
    { label : String
    , size : Size
    , icon : Maybe Svg
    , onClick : msg
    }
    -> Html msg
button config =
    Nri.Ui.styled Html.button
        (dataDescriptor "button")
        clickableTextStyles
        [ Events.onClick config.onClick
        ]
        [ viewContent config ]


{-| Creates a `<a>` element
-}
link :
    { label : String
    , size : Size
    , icon : Maybe Svg
    , url : String
    }
    -> List (Attribute msg)
    -> Html msg
link config additionalAttributes =
    Nri.Ui.styled Html.a
        (dataDescriptor "link")
        clickableTextStyles
        (Attributes.href config.url :: additionalAttributes)
        [ viewContent config ]


viewContent : { a | label : String, size : Size, icon : Maybe Svg } -> Html msg
viewContent config =
    let
        fontSize =
            sizeToPx config.size
    in
    span [ Attributes.css [ Css.fontSize fontSize ] ]
        (case config.icon of
            Just icon ->
                [ div
                    [ Attributes.css
                        [ Css.displayFlex
                        , Css.alignItems Css.center
                        , Css.property "line-height" "normal"
                        ]
                    ]
                    [ div
                        [ Attributes.css
                            [ Css.height fontSize
                            , Css.maxWidth fontSize
                            , case config.size of
                                Small ->
                                    Css.marginRight (Css.px 2)

                                Medium ->
                                    Css.marginRight (Css.px 3)

                                Large ->
                                    Css.marginRight (Css.px 4)
                            ]
                        ]
                        [ NriSvg.toHtml icon ]
                    , span [] [ text config.label ]
                    ]
                ]

            Nothing ->
                [ text config.label ]
        )


clickableTextStyles : List Css.Style
clickableTextStyles =
    [ Css.cursor Css.pointer
    , Nri.Ui.Fonts.V1.baseFont
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
    , Css.padding Css.zero
    , Css.display Css.inlineBlock
    , Css.verticalAlign Css.textBottom
    ]


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
