module Nri.Ui.ClickableText.V4 exposing
    ( button
    , link
    , Attribute
    , small, medium, large
    , href, onClick
    , icon
    , custom, css
    )

{-|


# Changes from V3

  - adds `css` helper


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


# Attributes

@docs Attribute
@docs small, medium, large
@docs href, onClick
@docs icon
@docs custom, css

-}

import Css exposing (Style)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.Svg.V1 as NriSvg exposing (Svg)


label : String -> Attribute msg
label label_ =
    set (\attributes -> { attributes | label = label_ })


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


type Size
    = Small
    | Medium
    | Large


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
css : List Style -> Attribute msg
css styles =
    set
        (\config ->
            { config
                | customStyles = List.append config.customStyles styles
            }
        )


{-| -}
onClick : msg -> Attribute msg
onClick msg =
    set (\attributes -> { attributes | onClick = Just msg })


{-| -}
href : String -> Attribute msg
href url =
    set (\attributes -> { attributes | url = url })


{-| Creates a `<button>` element
-}
button :
    String
    -> List (Attribute msg)
    -> Html msg
button label_ attributes =
    let
        config =
            (label label_ :: attributes)
                |> List.foldl (\(Attribute attribute) b -> attribute b) defaults
    in
    Nri.Ui.styled Html.button
        (dataDescriptor "button")
        (clickableTextStyles ++ config.customStyles)
        ((Maybe.map Events.onClick config.onClick
            |> Maybe.withDefault AttributesExtra.none
         )
            :: config.customAttributes
        )
        [ viewContent config ]


{-| Creates a `<a>` element
-}
link :
    String
    -> List (Attribute msg)
    -> Html msg
link label_ attributes =
    let
        config =
            (label label_ :: attributes)
                |> List.foldl (\(Attribute attribute) l -> attribute l) defaults
    in
    Nri.Ui.styled Html.a
        (dataDescriptor "link")
        (clickableTextStyles ++ config.customStyles)
        (Attributes.href config.url :: config.customAttributes)
        [ viewContent config ]


viewContent : { a | label : String, size : Size, icon : Maybe Svg } -> Html msg
viewContent config =
    let
        fontSize =
            sizeToPx config.size
    in
    span [ Attributes.css [ Css.fontSize fontSize ] ]
        (case config.icon of
            Just icon_ ->
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
                            , Css.minWidth fontSize -- so it doesn't shrink when the label is long
                            , case config.size of
                                Small ->
                                    Css.marginRight (Css.px 3)

                                Medium ->
                                    Css.marginRight (Css.px 3)

                                Large ->
                                    Css.marginRight (Css.px 4)
                            ]
                        ]
                        [ NriSvg.toHtml icon_ ]
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



--  Internals


type alias ClickableTextAttributes msg =
    { label : String
    , size : Size
    , icon : Maybe Svg
    , onClick : Maybe msg
    , url : String
    , customAttributes : List (Html.Attribute msg)
    , customStyles : List Style
    }


defaults : ClickableTextAttributes msg
defaults =
    { onClick = Nothing
    , url = "#"
    , size = Medium
    , label = ""
    , icon = Nothing
    , customAttributes = []
    , customStyles = []
    }


{-| -}
type Attribute msg
    = Attribute (ClickableTextAttributes msg -> ClickableTextAttributes msg)


set :
    (ClickableTextAttributes msg -> ClickableTextAttributes msg)
    -> Attribute msg
set with =
    Attribute (\config -> with config)
