module Nri.Ui.Callout.V1 exposing
    ( Attribute, callout
    , label
    , containerCss, contentCss
    , custom
    )

{-|

@docs Attribute, callout

@docs label

@docs containerCss, contentCss

@docs custom

-}

import Css
import Css.Global
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts


{-| Attributes of callouts. Use functions like [`label`](#label) and
[`containerCss`](#containerCss) to construct these.
-}
type Attribute msg
    = Label (Maybe (Html msg))
    | ContentCss (List Css.Style)
    | ContainerCss (List Css.Style)
    | Custom (Html.Attribute msg)


{-| Label the callout.

    label (Html.text "Hello, World")

-}
label : Html msg -> Attribute msg
label =
    Label << Just


{-| Customize styles for the outermost element of the callout. Use this to
adjust margin and padding around the callout (it does not have any of either by
default, but it does set `box-size: border-box` on itself).

    containerCss [ Css.marginBottom (Css.px 20) ]

These styles are applied after the default styles so you can override the
defaults when necessary without using `!important`

-}
containerCss : List Css.Style -> Attribute msg
containerCss =
    ContainerCss


{-| Customize styles for the parent element of the content you provide. Use
this to override spacing inside the callout, if necessary.

    contentCss [ Css.textTransform Css.uppercase ]

These styles are applied after the default styles so you can override the
defaults when necessary without using `!important`

-}
contentCss : List Css.Style -> Attribute msg
contentCss =
    ContentCss


{-| Add custom attributes. This is your gateway to customize (or break) the
callout however you want, so be careful! If you find yourself doing this a lot,
please consider adding another `Attribute` constructor to the module.

    custom (title "beta warning")

-}
custom : Html.Attribute msg -> Attribute msg
custom =
    Custom


type alias Attrs msg =
    { label : Maybe (Html msg)
    , containerCss : List Css.Style
    , contentCss : List Css.Style
    , customAttrs : List (Html.Attribute msg)
    }


defaultAttrs : Attrs msg
defaultAttrs =
    { label = Nothing
    , containerCss = []
    , contentCss = []
    , customAttrs = []
    }


customize : Attribute msg -> Attrs msg -> Attrs msg
customize attr attrs =
    case attr of
        Label text ->
            { attrs | label = text }

        ContentCss css ->
            { attrs | contentCss = css }

        ContainerCss css ->
            { attrs | containerCss = css }

        Custom custom_ ->
            { attrs | customAttrs = custom_ :: attrs.customAttrs }


{-| Render a callout. Use this like any other HTML node, but with specific
attribute constructors.

    callout
        [ label (Html.text "BETA") ]
        [ Html.text "This feature is still in beta. Careful of sharp edges." ]

-}
callout : List (Attribute msg) -> List (Html msg) -> Html msg
callout attrs children =
    let
        finalAttrs =
            List.foldl customize defaultAttrs attrs
    in
    Html.aside
        (css
            ([ Css.boxSizing Css.borderBox
             , Css.backgroundColor Colors.sunshine
             , Css.displayFlex
             , Css.flexDirection Css.row
             , Css.minHeight (Css.px 42)
             , Css.alignItems Css.stretch
             , Css.border3 (Css.px 1) Css.solid Colors.highlightYellow
             , Css.borderRadius (Css.px 4)
             ]
                ++ finalAttrs.containerCss
            )
            :: finalAttrs.customAttrs
        )
        [ case finalAttrs.label of
            Just text ->
                Html.div
                    [ css
                        [ -- position
                          Css.backgroundColor Colors.highlightYellow
                        , Css.color Colors.highlightYellowDark
                        , Css.padding2 Css.zero (Css.px 20)
                        , Css.displayFlex
                        , Css.alignItems Css.center

                        -- text
                        , Fonts.baseFont
                        , Css.fontWeight Css.bold
                        , Css.fontSize (Css.px 12)
                        ]
                    ]
                    [ text ]

            Nothing ->
                Html.text ""
        , Html.div
            [ css
                ([ -- position
                   Css.padding2 (Css.px 6) (Css.px 14)
                 , Css.displayFlex
                 , Css.alignItems Css.center

                 -- text
                 , Css.fontSize (Css.px 12)
                 , Css.color Colors.gray20
                 , Fonts.baseFont

                 -- children
                 , Css.Global.descendants
                    [ Css.Global.a
                        [ Css.color Colors.azure
                        , Css.textDecoration Css.none
                        ]
                    ]
                 ]
                    ++ finalAttrs.contentCss
                )
            ]
            [ Html.p [ css [ Css.margin Css.zero ] ] children ]
        ]
