module Nri.TextAreaWithOverlappingLabel exposing (styles, view)

{-|


## The Nri styleguide-specified textarea with overlapping label

@docs view, styles

-}

import Css exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Nri.Accessibility exposing (invisibleText)
import Nri.Colors exposing (..)
import Nri.Colors.Extra exposing (withAlpha)
import Nri.Stylers
import Nri.Styles


type alias Model msg =
    { value : String
    , autofocus : Bool
    , onInput : String -> msg
    , isInError : Bool
    , placeholder : String
    , label : String
    , showLabel : Bool
    , id : String
    }


{-| -}
view : Model msg -> Html msg
view model =
    div
        [ styles.classList
            [ ( Container, True )
            ]
        ]
        [ Html.textarea
            [ onInput model.onInput
            , Html.Attributes.id model.id
            , styles.classList
                [ ( IsInError, model.isInError )
                , ( TextArea, True )
                ]
            , autofocus model.autofocus
            , placeholder model.placeholder
            , attribute "data-gramm" "false" -- disables grammarly to prevent https://github.com/NoRedInk/NoRedInk/issues/14859
            ]
            [ Html.text model.value ]
        , Html.label
            [ for model.id
            , styles.classList
                [ ( InvisibleLabel, not model.showLabel )
                , ( Label, True )
                ]
            ]
            [ Html.text model.label ]
        ]


type CssClass
    = InvisibleLabel
    | Label
    | Container
    | IsInError
    | TextArea


{-| -}
styles : Nri.Styles.Styles Never CssClass msg
styles =
    Nri.Styles.styles "Nri-Textarea-"
        [ Css.class Container
            [ position relative
            , paddingTop (px 7)
            ]
        , Css.class InvisibleLabel
            [ Nri.Accessibility.invisibleText
            ]
        , Css.class Label
            [ backgroundColor Nri.Colors.white
            , left (px 10)
            , top (px 0)
            , padding2 (px 2) (px 5)
            , position absolute
            , Nri.Stylers.makeFont (Css.px 11) Nri.Colors.gray45
            , border3 (px 1) solid Nri.Colors.gray75
            , borderRadius (px 3)
            ]
        , Css.class TextArea
            [ boxShadow5 inset zero (px 1) (px 1) (withAlpha 0.2 gray20)
            , position relative
            , resize vertical
            , Nri.Stylers.makeFont (Css.px 13) Nri.Colors.gray20
            , border3 (px 1) solid Nri.Colors.gray75
            , borderRadius (px 8)
            , padding2 (px 12) (px 15)
            , Css.height (px 100)
            , Css.width (pct 100)
            , Css.property "transition" "all 0.1s ease"
            , focus
                [ borderColor Nri.Colors.turquoise -- TODO: swap for new styleguide color
                , outline none
                ]
            , Css.withClass IsInError
                [ borderColor purple
                ]
            ]
        ]
