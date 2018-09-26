module Nri.Ui.TextInput.V1 exposing
    ( InputType
    , Model
    , styles
    , view
    , withInvisibleLabel
    , number
    , text
    )

{-|

@docs InputType
@docs Model
@docs styles
@docs view
@docs withInvisibleLabel


## Input types

@docs number
@docs text

-}

import Accessibility.Style
import Css exposing (..)
import Css.Foreign exposing (Snippet, children, descendants, everything, selector)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Styles.V1
import Regex


{-| -}
type alias Model value msg =
    { label : String
    , isInError : Bool
    , onInput : value -> msg
    , placeholder : String
    , value : value
    , autofocus : Bool
    , type_ : InputType value
    }


{-| -}
type InputType value
    = InputType
        { toString : value -> String
        , fromString : String -> value
        , fieldType : String
        }


{-| An input that allows text entry
-}
text : InputType String
text =
    InputType
        { toString = identity
        , fromString = identity
        , fieldType = "text"
        }


{-| An input that allows number entry
-}
number : InputType (Maybe Int)
number =
    InputType
        { toString = Maybe.map toString >> Maybe.withDefault ""
        , fromString = String.toInt >> Result.toMaybe
        , fieldType = "number"
        }


{-| -}
view : Model value msg -> Html msg
view model =
    view_ [ styles.class [ OverlappingLabel ] ] model


{-| -}
withInvisibleLabel : Model value msg -> Html msg
withInvisibleLabel model =
    view_ [ Accessibility.Style.invisible ] model


view_ : List (Html.Attribute msg) -> Model value msg -> Html msg
view_ attributes model =
    let
        idValue =
            "Nri-Ui-TextInput-" ++ dashify model.label

        (InputType inputType) =
            model.type_
    in
    div [ styles.classList [ ( OverlappingLabelInput, True ), ( IsInError, model.isInError ) ] ]
        [ input
            [ Html.Attributes.id idValue
            , styles.classList [ ( Input, True ), ( IsInErrorInput, model.isInError ) ]
            , placeholder model.placeholder
            , defaultValue (inputType.toString model.value)
            , onInput (inputType.fromString >> model.onInput)
            , autofocus model.autofocus
            , type_ inputType.fieldType
            ]
            []
        , label
            (for idValue :: attributes)
            [ Html.text model.label ]
        ]


type CssClasses
    = OverlappingLabel
    | Input
    | OverlappingLabelInput
    | IsInError
    | IsInErrorInput


{-| -}
styles : Nri.Ui.Styles.V1.Styles Never CssClasses msg
styles =
    let
        inputStyle =
            [ border3 (px 1) solid gray75
            , borderRadius (px 8)
            , padding2 (px 8) (px 14)
            , Css.height (px 45)
            , Css.width (pct 100)
            , Css.property "transition" "all 0.1s ease"
            , focus
                [ borderColor azure
                , outline none
                , boxShadow none
                ]
            , Css.pseudoClass "placeholder"
                [ Css.color gray45
                ]
            , Css.fontSize (px 15)
            , Fonts.baseFont
            , Css.color gray20

            -- fix bootstrap
            , Css.display inlineBlock
            , Css.marginBottom zero
            , Css.lineHeight (px 20)
            , Css.marginTop (px 9)
            ]
    in
    Nri.Ui.Styles.V1.styles "Nri-Ui-TextInput-"
        [ Css.Foreign.selector "input[type=text]"
            [ Css.Foreign.withClass Input inputStyle ]
        , Css.Foreign.selector "input[type=number]"
            [ Css.Foreign.withClass Input inputStyle ]
        , Css.Foreign.class OverlappingLabel
            [ Css.backgroundColor Nri.Ui.Colors.V1.white
            , Css.left (Css.px 10)
            , Css.top (Css.px 0)
            , Css.padding2 zero (Css.px 5)
            , Css.fontSize (Css.px 12)
            , Fonts.baseFont
            , Css.color navy
            , Css.position Css.absolute
            , Css.fontWeight (int 600)
            ]
        , Css.Foreign.class OverlappingLabelInput
            [ Css.position Css.relative
            ]
        , Css.Foreign.class IsInError
            [ Css.Foreign.children
                [ Css.Foreign.class Input
                    [ Css.Foreign.withClass IsInErrorInput
                        [ borderColor purple
                        , focus [ borderColor purple ]
                        ]
                    ]
                , Css.Foreign.class OverlappingLabel
                    [ color purple
                    ]
                ]
            ]
        ]


{-| Convenience method for going from a string with spaces to a string with dashes.
-}
dashify : String -> String
dashify =
    Regex.replace Regex.All (Regex.regex " ") (always "-")
