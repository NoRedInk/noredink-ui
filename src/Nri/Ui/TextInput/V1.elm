module Nri.Ui.TextInput.V1
    exposing
        ( InputType
        , Model
        , number
        , styles
        , text
        , view
        , withInvisibleLabel
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

import Css exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Nri.Accessibility
import Nri.Colors exposing (..)
import Nri.Stylers
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
    view_ OverlappingLabel model


{-| -}
withInvisibleLabel : Model value msg -> Html msg
withInvisibleLabel model =
    view_ InvisibleLabel model


view_ : CssClasses -> Model value msg -> Html msg
view_ labelClass model =
    let
        idValue =
            "Nri-TextInput-" ++ dashify model.label

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
            [ for idValue
            , styles.class [ labelClass ]
            ]
            [ Html.text model.label ]
        ]


type CssClasses
    = InvisibleLabel
    | OverlappingLabel
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
            , Nri.Stylers.makeFont (px 15) gray20

            -- fix bootstrap
            , Css.display inlineBlock
            , Css.marginBottom zero
            , Css.lineHeight (px 20)
            , Css.marginTop (px 9)
            ]
    in
    Nri.Ui.Styles.V1.styles "Nri-TextInput-"
        [ Css.selector "input[type=text]"
            [ Css.withClass Input inputStyle ]
        , Css.selector "input[type=number]"
            [ Css.withClass Input inputStyle ]
        , Css.class InvisibleLabel
            [ Nri.Accessibility.invisibleText
            ]
        , Css.class OverlappingLabel
            [ Css.backgroundColor Nri.Colors.white
            , Css.left (Css.px 10)
            , Css.top (Css.px 0)
            , Css.padding2 zero (Css.px 5)
            , Nri.Stylers.makeFont (Css.px 12) navy
            , Css.position Css.absolute
            , Css.fontWeight (int 600)
            ]
        , Css.class OverlappingLabelInput
            [ Css.position Css.relative
            ]
        , Css.class IsInError
            [ Css.children
                [ Css.class Input
                    [ Css.withClass IsInErrorInput
                        [ borderColor purple
                        , focus [ borderColor purple ]
                        ]
                    ]
                , Css.class OverlappingLabel
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
