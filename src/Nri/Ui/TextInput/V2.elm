module Nri.Ui.TextInput.V2
    exposing
        ( InputType
        , Model
        , number
        , text
        , view
        , writing
        )

{-|

@docs InputType
@docs Model
@docs view, writing


## Input types

@docs number
@docs text

-}

import Accessibility.Style
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Nri.Ui.InputStyles exposing (Assets, CssClasses(..), styles)
import Regex


{-| -}
type alias Model value msg =
    { label : String
    , isInError : Bool
    , onInput : value -> msg
    , placeholder : String
    , value : value
    , autofocus : Bool
    , showLabel : Bool
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


type TextInputStyle
    = DefaultStyle
    | WritingStyle


{-| -}
view : Model value msg -> Html msg
view model =
    view_ DefaultStyle model


{-| -}
writing : Model value msg -> Html msg
writing model =
    view_ WritingStyle model


view_ : TextInputStyle -> Model value msg -> Html msg
view_ inputStyle model =
    let
        idValue =
            "Nri-Ui-TextInput-" ++ dashify model.label

        showWritingClass =
            inputStyle == WritingStyle

        (InputType inputType) =
            model.type_
    in
    div
        [ styles.classList
            [ ( Container, True )
            , ( IsInError, model.isInError )
            , ( Writing, showWritingClass )
            ]
        ]
        [ input
            [ Html.Attributes.id idValue
            , styles.class [ Input ]
            , placeholder model.placeholder
            , defaultValue (inputType.toString model.value)
            , onInput (inputType.fromString >> model.onInput)
            , autofocus model.autofocus
            , type_ inputType.fieldType
            ]
            []
        , if not model.showLabel then
            Html.label
                [ for idValue
                , styles.class [ Label ]
                , Accessibility.Style.invisible
                ]
                [ Html.text model.label ]
          else
            Html.label
                [ for idValue
                , styles.class [ Label ]
                ]
                [ Html.text model.label ]
        ]


{-| Convenience method for going from a string with spaces to a string with dashes.
-}
dashify : String -> String
dashify =
    Regex.replace Regex.All (Regex.regex " ") (always "-")
