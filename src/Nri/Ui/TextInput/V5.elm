module Nri.Ui.TextInput.V5 exposing
    ( Model
    , view, writing
    , generateId
    , number
    , text
    )

{-|


# Changes from V4

  - adds a password input

@docs Model
@docs view, writing
@docs generateId


## Input types

@docs number
@docs text

-}

import Accessibility.Styled.Style as Accessibility
import Css exposing (batch, center, position, px, relative, textAlign)
import Css.Global
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events as Events exposing (onInput)
import Nri.Ui.Html.Attributes.V2 as Extra
import Nri.Ui.InputStyles.V2 as InputStyles exposing (Theme)
import Nri.Ui.Util exposing (dashify)


{-| -}
type alias Model value msg =
    { label : String
    , isInError : Bool
    , onInput : value -> msg
    , onBlur : Maybe msg
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
        { toString = Maybe.map String.fromInt >> Maybe.withDefault ""
        , fromString = String.toInt
        , fieldType = "number"
        }


{-| -}
view : Model value msg -> Html msg
view model =
    view_ InputStyles.Standard model


{-| -}
writing : Model value msg -> Html msg
writing model =
    view_ InputStyles.Writing model


view_ : Theme -> Model value msg -> Html msg
view_ theme model =
    let
        idValue =
            generateId model.label

        (InputType inputType) =
            model.type_
    in
    div
        [ Attributes.css [ position relative ]
        ]
        [ input
            [ Attributes.id idValue
            , css
                [ InputStyles.input theme model.isInError
                , if theme == InputStyles.Writing then
                    Css.Global.withClass "override-sass-styles"
                        [ textAlign center
                        , Css.height Css.auto
                        ]

                  else
                    Css.Global.withClass "override-sass-styles"
                        [ Css.height (px 45)
                        ]
                ]
            , placeholder model.placeholder
            , value (inputType.toString model.value)
            , onInput (inputType.fromString >> model.onInput)
            , Maybe.withDefault Extra.none (Maybe.map Events.onBlur model.onBlur)
            , autofocus model.autofocus
            , type_ inputType.fieldType
            , class "override-sass-styles"
            , Attributes.attribute "aria-invalid" <|
                if model.isInError then
                    "true"

                else
                    "false"
            ]
            []
        , if model.showLabel then
            Html.label
                [ for idValue
                , css [ InputStyles.label theme model.isInError ]
                ]
                [ Html.text model.label ]

          else
            Html.label
                ([ for idValue
                 , css [ InputStyles.label theme model.isInError ]
                 ]
                    ++ Accessibility.invisible
                )
                [ Html.text model.label ]
        ]


{-| Gives you the DOM element id that will be used by a `TextInput.view` with the given label.
This is for use when you need the DOM element id for use in javascript (such as trigger an event to focus a particular text input)
-}
generateId : String -> String
generateId labelText =
    "Nri-Ui-TextInput-" ++ dashify labelText
