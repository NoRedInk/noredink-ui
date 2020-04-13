module Nri.Ui.TextInput.V6 exposing
    ( Model
    , view, writing
    , generateId
    , InputType, number, float, text, password, email
    )

{-|


# Changes from V5

  - nothing yet

@docs Model
@docs view, writing
@docs generateId


## Input types

@docs InputType, number, float, text, password, email

-}

import Accessibility.Styled.Style as Accessibility
import Css exposing (center, position, px, relative, textAlign)
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
        , inputMode : Maybe String
        , autocomplete : Maybe String
        }


{-| An input that allows text entry
-}
text : InputType String
text =
    InputType
        { toString = identity
        , fromString = identity
        , fieldType = "text"
        , inputMode = Nothing
        , autocomplete = Nothing
        }


{-| An input that allows integer entry
-}
number : InputType (Maybe Int)
number =
    InputType
        { toString = Maybe.map String.fromInt >> Maybe.withDefault ""
        , fromString = String.toInt
        , fieldType = "number"
        , inputMode = Nothing
        , autocomplete = Nothing
        }


{-| An input that allows float entry
-}
float : InputType (Maybe Float)
float =
    InputType
        { toString = Maybe.map String.fromFloat >> Maybe.withDefault ""
        , fromString = String.toFloat
        , fieldType = "number"
        , inputMode = Nothing
        , autocomplete = Nothing
        }


{-| An input that allows password entry
-}
password : InputType String
password =
    InputType
        { toString = identity
        , fromString = identity
        , fieldType = "password"
        , inputMode = Nothing
        , autocomplete = Just "current-password"
        }


{-| An input that is optimized for email entry

NOTE: this uses `inputmode="email"` so that mobile devices will use the email keyboard,
but not `type="email"` because that would enable browser-provided validation which is inconsistent and at odds
with our validation UI.

-}
email : InputType String
email =
    InputType
        { toString = identity
        , fromString = identity
        , fieldType = "text"
        , inputMode = Just "email"
        , autocomplete = Just "email"
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

        maybeStep =
            if inputType.fieldType == "number" then
                [ step "any" ]

            else
                []

        maybeAttr attr maybeValue =
            maybeValue
                |> Maybe.map attr
                |> Maybe.withDefault Extra.none
    in
    div
        [ Attributes.css [ position relative ]
        ]
        [ input
            (maybeStep
                ++ [ Attributes.id idValue
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
                   , maybeAttr Events.onBlur model.onBlur
                   , autofocus model.autofocus
                   , type_ inputType.fieldType
                   , maybeAttr (attribute "inputmode") inputType.inputMode
                   , maybeAttr (attribute "autocomplete") inputType.autocomplete
                   , class "override-sass-styles"
                   , Attributes.attribute "aria-invalid" <|
                        if model.isInError then
                            "true"

                        else
                            "false"
                   ]
            )
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
