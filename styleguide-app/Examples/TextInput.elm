module Examples.TextInput exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled as Html exposing (..)
import Category exposing (Category(..))
import Debug.Control as Control exposing (Control)
import Dict exposing (Dict)
import Example exposing (Example)
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.TextInput.V6 as TextInput


{-| -}
type Msg
    = SetTextInput Id String
    | SetNumberInput (Maybe Int)
    | SetFloatInput (Maybe Float)
    | SetPassword String
    | UpdateControl (Control ExampleConfig)


{-| -}
type alias State =
    { numberInputValue : Maybe Int
    , floatInputValue : Maybe Float
    , stringInputValues : Dict Id String
    , passwordInputValue : String
    , control : Control ExampleConfig
    }


type alias ExampleConfig =
    { showLabel : Bool
    , label : String
    , isInError : Bool
    , placeholder : String
    }


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.TextInput.V6"
    , categories = [ Inputs ]
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view =
        \state ->
            let
                exampleConfig =
                    Control.currentValue state.control
            in
            [ Html.div []
                [ Control.view UpdateControl state.control
                    |> Html.fromUnstyled
                , Heading.h3 [] [ text "TextInput.view { type_ = TextInput.text }" ]
                , TextInput.view
                    { label = exampleConfig.label ++ " (text)"
                    , isInError = exampleConfig.isInError
                    , placeholder = exampleConfig.placeholder
                    , showLabel = exampleConfig.showLabel
                    , value = Maybe.withDefault "" <| Dict.get 1 state.stringInputValues
                    , onInput = SetTextInput 1
                    , onBlur = Nothing
                    , autofocus = False
                    , type_ = TextInput.text
                    }
                , Heading.h3 [] [ text "... type_ = TextInput.number" ]
                , TextInput.view
                    { label = exampleConfig.label ++ " (number)"
                    , isInError = exampleConfig.isInError
                    , placeholder = exampleConfig.placeholder
                    , showLabel = exampleConfig.showLabel
                    , value = state.numberInputValue
                    , onInput = SetNumberInput
                    , onBlur = Nothing
                    , autofocus = False
                    , type_ = TextInput.number
                    }
                , Heading.h3 [] [ text "... type_ = TextInput.float" ]
                , TextInput.view
                    { label = exampleConfig.label ++ " (float)"
                    , isInError = exampleConfig.isInError
                    , placeholder = exampleConfig.placeholder
                    , showLabel = exampleConfig.showLabel
                    , value = state.floatInputValue
                    , onInput = SetFloatInput
                    , onBlur = Nothing
                    , autofocus = False
                    , type_ = TextInput.float
                    }
                , Heading.h3 [] [ text "... type_ = TextInput.password" ]
                , TextInput.view
                    { label = exampleConfig.label ++ " (password)"
                    , isInError = exampleConfig.isInError
                    , placeholder = exampleConfig.placeholder
                    , showLabel = exampleConfig.showLabel
                    , value = state.passwordInputValue
                    , onInput = SetPassword
                    , onBlur = Nothing
                    , autofocus = False
                    , type_ = TextInput.password
                    }
                , Heading.h3 [] [ text "... type_ = TextInput.email" ]
                , TextInput.view
                    { label = exampleConfig.label ++ " (email)"
                    , isInError = exampleConfig.isInError
                    , placeholder = exampleConfig.placeholder
                    , showLabel = exampleConfig.showLabel
                    , value = Maybe.withDefault "" <| Dict.get 2 state.stringInputValues
                    , onInput = SetTextInput 2
                    , onBlur = Nothing
                    , autofocus = False
                    , type_ = TextInput.email
                    }
                , Heading.h3 [] [ Html.text "TextInput.writing { type_ = TextInput.text }" ]
                , TextInput.writing
                    { label = exampleConfig.label ++ " (writing)"
                    , isInError = exampleConfig.isInError
                    , placeholder = exampleConfig.placeholder
                    , value = Maybe.withDefault "" <| Dict.get 4 state.stringInputValues
                    , onInput = SetTextInput 4
                    , onBlur = Nothing
                    , autofocus = False
                    , type_ = TextInput.text
                    , showLabel = exampleConfig.showLabel
                    }
                , Heading.h3 [] [ text "onBlur demonstration" ]
                , TextInput.writing
                    { label = exampleConfig.label ++ " (onBlur)"
                    , isInError = exampleConfig.isInError
                    , placeholder = exampleConfig.placeholder
                    , value = Maybe.withDefault "" <| Dict.get 7 state.stringInputValues
                    , onInput = SetTextInput 7
                    , onBlur = Just (SetTextInput 7 "Blurred!")
                    , autofocus = False
                    , type_ = TextInput.text
                    , showLabel = exampleConfig.showLabel
                    }
                ]
            ]
    }


{-| -}
init : State
init =
    { numberInputValue = Nothing
    , floatInputValue = Nothing
    , stringInputValues = Dict.empty
    , passwordInputValue = ""
    , control =
        Control.record ExampleConfig
            |> Control.field "showLabel" (Control.bool True)
            |> Control.field "label" (Control.string "Assignment name")
            |> Control.field "isInError" (Control.bool False)
            |> Control.field "placeholder" (Control.string "Learning with commas")
    }


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetTextInput id textInputValue ->
            ( { state | stringInputValues = Dict.insert id textInputValue state.stringInputValues }, Cmd.none )

        SetNumberInput numberInputValue ->
            ( { state | numberInputValue = numberInputValue }, Cmd.none )

        SetFloatInput floatInputValue ->
            ( { state | floatInputValue = floatInputValue }, Cmd.none )

        SetPassword password ->
            ( { state | passwordInputValue = password }, Cmd.none )

        UpdateControl newControl ->
            ( { state | control = newControl }, Cmd.none )



-- INTERNAL


type alias Id =
    Int
