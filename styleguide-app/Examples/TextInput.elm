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
                , TextInput.view (exampleConfig.label ++ " (text)")
                    (TextInput.text (SetTextInput 1))
                    ([ TextInput.errorIf exampleConfig.isInError
                     , TextInput.placeholder exampleConfig.placeholder
                     ]
                        ++ (if exampleConfig.showLabel then
                                []

                            else
                                [ TextInput.hiddenLabel ]
                           )
                    )
                    (Maybe.withDefault "" <| Dict.get 1 state.stringInputValues)
                , Heading.h3 [] [ text "... type_ = TextInput.number" ]
                , TextInput.view (exampleConfig.label ++ " (number)")
                    (TextInput.number SetNumberInput)
                    ([ TextInput.errorIf exampleConfig.isInError
                     , TextInput.placeholder exampleConfig.placeholder
                     ]
                        ++ (if exampleConfig.showLabel then
                                []

                            else
                                [ TextInput.hiddenLabel ]
                           )
                    )
                    state.numberInputValue
                , Heading.h3 [] [ text "... type_ = TextInput.float" ]
                , TextInput.view (exampleConfig.label ++ " (float)")
                    (TextInput.float SetFloatInput)
                    ([ TextInput.errorIf exampleConfig.isInError
                     , TextInput.placeholder exampleConfig.placeholder
                     ]
                        ++ (if exampleConfig.showLabel then
                                []

                            else
                                [ TextInput.hiddenLabel ]
                           )
                    )
                    state.floatInputValue
                , Heading.h3 [] [ text "... type_ = TextInput.password" ]
                , TextInput.view (exampleConfig.label ++ " (password)")
                    (TextInput.password SetPassword)
                    ([ TextInput.errorIf exampleConfig.isInError
                     , TextInput.placeholder exampleConfig.placeholder
                     ]
                        ++ (if exampleConfig.showLabel then
                                []

                            else
                                [ TextInput.hiddenLabel ]
                           )
                    )
                    state.passwordInputValue
                , Heading.h3 [] [ text "... type_ = TextInput.email" ]
                , TextInput.view (exampleConfig.label ++ " (email)")
                    (TextInput.email (SetTextInput 2))
                    ([ TextInput.errorIf exampleConfig.isInError
                     , TextInput.placeholder exampleConfig.placeholder
                     ]
                        ++ (if exampleConfig.showLabel then
                                []

                            else
                                [ TextInput.hiddenLabel ]
                           )
                    )
                    (Maybe.withDefault "" <| Dict.get 2 state.stringInputValues)
                , Heading.h3 [] [ Html.text "TextInput.writing { type_ = TextInput.text }" ]
                , TextInput.view (exampleConfig.label ++ " (writing)")
                    (TextInput.text (SetTextInput 4))
                    ([ TextInput.writing
                     , TextInput.errorIf exampleConfig.isInError
                     , TextInput.placeholder exampleConfig.placeholder
                     ]
                        ++ (if exampleConfig.showLabel then
                                []

                            else
                                [ TextInput.hiddenLabel ]
                           )
                    )
                    (Maybe.withDefault "" <| Dict.get 4 state.stringInputValues)
                , Heading.h3 [] [ text "onBlur demonstration" ]
                , TextInput.view (exampleConfig.label ++ " (onBlur)")
                    (TextInput.text (SetTextInput 7))
                    ([ TextInput.writing
                     , TextInput.errorIf exampleConfig.isInError
                     , TextInput.placeholder exampleConfig.placeholder
                     , TextInput.onBlur (SetTextInput 7 "Blurred!")
                     ]
                        ++ (if exampleConfig.showLabel then
                                []

                            else
                                [ TextInput.hiddenLabel ]
                           )
                    )
                    (Maybe.withDefault "" <| Dict.get 7 state.stringInputValues)
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
