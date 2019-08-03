module Examples.TextInput exposing (Msg, State, example, init, update)

{-|

@docs Msg, State, example, init, update

-}

import Dict exposing (Dict)
import Html.Styled as Html
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.TextInput.V5 as TextInput


{-| -}
type Msg
    = SetTextInput Id String
    | SetNumberInput (Maybe Int)


{-| -}
type alias State =
    { numberInputValue : Maybe Int
    , textInputValues : Dict Id String
    }


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { name = "Nri.Ui.TextInput.V4"
    , category = Inputs
    , content =
        [ Html.map parentMessage <|
            Html.div []
                [ TextInput.view
                    { label = "Criterion"
                    , isInError = False
                    , placeholder = "For example, \"Something!!\""
                    , value = Maybe.withDefault "" <| Dict.get 1 state.textInputValues
                    , onInput = SetTextInput 1
                    , onBlur = Nothing
                    , autofocus = False
                    , type_ = TextInput.text
                    , showLabel = True
                    }
                , Html.br [] []
                , TextInput.view
                    { label = "Points"
                    , isInError = False
                    , placeholder = "enter a number"
                    , value = state.numberInputValue
                    , onInput = SetNumberInput
                    , onBlur = Nothing
                    , autofocus = False
                    , type_ = TextInput.number
                    , showLabel = True
                    }
                , Html.br [] []
                , TextInput.view
                    { label = "Error"
                    , isInError = True
                    , placeholder = "Wrong!"
                    , value = state.numberInputValue
                    , onInput = SetNumberInput
                    , onBlur = Nothing
                    , autofocus = False
                    , type_ = TextInput.number
                    , showLabel = True
                    }
                , Heading.h3 [] [ Html.text "invisible label" ]
                , TextInput.view
                    { label = "Criterion"
                    , isInError = False
                    , placeholder = "For example, \"Something!!\""
                    , value = Maybe.withDefault "" <| Dict.get 2 state.textInputValues
                    , onInput = SetTextInput 2
                    , onBlur = Nothing
                    , autofocus = False
                    , type_ = TextInput.text
                    , showLabel = False
                    }
                , Html.br [] []
                , TextInput.view
                    { label = "Criterion"
                    , placeholder = "Everything you type is wrong!"
                    , value = Maybe.withDefault "" <| Dict.get 3 state.textInputValues
                    , onInput = SetTextInput 3
                    , onBlur = Nothing
                    , isInError = True
                    , autofocus = False
                    , type_ = TextInput.text
                    , showLabel = False
                    }
                , Heading.h3 [] [ Html.text "Writing Style" ]
                , TextInput.writing
                    { label = "Writing!"
                    , isInError = False
                    , placeholder = "Writing is good for me and my family"
                    , value = Maybe.withDefault "" <| Dict.get 4 state.textInputValues
                    , onInput = SetTextInput 4
                    , onBlur = Nothing
                    , autofocus = False
                    , type_ = TextInput.text
                    , showLabel = True
                    }
                , Html.br [] []
                , TextInput.writing
                    { label = "Writing with errors!"
                    , isInError = True
                    , placeholder = "Oopsie!"
                    , value = Maybe.withDefault "" <| Dict.get 5 state.textInputValues
                    , onInput = SetTextInput 5
                    , onBlur = Nothing
                    , autofocus = False
                    , type_ = TextInput.text
                    , showLabel = True
                    }
                , Html.br [] []
                , TextInput.writing
                    { label = "Writing without labels!"
                    , isInError = False
                    , placeholder = "No label on this writing input!"
                    , value = Maybe.withDefault "" <| Dict.get 6 state.textInputValues
                    , onInput = SetTextInput 6
                    , onBlur = Nothing
                    , autofocus = False
                    , type_ = TextInput.text
                    , showLabel = False
                    }
                , Html.br [] []
                , TextInput.writing
                    { label = "Writing onBlur demonstration!"
                    , isInError = False
                    , placeholder = "Click away to blur!"
                    , value = Maybe.withDefault "" <| Dict.get 7 state.textInputValues
                    , onInput = SetTextInput 7
                    , onBlur = Just (SetTextInput 7 "Blurred!")
                    , autofocus = False
                    , type_ = TextInput.text
                    , showLabel = True
                    }
                ]
        ]
    }


{-| -}
init : State
init =
    { numberInputValue = Nothing
    , textInputValues = Dict.empty
    }


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetTextInput id textInputValue ->
            ( { state | textInputValues = Dict.insert id textInputValue state.textInputValues }, Cmd.none )

        SetNumberInput numberInputValue ->
            ( { state | numberInputValue = numberInputValue }, Cmd.none )



-- INTERNAL


type alias Id =
    Int
