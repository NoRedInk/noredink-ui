module Examples.TextInput exposing (Msg, State, example, init, update)

{- \
   @docs Msg, State, example, init, update,
-}

import Dict exposing (Dict)
import Html
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.TextInput.V1 as TextInput


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
    { filename = "Nri.Ui.TextInput.V1"
    , category = Inputs
    , content =
        [ TextInput.view
            { label = "Criterion"
            , isInError = False
            , placeholder = "For example, \"Something!!\""
            , value = Maybe.withDefault "" <| Dict.get 1 state.textInputValues
            , onInput = SetTextInput 1
            , autofocus = False
            , type_ = TextInput.text
            }
        , TextInput.view
            { label = "Points"
            , isInError = False
            , placeholder = "enter a number"
            , value = state.numberInputValue
            , onInput = SetNumberInput
            , autofocus = False
            , type_ = TextInput.number
            }
        , TextInput.view
            { label = "Error"
            , isInError = True
            , placeholder = "Wrong!"
            , value = state.numberInputValue
            , onInput = SetNumberInput
            , autofocus = False
            , type_ = TextInput.number
            }
        , Html.h3 [] [ Html.text "invisible label" ]
        , TextInput.withInvisibleLabel
            { label = "Criterion"
            , isInError = False
            , placeholder = "For example, \"Something!!\""
            , value = Maybe.withDefault "" <| Dict.get 2 state.textInputValues
            , onInput = SetTextInput 2
            , autofocus = False
            , type_ = TextInput.text
            }
        , Html.br [] []
        , TextInput.withInvisibleLabel
            { label = "Criterion"
            , placeholder = "Everything you type is wrong!"
            , value = Maybe.withDefault "" <| Dict.get 3 state.textInputValues
            , onInput = SetTextInput 3
            , isInError = True
            , autofocus = False
            , type_ = TextInput.text
            }
        ]
            |> List.map (Html.map parentMessage)
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
