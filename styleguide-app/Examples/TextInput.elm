module Examples.TextInput exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled as Html exposing (..)
import Category exposing (Category(..))
import Css exposing (..)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Dict exposing (Dict)
import Example exposing (Example)
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Message.V3 as Message
import Nri.Ui.TextInput.V7 as TextInput
import ViewHelpers exposing (viewExamples)


{-| -}
example : Example State Msg
example =
    { name = "TextInput"
    , version = 7
    , categories = [ Inputs ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view =
        \state ->
            let
                exampleConfig =
                    Control.currentValue state.control

                toExample { name, toString, inputType, onBlur, onEnter } index =
                    ( name
                    , TextInput.view exampleConfig.label
                        (exampleConfig.attributes
                            ++ [ TextInput.id ("text-input__" ++ name ++ "-example")
                               , TextInput.map toString identity (SetInput index) inputType
                               , TextInput.onInput (SetInput index)
                               , TextInput.value (Maybe.withDefault "" (Dict.get index state.inputValues))
                               ]
                            ++ List.filterMap identity
                                [ if exampleConfig.onBlur then
                                    Just (TextInput.onBlur (SetInput index onBlur))

                                  else
                                    Nothing
                                , if exampleConfig.onEnter then
                                    Just (TextInput.onEnter (SetInput index onEnter))

                                  else
                                    Nothing
                                ]
                        )
                    )
            in
            [ Control.view UpdateControl state.control
                |> Html.fromUnstyled
            , (viewExamples << List.indexedMap (\i toView -> toView i))
                [ toExample
                    { name = "text"
                    , toString = identity
                    , inputType = TextInput.text
                    , onBlur = "Blurred!!!"
                    , onEnter = "Entered!!!"
                    }
                , toExample
                    { name = "number"
                    , toString = Maybe.map String.fromInt >> Maybe.withDefault ""
                    , inputType = TextInput.number
                    , onBlur = "10000000"
                    , onEnter = "20000000"
                    }
                , toExample
                    { name = "float"
                    , toString = Maybe.map String.fromFloat >> Maybe.withDefault ""
                    , inputType = TextInput.float
                    , onBlur = "1.00000001"
                    , onEnter = "100000001.1"
                    }
                , toExample
                    { name = "password"
                    , toString = identity
                    , inputType = TextInput.password
                    , onBlur = "Blurred!!!"
                    , onEnter = "Entered!!!"
                    }
                , toExample
                    { name = "email"
                    , toString = identity
                    , inputType = TextInput.email
                    , onBlur = "Blurred!!!"
                    , onEnter = "Entered!!!"
                    }
                , toExample
                    { name = "search"
                    , toString = identity
                    , inputType = TextInput.search
                    , onBlur = "Blurred!!!"
                    , onEnter = "Entered!!!"
                    }
                ]
            ]
    }


{-| -}
type alias State =
    { inputValues : Dict Int String
    , control : Control ExampleConfig
    }


{-| -}
init : State
init =
    { inputValues = Dict.empty
    , control = initControl
    }


type alias ExampleConfig =
    { label : String
    , attributes : List (TextInput.Attribute String Msg)
    , onBlur : Bool
    , onEnter : Bool
    }


initControl : Control ExampleConfig
initControl =
    Control.record ExampleConfig
        |> Control.field "label" (Control.string "Assignment name")
        |> Control.field "attributes" controlAttributes
        |> Control.field "onBlur" (Control.bool False)
        |> Control.field "onEnter" (Control.bool False)


controlAttributes : Control (List (TextInput.Attribute value msg))
controlAttributes =
    ControlExtra.list
        |> ControlExtra.optionalListItem "placeholder"
            (Control.map TextInput.placeholder <|
                Control.string "Learning with commas"
            )
        |> ControlExtra.optionalListItem "hiddenLabel"
            (Control.value TextInput.hiddenLabel)
        |> ControlExtra.optionalListItem "errorIf"
            (Control.map TextInput.errorIf <| Control.bool True)
        |> ControlExtra.optionalListItem "errorMessage"
            (Control.map TextInput.errorMessage <| Control.maybe True <| Control.string "The statement must be true.")
        |> ControlExtra.optionalListItem "disabled"
            (Control.value TextInput.disabled)
        |> ControlExtra.optionalListItem "loading"
            (Control.value TextInput.loading)
        |> ControlExtra.optionalListItem "writing"
            (Control.value TextInput.writing)
        |> ControlExtra.listItem "noMargin"
            (Control.map TextInput.noMargin (Control.bool False))
        |> ControlExtra.optionalListItem "css"
            (Control.value (TextInput.css [ Css.backgroundColor Colors.azure ]))


{-| -}
type Msg
    = SetInput Int String
    | UpdateControl (Control ExampleConfig)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetInput id string ->
            ( { state | inputValues = Dict.insert id string state.inputValues }
            , Cmd.none
            )

        UpdateControl newControl ->
            ( { state | control = newControl }
            , Cmd.none
            )
