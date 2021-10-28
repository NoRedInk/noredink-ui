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

                attributes { setField, onBlur, onReset, onEnter } =
                    exampleConfig.attributes
                        ++ List.filterMap identity
                            [ if exampleConfig.onBlur then
                                Just (TextInput.onBlur (setField onBlur))

                              else
                                Nothing
                            , if exampleConfig.onReset then
                                Just (TextInput.onReset (setField onReset))

                              else
                                Nothing
                            , if exampleConfig.onEnter then
                                Just (TextInput.onEnter (setField onEnter))

                              else
                                Nothing
                            ]
            in
            [ Control.view UpdateControl state.control
                |> Html.fromUnstyled
            , viewExamples
                [ ( "text"
                  , TextInput.view exampleConfig.label
                        (TextInput.id "text-input__text-example"
                            :: TextInput.text
                            :: TextInput.onInput (SetInput 1)
                            :: (TextInput.value <|
                                    (Maybe.withDefault "" <| Dict.get 1 state.inputValues)
                               )
                            :: attributes
                                { setField = SetInput 1
                                , onBlur = "Blurred!!!"
                                , onReset = ""
                                , onEnter = "Entered!!!"
                                }
                        )
                  )
                , ( "number"
                  , TextInput.view exampleConfig.label
                        (TextInput.id "text-input__number-example"
                            :: TextInput.map
                                (Maybe.map String.fromInt >> Maybe.withDefault "")
                                identity
                                (SetInput 100)
                                TextInput.number
                            :: TextInput.onInput (SetInput 100)
                            :: TextInput.value
                                (Maybe.withDefault "" <|
                                    Dict.get 100 state.inputValues
                                )
                            :: attributes
                                { setField = SetInput 100
                                , onBlur = "10000000"
                                , onReset = ""
                                , onEnter = "20000000"
                                }
                        )
                  )
                , ( "float"
                  , TextInput.view exampleConfig.label
                        (TextInput.id "text-input__float-example"
                            :: TextInput.map
                                (Maybe.map String.fromFloat >> Maybe.withDefault "")
                                identity
                                (SetInput 1000)
                                TextInput.float
                            :: TextInput.onInput (SetInput 1000)
                            :: TextInput.value
                                (Maybe.withDefault "" <|
                                    Dict.get 1000 state.inputValues
                                )
                            :: attributes
                                { setField = SetInput 1000
                                , onBlur = "1.00000001"
                                , onReset = ""
                                , onEnter = "100000001.1"
                                }
                        )
                  )
                , ( "password"
                  , TextInput.view exampleConfig.label
                        (TextInput.id "text-input__password-example"
                            :: TextInput.password
                            :: TextInput.onInput (SetInput 10)
                            :: TextInput.value (Maybe.withDefault "" <| Dict.get 10 state.inputValues)
                            :: attributes
                                { setField = SetInput 10
                                , onBlur = "Blurred!!!"
                                , onReset = ""
                                , onEnter = "Entered!!!"
                                }
                        )
                  )
                , ( "email"
                  , TextInput.view exampleConfig.label
                        (TextInput.id "text-input__email-example"
                            :: TextInput.email
                            :: TextInput.onInput (SetInput 2)
                            :: TextInput.value (Maybe.withDefault "" <| Dict.get 2 state.inputValues)
                            :: attributes
                                { setField = SetInput 2
                                , onBlur = "Blurred!!!"
                                , onReset = ""
                                , onEnter = "Entered!!!"
                                }
                        )
                  )
                , ( "search"
                  , TextInput.view exampleConfig.label
                        (TextInput.id "text-input__search-example"
                            :: TextInput.search
                            :: TextInput.onInput (SetInput 11)
                            :: TextInput.value (Maybe.withDefault "" <| Dict.get 11 state.inputValues)
                            :: attributes
                                { setField = SetInput 11
                                , onBlur = "Blurred!!!"
                                , onReset = ""
                                , onEnter = "Entered!!!"
                                }
                        )
                  )
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
    , onReset : Bool
    , onEnter : Bool
    }


initControl : Control ExampleConfig
initControl =
    Control.record ExampleConfig
        |> Control.field "label" (Control.string "Assignment name")
        |> Control.field "attributes" controlAttributes
        |> Control.field "onBlur" (Control.bool False)
        |> Control.field "onReset" (Control.bool False)
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
