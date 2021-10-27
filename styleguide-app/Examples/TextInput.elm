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
                [ ( "TextInput.text"
                  , TextInput.view (exampleConfig.label ++ " (text)")
                        (TextInput.text (SetTextInput 1))
                        (attributes
                            { setField = SetTextInput 1
                            , onBlur = "Blurred!!!"
                            , onReset = ""
                            , onEnter = "Entered!!!"
                            }
                        )
                        (Maybe.withDefault "" <| Dict.get 1 state.stringInputValues)
                  )
                , ( "TextInput.number"
                  , TextInput.view (exampleConfig.label ++ " (number)")
                        (TextInput.number SetNumberInput)
                        (TextInput.id "hey-this-is-a-test-id"
                            :: attributes
                                { setField = SetNumberInput
                                , onBlur = Just 10000000
                                , onReset = Nothing
                                , onEnter = Just 20000000
                                }
                        )
                        state.numberInputValue
                  )
                , ( "TextInput.float"
                  , TextInput.view (exampleConfig.label ++ " (float)")
                        (TextInput.float SetFloatInput)
                        (attributes
                            { setField = SetFloatInput
                            , onBlur = Just 1.00000001
                            , onReset = Nothing
                            , onEnter = Just 100000001.1
                            }
                        )
                        state.floatInputValue
                  )
                , ( "TextInput.email"
                  , TextInput.view (exampleConfig.label ++ " (email)")
                        (TextInput.email (SetTextInput 2))
                        (attributes
                            { setField = SetTextInput 2
                            , onBlur = "Blurred!!!"
                            , onReset = ""
                            , onEnter = "Entered!!!"
                            }
                        )
                        (Maybe.withDefault "" <| Dict.get 2 state.stringInputValues)
                  )
                , ( "TextInput.writing"
                  , TextInput.view (exampleConfig.label ++ " (writing)")
                        (TextInput.text (SetTextInput 4))
                        (TextInput.writing
                            :: attributes
                                { setField = SetTextInput 4
                                , onBlur = "Blurred!!!"
                                , onReset = ""
                                , onEnter = "Entered!!!"
                                }
                        )
                        (Maybe.withDefault "" <| Dict.get 4 state.stringInputValues)
                  )
                , ( "TextInput.search"
                  , TextInput.view (exampleConfig.label ++ " (search)")
                        (TextInput.search SetSearchTerm)
                        (attributes
                            { setField = SetSearchTerm
                            , onBlur = "Blurred!!!"
                            , onReset = ""
                            , onEnter = "Entered!!!"
                            }
                        )
                        state.searchInputValue
                  )
                , ( "TextInput.css"
                  , TextInput.view (exampleConfig.label ++ " (custom CSS)")
                        (TextInput.text (SetTextInput 8))
                        (TextInput.css [ Css.backgroundColor Colors.azure ]
                            :: attributes
                                { setField = SetTextInput 8
                                , onBlur = "Blurred!!!"
                                , onReset = ""
                                , onEnter = "Entered!!!"
                                }
                        )
                        (Maybe.withDefault "" <| Dict.get 8 state.stringInputValues)
                  )
                ]
            ]
    }


{-| -}
type alias State =
    { numberInputValue : Maybe Int
    , floatInputValue : Maybe Float
    , stringInputValues : Dict Id String
    , passwordInputValue : String
    , searchInputValue : String
    , control : Control ExampleConfig
    }


{-| -}
init : State
init =
    { numberInputValue = Nothing
    , floatInputValue = Nothing
    , stringInputValues = Dict.empty
    , passwordInputValue = ""
    , searchInputValue = ""
    , control = initControl
    }


type alias ExampleConfig =
    { label : String
    , attributes : List (TextInput.Attribute Msg)
    , onBlur : Bool
    , onReset : Bool
    , onEnter : Bool
    }


initControl : Control ExampleConfig
initControl =
    Control.record ExampleConfig
        |> Control.field "label" (Control.string "Assignment name")
        |> Control.field "attributes" controlAttributes
        |> Control.field "TextInput.onBlur" (Control.bool False)
        |> Control.field "TextInput.onReset" (Control.bool False)
        |> Control.field "TextInput.onEnter" (Control.bool False)


controlAttributes : Control (List (TextInput.Attribute msg))
controlAttributes =
    ControlExtra.list
        |> ControlExtra.optionalListItem "TextInput.placeholder"
            (Control.map TextInput.placeholder <|
                Control.string "Learning with commas"
            )
        |> ControlExtra.optionalListItem "TextInput.hiddenLabel"
            (Control.value TextInput.hiddenLabel)
        |> ControlExtra.optionalListItem "TextInput.errorIf"
            (Control.map TextInput.errorIf <| Control.bool True)
        |> ControlExtra.optionalListItem "TextInput.errorMessage"
            (Control.map TextInput.errorMessage <| Control.maybe True <| Control.string "The statement must be true.")
        |> ControlExtra.optionalListItem "TextInput.disabled"
            (Control.value TextInput.disabled)
        |> ControlExtra.optionalListItem "TextInput.loading"
            (Control.value TextInput.loading)
        |> ControlExtra.listItem "TextInput.noMargin"
            (Control.map TextInput.noMargin (Control.bool False))


{-| -}
type Msg
    = SetTextInput Id String
    | SetNumberInput (Maybe Int)
    | SetFloatInput (Maybe Float)
    | SetPassword String
    | SetSearchTerm String
    | UpdateControl (Control ExampleConfig)


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

        SetSearchTerm searchInputValue ->
            ( { state | searchInputValue = searchInputValue }, Cmd.none )

        UpdateControl newControl ->
            ( { state | control = newControl }, Cmd.none )



-- INTERNAL


type alias Id =
    Int
