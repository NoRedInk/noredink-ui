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

                sharedAttributes : List (TextInput.Attribute value Msg)
                sharedAttributes =
                    List.map (TextInput.map never (\_ -> "") (\_ -> NoOp))
                        exampleConfig.attributes

                attributes { setField, onBlur, onReset, onEnter } =
                    sharedAttributes
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
                            :: TextInput.onInput (SetTextInput 1)
                            :: (TextInput.value <|
                                    (Maybe.withDefault "" <| Dict.get 1 state.stringInputValues)
                               )
                            :: attributes
                                { setField = SetTextInput 1
                                , onBlur = "Blurred!!!"
                                , onReset = ""
                                , onEnter = "Entered!!!"
                                }
                        )
                  )
                , ( "number"
                  , TextInput.view exampleConfig.label
                        (TextInput.id "text-input__number-example"
                            :: TextInput.number
                            :: TextInput.onInput SetNumberInput
                            :: TextInput.value state.numberInputValue
                            :: attributes
                                { setField = SetNumberInput
                                , onBlur = Just 10000000
                                , onReset = Nothing
                                , onEnter = Just 20000000
                                }
                        )
                  )
                , ( "float"
                  , TextInput.view exampleConfig.label
                        (TextInput.id "text-input__float-example"
                            :: TextInput.float
                            :: TextInput.onInput SetFloatInput
                            :: TextInput.value state.floatInputValue
                            :: attributes
                                { setField = SetFloatInput
                                , onBlur = Just 1.00000001
                                , onReset = Nothing
                                , onEnter = Just 100000001.1
                                }
                        )
                  )
                , ( "password"
                  , TextInput.view exampleConfig.label
                        (TextInput.id "text-input__password-example"
                            :: TextInput.password
                            :: TextInput.onInput SetPassword
                            :: TextInput.value state.passwordInputValue
                            :: attributes
                                { setField = SetPassword
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
                            :: TextInput.onInput (SetTextInput 2)
                            :: TextInput.value (Maybe.withDefault "" <| Dict.get 2 state.stringInputValues)
                            :: attributes
                                { setField = SetTextInput 2
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
                            :: TextInput.onInput SetSearchTerm
                            :: TextInput.value state.searchInputValue
                            :: attributes
                                { setField = SetSearchTerm
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
    , attributes : List (TextInput.Attribute Never Msg)
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


controlAttributes : Control (List (TextInput.Attribute Never msg))
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
    = SetTextInput Id String
    | SetNumberInput (Maybe Int)
    | SetFloatInput (Maybe Float)
    | SetPassword String
    | SetSearchTerm String
    | UpdateControl (Control ExampleConfig)
    | NoOp


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

        NoOp ->
            ( state, Cmd.none )



-- INTERNAL


type alias Id =
    Int
