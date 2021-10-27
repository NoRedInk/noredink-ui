module Examples.TextInput exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled as Html exposing (..)
import Category exposing (Category(..))
import Css exposing (..)
import Debug.Control as Control exposing (Control)
import Dict exposing (Dict)
import Example exposing (Example)
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.TextInput.V6 as TextInput
import Nri.Ui.Message.V3 as Message


{-| -}
type Msg
    = SetTextInput Id String
    | SetNumberInput (Maybe Int)
    | SetFloatInput (Maybe Float)
    | SetPassword String
    | SetSearchTerm String
    | UpdateControl (Control ExampleConfig)
    | HitEnter


{-| -}
type alias State =
    { numberInputValue : Maybe Int
    , floatInputValue : Maybe Float
    , stringInputValues : Dict Id String
    , passwordInputValue : String
    , searchInputValue : String
    , control : Control ExampleConfig
    , enterCount : Int
    }


type alias ExampleConfig =
    { label : String
    , maybePlaceholderAttribute : Maybe (TextInput.Attribute Msg)
    , maybeErrorAttribute1 : Maybe (TextInput.Attribute Msg)
    , maybeErrorAttribute2 : Maybe (TextInput.Attribute Msg)
    , maybeShowLabelAttribute : Maybe (TextInput.Attribute Msg)
    , maybeDisabledAttribute : Maybe (TextInput.Attribute Msg)
    , maybeLoadingAttribute : Maybe (TextInput.Attribute Msg)
    , noMarginAttribute : TextInput.Attribute Msg
    , onBlur : Bool
    , onReset : Bool
    }


{-| -}
example : Example State Msg
example =
    { name = "TextInput"
    , version = 6
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
                    List.filterMap identity
                        [ exampleConfig.maybeErrorAttribute1
                        , exampleConfig.maybeErrorAttribute2
                        , exampleConfig.maybePlaceholderAttribute
                        , exampleConfig.maybeShowLabelAttribute
                        , exampleConfig.maybeDisabledAttribute
                        , exampleConfig.maybeLoadingAttribute
                        , Just exampleConfig.noMarginAttribute
                        , if exampleConfig.onBlur then
                            Just (TextInput.onBlur (setField onBlur))

                          else
                            Nothing
                        , if exampleConfig.onReset then
                            Just (TextInput.onReset (setField onReset))

                          else
                            Nothing
                        , Just <| TextInput.onEnter onEnter
                        ]
            in
            [ Control.view UpdateControl state.control
                |> Html.fromUnstyled
            , Html.div
                [ css
                    [ property "display" "grid"
                    , property "grid-template-columns" "auto 1fr"
                    , property "grid-gap" "10px"
                    ]
                ]
                [ Heading.h3 [] [ text "TextInput.text" ]
                , TextInput.view (exampleConfig.label ++ " (text)")
                    (TextInput.text (SetTextInput 1))
                    (attributes
                        { setField = SetTextInput 1
                        , onBlur = "Blurred!!!"
                        , onReset = ""
                        , onEnter = HitEnter
                        }
                    )
                    (Maybe.withDefault "" <| Dict.get 1 state.stringInputValues)
                , Heading.h3 [] [ text "TextInput.number" ]
                , TextInput.view (exampleConfig.label ++ " (number)")
                    (TextInput.number SetNumberInput)
                    (TextInput.id "hey-this-is-a-test-id"
                        :: attributes
                            { setField = SetNumberInput
                            , onBlur = Just 10000000
                            , onReset = Nothing
                        , onEnter = HitEnter
                            }
                    )
                    state.numberInputValue
                , Heading.h3 [] [ text "TextInput.float" ]
                , TextInput.view (exampleConfig.label ++ " (float)")
                    (TextInput.float SetFloatInput)
                    (attributes
                        { setField = SetFloatInput
                        , onBlur = Just 1.00000001
                        , onReset = Nothing
                        , onEnter = HitEnter
                        }
                    )
                    state.floatInputValue
                , Heading.h3 [] [ text "TextInput.password" ]
                , TextInput.view (exampleConfig.label ++ " (password)")
                    (TextInput.password SetPassword)
                    (attributes
                        { setField = SetPassword
                        , onBlur = "Blurred!!!"
                        , onReset = ""
                        , onEnter = HitEnter
                        }
                    )
                    state.passwordInputValue
                , Heading.h3 [] [ text "TextInput.email" ]
                , TextInput.view (exampleConfig.label ++ " (email)")
                    (TextInput.email (SetTextInput 2))
                    (attributes
                        { setField = SetTextInput 2
                        , onBlur = "Blurred!!!"
                        , onReset = ""
                        , onEnter = HitEnter
                        }
                    )
                    (Maybe.withDefault "" <| Dict.get 2 state.stringInputValues)
                , Heading.h3 [] [ Html.text "TextInput.writing" ]
                , TextInput.view (exampleConfig.label ++ " (writing)")
                    (TextInput.text (SetTextInput 4))
                    (TextInput.writing
                        :: attributes
                            { setField = SetTextInput 4
                            , onBlur = "Blurred!!!"
                            , onReset = ""
                        , onEnter = HitEnter
                            }
                    )
                    (Maybe.withDefault "" <| Dict.get 4 state.stringInputValues)
                , Heading.h3 [] [ Html.text "TextInput.search" ]
                , TextInput.view (exampleConfig.label ++ " (search)")
                    (TextInput.search SetSearchTerm)
                    (attributes
                        { setField = SetSearchTerm
                        , onBlur = "Blurred!!!"
                        , onReset = ""
                        , onEnter = HitEnter
                        }
                    )
                    state.searchInputValue
                , Heading.h3 [] [ text "TextInput.css" ]
                , TextInput.view (exampleConfig.label ++ " (custom CSS)")
                    (TextInput.text (SetTextInput 8))
                    (TextInput.css [ Css.backgroundColor Colors.azure ]
                        :: attributes
                            { setField = SetTextInput 8
                            , onBlur = "Blurred!!!"
                            , onReset = ""
                        , onEnter = HitEnter
                            }
                    )
                    (Maybe.withDefault "" <| Dict.get 8 state.stringInputValues)
                , Message.view [
                    Message.tiny,
                    Message.tip,
                    Message.plaintext <| "Hit enter " ++ String.fromInt state.enterCount ++ " times"
                ]
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
    , searchInputValue = ""
    , control =
        Control.record ExampleConfig
            |> Control.field "label" (Control.string "Assignment name")
            |> Control.field "TextInput.placeholder"
                (Control.maybe True <|
                    Control.map TextInput.placeholder <|
                        Control.string "Learning with commas"
                )
            |> Control.field "TextInput.hiddenLabel"
                (Control.maybe False (Control.value TextInput.hiddenLabel))
            |> Control.field "TextInput.errorIf"
                (Control.maybe False (Control.map TextInput.errorIf <| Control.bool True))
            |> Control.field "TextInput.errorMessage"
                (Control.maybe False (Control.map TextInput.errorMessage <| Control.maybe True <| Control.string "The statement must be true."))
            |> Control.field "TextInput.disabled"
                (Control.maybe False (Control.value TextInput.disabled))
            |> Control.field "TextInput.loading"
                (Control.maybe False (Control.value TextInput.loading))
            |> Control.field "TextInput.noMargin"
                (Control.map TextInput.noMargin (Control.bool False))
            |> Control.field "TextInput.onBlur"
                (Control.bool False)
            |> Control.field "TextInput.onReset"
                (Control.bool False)
    , enterCount = 0
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

        SetSearchTerm searchInputValue ->
            ( { state | searchInputValue = searchInputValue }, Cmd.none )

        UpdateControl newControl ->
            ( { state | control = newControl }, Cmd.none )

        HitEnter ->
            ({ state | enterCount = state.enterCount + 1}, Cmd.none)



-- INTERNAL


type alias Id =
    Int
