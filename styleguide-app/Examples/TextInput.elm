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


{-| -}
type Msg
    = SetTextInput Id String
    | SetNumberInput (Maybe Int)
    | SetFloatInput (Maybe Float)
    | SetPassword String
    | SetSearchTerm String
    | UpdateControl (Control ExampleConfig)


{-| -}
type alias State =
    { numberInputValue : Maybe Int
    , floatInputValue : Maybe Float
    , stringInputValues : Dict Id String
    , passwordInputValue : String
    , searchInputValue : String
    , control : Control ExampleConfig
    }


type alias ExampleConfig =
    { label : String
    , maybePlaceholderAttribute : Maybe (TextInput.Attribute Msg)
    , maybeErrorAttribute1 : Maybe (TextInput.Attribute Msg)
    , maybeErrorAttribute2 : Maybe (TextInput.Attribute Msg)
    , maybeShowLabelAttribute : Maybe (TextInput.Attribute Msg)
    , maybeDisabledAttribute : Maybe (TextInput.Attribute Msg)
    , maybeLoadingAttribute : Maybe (TextInput.Attribute Msg)
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
                    (List.filterMap identity
                        [ exampleConfig.maybeErrorAttribute1
                        , exampleConfig.maybeErrorAttribute2
                        , exampleConfig.maybePlaceholderAttribute
                        , exampleConfig.maybeShowLabelAttribute
                        , exampleConfig.maybeDisabledAttribute
                        , exampleConfig.maybeLoadingAttribute
                        ]
                    )
                    (Maybe.withDefault "" <| Dict.get 1 state.stringInputValues)
                , Heading.h3 [] [ text "TextInput.number" ]
                , TextInput.view (exampleConfig.label ++ " (number)")
                    (TextInput.number SetNumberInput)
                    (List.filterMap identity
                        [ exampleConfig.maybeErrorAttribute1
                        , exampleConfig.maybeErrorAttribute2
                        , exampleConfig.maybePlaceholderAttribute
                        , exampleConfig.maybeShowLabelAttribute
                        , exampleConfig.maybeDisabledAttribute
                        , exampleConfig.maybeLoadingAttribute
                        , Just (TextInput.id "hey-this-is-a-test-id")
                        ]
                    )
                    state.numberInputValue
                , Heading.h3 [] [ text "TextInput.float" ]
                , TextInput.view (exampleConfig.label ++ " (float)")
                    (TextInput.float SetFloatInput)
                    (List.filterMap identity
                        [ exampleConfig.maybeErrorAttribute1
                        , exampleConfig.maybeErrorAttribute2
                        , exampleConfig.maybePlaceholderAttribute
                        , exampleConfig.maybeShowLabelAttribute
                        , exampleConfig.maybeDisabledAttribute
                        , exampleConfig.maybeLoadingAttribute
                        ]
                    )
                    state.floatInputValue
                , Heading.h3 [] [ text "TextInput.password" ]
                , TextInput.view (exampleConfig.label ++ " (password)")
                    (TextInput.password SetPassword)
                    (List.filterMap identity
                        [ exampleConfig.maybeErrorAttribute1
                        , exampleConfig.maybeErrorAttribute2
                        , exampleConfig.maybePlaceholderAttribute
                        , exampleConfig.maybeShowLabelAttribute
                        , exampleConfig.maybeDisabledAttribute
                        , exampleConfig.maybeLoadingAttribute
                        ]
                    )
                    state.passwordInputValue
                , Heading.h3 [] [ text "TextInput.email" ]
                , TextInput.view (exampleConfig.label ++ " (email)")
                    (TextInput.email (SetTextInput 2))
                    (List.filterMap identity
                        [ exampleConfig.maybeErrorAttribute1
                        , exampleConfig.maybeErrorAttribute2
                        , exampleConfig.maybePlaceholderAttribute
                        , exampleConfig.maybeShowLabelAttribute
                        , exampleConfig.maybeDisabledAttribute
                        , exampleConfig.maybeLoadingAttribute
                        ]
                    )
                    (Maybe.withDefault "" <| Dict.get 2 state.stringInputValues)
                , Heading.h3 [] [ Html.text "TextInput.writing" ]
                , TextInput.view (exampleConfig.label ++ " (writing)")
                    (TextInput.text (SetTextInput 4))
                    (List.filterMap identity
                        [ Just TextInput.writing
                        , exampleConfig.maybeErrorAttribute1
                        , exampleConfig.maybeErrorAttribute2
                        , exampleConfig.maybePlaceholderAttribute
                        , exampleConfig.maybeShowLabelAttribute
                        , exampleConfig.maybeDisabledAttribute
                        , exampleConfig.maybeLoadingAttribute
                        ]
                    )
                    (Maybe.withDefault "" <| Dict.get 4 state.stringInputValues)
                , Heading.h3 [] [ Html.text "TextInput.search" ]
                , TextInput.view (exampleConfig.label ++ " (search)")
                    (TextInput.search SetSearchTerm)
                    (List.filterMap identity
                        [ exampleConfig.maybeErrorAttribute1
                        , exampleConfig.maybeErrorAttribute2
                        , exampleConfig.maybePlaceholderAttribute
                        , exampleConfig.maybeShowLabelAttribute
                        , exampleConfig.maybeDisabledAttribute
                        , exampleConfig.maybeLoadingAttribute
                        ]
                    )
                    state.searchInputValue
                , Heading.h3 [] [ text "TextInput.onBlur" ]
                , TextInput.view (exampleConfig.label ++ " (onBlur)")
                    (TextInput.text (SetTextInput 7))
                    (List.filterMap identity
                        [ Just (TextInput.onBlur (SetTextInput 7 "Blurred!"))
                        , exampleConfig.maybeErrorAttribute1
                        , exampleConfig.maybeErrorAttribute2
                        , exampleConfig.maybePlaceholderAttribute
                        , exampleConfig.maybeShowLabelAttribute
                        , exampleConfig.maybeDisabledAttribute
                        , exampleConfig.maybeLoadingAttribute
                        ]
                    )
                    (Maybe.withDefault "" <| Dict.get 7 state.stringInputValues)
                , Heading.h3 [] [ text "TextInput.css" ]
                , TextInput.view (exampleConfig.label ++ " (custom CSS)")
                    (TextInput.text (SetTextInput 8))
                    (List.filterMap identity
                        [ exampleConfig.maybeErrorAttribute1
                        , exampleConfig.maybeErrorAttribute2
                        , exampleConfig.maybePlaceholderAttribute
                        , exampleConfig.maybeShowLabelAttribute
                        , exampleConfig.maybeDisabledAttribute
                        , exampleConfig.maybeLoadingAttribute
                        , Just (TextInput.css [ Css.backgroundColor Colors.azure ])
                        ]
                    )
                    (Maybe.withDefault "" <| Dict.get 8 state.stringInputValues)
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



-- INTERNAL


type alias Id =
    Int
