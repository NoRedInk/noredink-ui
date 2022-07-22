module Examples.TextInput exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled as Html exposing (..)
import Accessibility.Styled.Key as Key
import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Dict exposing (Dict)
import Example exposing (Example)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.TextInput.V7 as TextInput
import ViewHelpers exposing (viewExamples)


moduleName : String
moduleName =
    "TextInput"


version =
    7


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Inputs ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ TextInput.view "Text Input"
            [ TextInput.custom [ Key.tabbable False ]
            ]
        , TextInput.view "Errored"
            [ TextInput.value "invalid content"
            , TextInput.errorIf True
            , TextInput.custom [ Key.tabbable False ]
            ]
        ]
    , view =
        \ellieLinkConfig state ->
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state.control
                , mainType = "Html msg"
                , extraImports = []
                , toExampleCode = \_ -> []
                }
            , Heading.h2 [ Heading.style Heading.Subhead ] [ Html.text "Example" ]
            , viewExamples <|
                ( "readOnlyText"
                , TextInput.view "Shareable Assignment Link"
                    [ TextInput.readOnlyText
                    , TextInput.value "noredink.com/s/blueprint-code"
                    ]
                )
                    :: customizableExamples state
            ]
    }


customizableExamples : State -> List ( String, Html Msg )
customizableExamples state =
    let
        exampleConfig =
            Control.currentValue state.control

        toExample { name, toString, inputType, onFocus, onBlur, onEnter } index =
            ( name
            , TextInput.view exampleConfig.label
                (exampleConfig.attributes
                    ++ [ TextInput.id ("text-input__" ++ name ++ "-example")
                       , inputType (toString >> SetInput index)
                            |> TextInput.map toString identity
                       , TextInput.value (Maybe.withDefault "" (Dict.get index state.inputValues))
                       ]
                    ++ List.filterMap identity
                        [ if exampleConfig.onFocus then
                            Just (TextInput.onFocus (SetInput index onFocus))

                          else
                            Nothing
                        , if exampleConfig.onBlur then
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
    List.indexedMap (\i toView -> toView i)
        [ toExample
            { name = "text"
            , toString = identity
            , inputType = TextInput.text
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "number"
            , toString = Maybe.map String.fromInt >> Maybe.withDefault ""
            , inputType = TextInput.number
            , onFocus = "1234"
            , onBlur = "10000000"
            , onEnter = "20000000"
            }
        , toExample
            { name = "float"
            , toString = Maybe.map String.fromFloat >> Maybe.withDefault ""
            , inputType = TextInput.float
            , onFocus = "123"
            , onBlur = "1.00000001"
            , onEnter = "100000001.1"
            }
        , toExample
            { name = "newPassword"
            , toString = identity
            , inputType =
                \onInput ->
                    TextInput.newPassword
                        { onInput = onInput
                        , showPassword = state.showPassword
                        , setShowPassword = SetShowPassword
                        }
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "email"
            , toString = identity
            , inputType = TextInput.email
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "search"
            , toString = identity
            , inputType = TextInput.search
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "givenName"
            , toString = identity
            , inputType = TextInput.givenName
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "familyName"
            , toString = identity
            , inputType = TextInput.familyName
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "organization"
            , toString = identity
            , inputType = TextInput.organization
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "organizationTitle"
            , toString = identity
            , inputType = TextInput.organizationTitle
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "addressLine1"
            , toString = identity
            , inputType = TextInput.addressLine1
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "addressLevel2"
            , toString = identity
            , inputType = TextInput.addressLevel2
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "postalCode"
            , toString = identity
            , inputType = TextInput.postalCode
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "tel"
            , toString = identity
            , inputType = TextInput.tel
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "sex"
            , toString = identity
            , inputType = TextInput.sex
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        ]


{-| -}
type alias State =
    { inputValues : Dict Int String
    , showPassword : Bool
    , control : Control ExampleConfig
    }


{-| -}
init : State
init =
    { inputValues = Dict.empty
    , showPassword = False
    , control = initControl
    }


type alias ExampleConfig =
    { label : String
    , attributes : List (TextInput.Attribute String Msg)
    , onFocus : Bool
    , onBlur : Bool
    , onEnter : Bool
    }


initControl : Control ExampleConfig
initControl =
    Control.record ExampleConfig
        |> Control.field "label" (Control.string "Assignment name")
        |> Control.field "attributes" controlAttributes
        |> Control.field "onFocus" (Control.bool False)
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
            (Control.map (Just >> TextInput.errorMessage) <| Control.string "The statement must be true.")
        |> ControlExtra.optionalListItem "guidance"
            (Control.map TextInput.guidance <| Control.string "The statement must be true.")
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
    | SetShowPassword Bool
    | UpdateControl (Control ExampleConfig)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetInput id string ->
            ( { state | inputValues = Dict.insert id string state.inputValues }
            , Cmd.none
            )

        SetShowPassword showPassword ->
            ( { state | showPassword = showPassword }
            , Cmd.none
            )

        UpdateControl newControl ->
            ( { state | control = newControl }
            , Cmd.none
            )
