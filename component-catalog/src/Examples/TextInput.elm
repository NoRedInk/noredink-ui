module Examples.TextInput exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Key as Key
import Browser.Dom as Dom
import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Dict exposing (Dict)
import Example exposing (Example)
import Guidance
import Iso8601
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.TextInput.V8 as TextInput
import Process
import Task
import ViewHelpers exposing (viewExamples)


moduleName : String
moduleName =
    "TextInput"


version =
    8


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Inputs ]
    , keyboardSupport = []
    , init = init
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
    , about = Guidance.useATACGuide moduleName
    , view =
        \ellieLinkConfig state ->
            let
                examples =
                    ( "readOnlyText"
                    , """TextInput.view "Shareable Assignment Link"
        [ TextInput.readOnlyText
        , TextInput.value "noredink.com/s/blueprint-code"
        ]
                         """
                    , TextInput.view "Shareable Assignment Link"
                        [ TextInput.readOnlyText
                        , TextInput.value "noredink.com/s/blueprint-code"
                        ]
                    )
                        :: customizableExamples state
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state.control
                , mainType = Nothing
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \_ ->
                        List.map
                            (\( name, toExampleCode, _ ) ->
                                { sectionName = name
                                , code =
                                    toExampleCode
                                }
                            )
                            examples
                }
            , Heading.h2
                [ Heading.plaintext "Customizable Examples"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , viewExamples (List.map (\( name, _, ex ) -> ( name, ex )) examples)
            ]
    }


newPasswordVisibilityToggleId : String
newPasswordVisibilityToggleId =
    "text-input__newPassword-example__visibility-toggle"


currentPasswordVisibilityToggleId : String
currentPasswordVisibilityToggleId =
    "text-input__currentPassword-example__visibility-toggle"


customizableExamples : State -> List ( String, String, Html Msg )
customizableExamples state =
    let
        exampleConfig =
            Control.currentValue state.control

        toExample { name, toString, inputType, inputTypeCode, inputTypeValueCode, onFocus, onBlur, onEnter } index =
            let
                maybeValue =
                    Dict.get index state.inputValues
            in
            ( name
            , "TextInput.view "
                ++ Code.string exampleConfig.label
                ++ ([ Just <| "TextInput.id " ++ Code.string ("text-input__" ++ name ++ "-example")
                    , Just <| inputTypeCode ++ " identity -- use a Msg instead of identity"
                    , Just <| "TextInput.value " ++ inputTypeValueCode maybeValue
                    , if exampleConfig.onFocus then
                        Just "TextInput.onFocus identity -- use a Msg instead of identity"

                      else
                        Nothing
                    , if exampleConfig.onBlur then
                        Just "TextInput.onBlur identity -- use a Msg instead of identity"

                      else
                        Nothing
                    , if exampleConfig.onEnter then
                        Just "TextInput.onEnter identity -- use a Msg instead of identity"

                      else
                        Nothing
                    ]
                        |> List.filterMap identity
                        |> (\attributes -> Code.list (attributes ++ List.map Tuple.first exampleConfig.attributes))
                   )
            , TextInput.view exampleConfig.label
                (List.map Tuple.second exampleConfig.attributes
                    ++ [ TextInput.id ("text-input__" ++ name ++ "-example")
                       , inputType (toString >> SetInput index)
                            |> TextInput.map toString identity
                       , TextInput.value (Maybe.withDefault "" maybeValue)
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
            , inputTypeCode = "TextInput.text"
            , inputTypeValueCode = \value -> Code.string (Maybe.withDefault "" value)
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "number"
            , toString = Maybe.map String.fromInt >> Maybe.withDefault ""
            , inputType = TextInput.number
            , inputTypeCode = "TextInput.number"
            , inputTypeValueCode = Code.maybe >> Code.withParens
            , onFocus = "1234"
            , onBlur = "10000000"
            , onEnter = "20000000"
            }
        , toExample
            { name = "float"
            , toString = Maybe.map String.fromFloat >> Maybe.withDefault ""
            , inputType = TextInput.float
            , inputTypeCode = "TextInput.float"
            , inputTypeValueCode = Code.maybe >> Code.withParens
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
                        , showPassword = state.showNewPassword
                        , setShowPassword = SetShowNewPassword
                        , visibilityToggleId = newPasswordVisibilityToggleId
                        }
            , inputTypeCode =
                """TextInput.newPassword <|
        \\onInput ->
            TextInput.newPassword
                { onInput = onInput
                , showNewPassword = model.showNewPassword -- pass in whether the PW should be shown as plaintext. You'll need to wire this in.
                , setShowNewPassword = SetShowNewPassword -- You'll need to wire this in before the code will compile
                , visibilityToggleId = newPasswordVisibilityToggleId
                }
                """
            , inputTypeValueCode = \value -> Code.string (Maybe.withDefault "" value)
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "currentPassword"
            , toString = identity
            , inputType =
                \onInput ->
                    TextInput.currentPassword
                        { onInput = onInput
                        , showPassword = state.showCurrentPassword
                        , setShowPassword = SetShowCurrentPassword
                        , visibilityToggleId = currentPasswordVisibilityToggleId
                        }
            , inputTypeCode =
                """TextInput.currentPassword <|
        \\onInput ->
            TextInput.currentPassword
                { onInput = onInput
                , showPassword = model.showCurrentPassword -- pass in whether the PW should be shown as plaintext. You'll need to wire this in.
                , setShowPassword = SetShowCurrentPassword -- You'll need to wire this in before the code will compile
                , visibilityToggleId = currentPasswordVisibilityToggleId
                }
                """
            , inputTypeValueCode = \value -> Code.string (Maybe.withDefault "" value)
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "email"
            , toString = identity
            , inputType = TextInput.email
            , inputTypeCode = "TextInput.email"
            , inputTypeValueCode = \value -> Code.string (Maybe.withDefault "" value)
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "search"
            , toString = identity
            , inputType = TextInput.search
            , inputTypeCode = "TextInput.search"
            , inputTypeValueCode = \value -> Code.string (Maybe.withDefault "" value)
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "givenName"
            , toString = identity
            , inputType = TextInput.givenName
            , inputTypeCode = "TextInput.givenName"
            , inputTypeValueCode = \value -> Code.string (Maybe.withDefault "" value)
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "familyName"
            , toString = identity
            , inputType = TextInput.familyName
            , inputTypeCode = "TextInput.familyName"
            , inputTypeValueCode = \value -> Code.string (Maybe.withDefault "" value)
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "organization"
            , toString = identity
            , inputType = TextInput.organization
            , inputTypeCode = "TextInput.organization"
            , inputTypeValueCode = \value -> Code.string (Maybe.withDefault "" value)
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "organizationTitle"
            , toString = identity
            , inputType = TextInput.organizationTitle
            , inputTypeCode = "TextInput.organizationTitle"
            , inputTypeValueCode = \value -> Code.string (Maybe.withDefault "" value)
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "addressLine1"
            , toString = identity
            , inputType = TextInput.addressLine1
            , inputTypeCode = "TextInput.addressLine1"
            , inputTypeValueCode = \value -> Code.string (Maybe.withDefault "" value)
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "addressLevel2"
            , toString = identity
            , inputType = TextInput.addressLevel2
            , inputTypeCode = "TextInput.addressLevel2"
            , inputTypeValueCode = \value -> Code.string (Maybe.withDefault "" value)
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "countryName"
            , toString = identity
            , inputType = TextInput.countryName
            , inputTypeCode = "TextInput.countryName"
            , inputTypeValueCode = \value -> Code.string (Maybe.withDefault "" value)
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "postalCode"
            , toString = identity
            , inputType = TextInput.postalCode
            , inputTypeCode = "TextInput.postalCode"
            , inputTypeValueCode = \value -> Code.string (Maybe.withDefault "" value)
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "tel"
            , toString = identity
            , inputType = TextInput.tel
            , inputTypeCode = "TextInput.tel"
            , inputTypeValueCode = \value -> Code.string (Maybe.withDefault "" value)
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "sex"
            , toString = identity
            , inputType = TextInput.sex
            , inputTypeCode = "TextInput.sex"
            , inputTypeValueCode = \value -> Code.string (Maybe.withDefault "" value)
            , onFocus = "Focused!!!"
            , onBlur = "Blurred!!!"
            , onEnter = "Entered!!!"
            }
        , toExample
            { name = "date"
            , toString =
                \result ->
                    case result of
                        Just date ->
                            date |> Iso8601.fromTime >> String.slice 0 10

                        Nothing ->
                            ""
            , inputType = TextInput.date
            , inputTypeCode = "TextInput.date"
            , inputTypeValueCode = \value -> Code.string (Maybe.withDefault "" value)
            , onFocus = "onFocus"
            , onBlur = "onBlur"
            , onEnter = "onEnter"
            }
        , toExample
            { name = "datetime"
            , toString =
                \result ->
                    case result of
                        Just date ->
                            date |> Iso8601.fromTime >> String.dropRight 1

                        Nothing ->
                            ""
            , inputType = TextInput.datetime
            , inputTypeCode = "TextInput.datetime"
            , inputTypeValueCode = \value -> Code.string (Maybe.withDefault "" value)
            , onFocus = "onFocus"
            , onBlur = "onBlur"
            , onEnter = "onEnter"
            }
        ]


{-| -}
type alias State =
    { inputValues : Dict Int String
    , showNewPassword : Bool
    , showCurrentPassword : Bool
    , control : Control ExampleConfig
    }


{-| -}
init : State
init =
    { inputValues = Dict.empty
    , showNewPassword = False
    , showCurrentPassword = False
    , control = initControl
    }


type alias ExampleConfig =
    { label : String
    , attributes : List ( String, TextInput.Attribute String Msg )
    , onFocus : Bool
    , onBlur : Bool
    , onEnter : Bool
    }


initControl : Control ExampleConfig
initControl =
    Control.record (\a b ( c, d, e ) -> ExampleConfig a b c d e)
        |> Control.field "label" (Control.string "Assignment name")
        |> Control.field "" controlAttributes
        |> Control.field "Event listeners"
            (Control.record (\a b c -> ( a, b, c ))
                |> Control.field "onFocus" (Control.bool False)
                |> Control.field "onBlur" (Control.bool False)
                |> Control.field "onEnter" (Control.bool False)
            )


controlAttributes : Control (List ( String, TextInput.Attribute value msg ))
controlAttributes =
    Control.list
        |> ControlExtra.listItems "State"
            (Control.list
                |> ControlExtra.optionalBoolListItem "disabled"
                    ( "TextInput.disabled", TextInput.disabled )
                |> ControlExtra.optionalBoolListItem "loading"
                    ( "TextInput.loading", TextInput.loading )
            )
        |> ControlExtra.listItems "Theme, CSS, & Style Extras"
            (Control.list
                |> ControlExtra.optionalBoolListItem "hiddenLabel"
                    ( "TextInput.hiddenLabel", TextInput.hiddenLabel )
                |> ControlExtra.optionalBoolListItem "writing"
                    ( "TextInput.writing", TextInput.writing )
                |> ControlExtra.optionalBoolListItem "noMargin"
                    ( "TextInput.noMargin True", TextInput.noMargin True )
                |> ControlExtra.optionalBoolListItem "css"
                    ( "TextInput.css [ Css.backgroundColor Colors.azure ]"
                    , TextInput.css [ Css.backgroundColor Colors.azure ]
                    )
            )
        |> ControlExtra.listItems "Content"
            (Control.list
                |> ControlExtra.optionalListItem "placeholder"
                    (Control.string "Learning with commas"
                        |> Control.map
                            (\str ->
                                ( "TextInput.placeholder " ++ Code.string str
                                , TextInput.placeholder str
                                )
                            )
                    )
                |> CommonControls.guidanceAndErrorMessage
                    { moduleName = moduleName
                    , guidance = TextInput.guidance
                    , guidanceHtml = TextInput.guidanceHtml
                    , errorMessage = Just TextInput.errorMessage
                    , message = "The statement must be true."
                    }
            )


{-| -}
type Msg
    = SetInput Int String
    | SetShowNewPassword Bool
    | SetShowCurrentPassword Bool
    | UpdateControl (Control ExampleConfig)
    | Focused (Result Dom.Error ())
    | Blurred (Result Dom.Error ())


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetInput id string ->
            ( { state | inputValues = Dict.insert id string state.inputValues }
            , Cmd.none
            )

        SetShowNewPassword showNewPassword ->
            ( { state | showNewPassword = showNewPassword }
            , Cmd.batch
                [ Task.attempt Blurred (Dom.blur newPasswordVisibilityToggleId)
                , Process.sleep 0
                    |> Task.andThen (\_ -> Dom.focus newPasswordVisibilityToggleId)
                    |> Task.attempt Focused
                ]
            )

        SetShowCurrentPassword showCurrentPassword ->
            ( { state | showCurrentPassword = showCurrentPassword }
            , Cmd.batch
                [ Task.attempt Blurred (Dom.blur currentPasswordVisibilityToggleId)
                , Process.sleep 0
                    |> Task.andThen (\_ -> Dom.focus currentPasswordVisibilityToggleId)
                    |> Task.attempt Focused
                ]
            )

        UpdateControl newControl ->
            ( { state | control = newControl }
            , Cmd.none
            )

        Focused _ ->
            ( state, Cmd.none )

        Blurred _ ->
            ( state, Cmd.none )
