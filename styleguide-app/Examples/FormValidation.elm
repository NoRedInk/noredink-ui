module Examples.FormValidation exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Accessibility.Styled exposing (..)
import Category
import Css exposing (..)
import Example exposing (Example)
import Html.Styled exposing (styled)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FormValidation.V1 as FormValidation
import Nri.Ui.Text.V4 as Text
import Nri.Ui.TextInput.V6 as TextInput
import String.Verify
import Verify exposing (Validator)


{-| -}
type alias State =
    { formData : UnvalidatedForm
    , formState : FormValidation.FormState FormField
    }


init : State
init =
    { formData =
        { firstName = ""
        , lastName = ""
        , username = ""
        }
    , formState = FormValidation.init
    }


{-| -}
type Msg
    = OnInput FormField String
    | SubmitForm


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.FormValidation.V1"
    , categories = [ Category.Inputs ]
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view =
        \model ->
            let
                formDefinition =
                    FormValidation.form
                        [ FormValidation.textInput True FirstName .firstName
                        , FormValidation.textInput True LastName .lastName
                        , FormValidation.textInput False Username .username
                        ]
            in
            [ Text.smallBody
                [ text "NOTE: On the real site, we normally don't label fields as \"(required)\" or \"(optional)\". "
                , text "The example here does so just to make it clear what's going on."
                ]
            , FormValidation.view formDefinition OnInput validator model.formState model.formData <|
                \form ->
                    styled div
                        [ padding (px 25)
                        , border3 (px 3) dashed Colors.gray75
                        , borderRadius (px 10)
                        , maxWidth (px 250)
                        ]
                        []
                        [ form.textInput FirstName
                            "First name (required)"
                            [ TextInput.placeholder ""
                            ]
                        , form.textInput LastName
                            "Last name (required)"
                            [ TextInput.placeholder ""
                            , TextInput.css [ marginTop (px 10) ]
                            ]
                        , form.textInput Username
                            "Username (optional)"
                            [ TextInput.placeholder ""
                            , TextInput.css [ marginTop (px 10) ]
                            ]

                        --, form.textInput Points
                        --    "Points"
                        --    [ TextInput.placeholder "20"
                        --    , TextInput.number
                        --    ]
                        , form.submitButton
                            { activeLabel = "Submit"
                            , submittingLabel = "Saving..."
                            , onClick = SubmitForm
                            }
                            [ Button.css [ marginTop (px 10) ]
                            ]
                        ]
            ]
    }


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        OnInput field newString ->
            let
                formData =
                    model.formData

                newFormData =
                    case field of
                        FirstName ->
                            { formData | firstName = newString }

                        LastName ->
                            { formData | lastName = newString }

                        Username ->
                            { formData | username = newString }
            in
            ( { model
                | formData = newFormData
                , formState = FormValidation.onInput validator newFormData model.formState
              }
            , Cmd.none
            )

        SubmitForm ->
            ( { model | formState = FormValidation.submit validator model.formData model.formState }
            , Cmd.none
            )


type FormField
    = FirstName
    | LastName
    | Username


type alias UnvalidatedForm =
    { firstName : String
    , lastName : String
    , username : String
    }


type alias ValidatedForm =
    { firstName : String
    , lastName : String
    , username : Maybe String
    }


validator : Validator ( FormField, String ) UnvalidatedForm ValidatedForm
validator =
    let
        maybeBlank s =
            if s == "" then
                Ok Nothing

            else
                Ok (Just s)
    in
    Verify.validate ValidatedForm
        |> Verify.verify .firstName (String.Verify.notBlank ( FirstName, "First name is required" ))
        |> Verify.verify .lastName (String.Verify.notBlank ( LastName, "Last name is required" ))
        |> Verify.verify .username maybeBlank



--myValidator : Validator MyUnvalidated MyError MyValidated
--myValidator =
--    Validator.succeed MyUnvalidated MyValidated
--        |> Validator.string .firstName Validator.notBlank
--        |> Validator.string .lastName Validator.notBlank
--        |> Validator.string .username Validator.maybeNotBlank
