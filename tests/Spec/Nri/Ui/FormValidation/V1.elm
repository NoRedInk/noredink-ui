module Spec.Nri.Ui.FormValidation.V1 exposing (all)

import Accessibility.Styled as Html
import Expect exposing (Expectation)
import Html.Attributes
import Nri.Ui.FormValidation.V1 as FormValidation
import Nri.Ui.TextInput.V6 as TextInput
import ProgramTest exposing (..)
import String.Verify
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)
import Verify exposing (Validator)


type alias ProgramTest =
    ProgramTest.ProgramTest FormModel FormMsg ()


type alias FormModel =
    { formData : UnvalidatedForm
    , formState : FormValidation.FormState FormField
    }


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


type FormMsg
    = OnInput FormField String
    | SubmitForm
    | ResetFormState


start : ProgramTest
start =
    let
        init =
            { formData =
                { firstName = ""
                , lastName = ""
                , username = ""
                }
            , formState = FormValidation.init
            }

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
                    { model
                        | formData = newFormData
                        , formState = FormValidation.onInput validator newFormData model.formState
                    }

                SubmitForm ->
                    { model | formState = FormValidation.submit validator model.formData model.formState }

                ResetFormState ->
                    -- NOTE: this doesn't clear the form data
                    { model | formState = FormValidation.reset }

        view model =
            let
                formDefinition =
                    FormValidation.form
                        [ FormValidation.textInput True FirstName .firstName
                        , FormValidation.textInput True LastName .lastName
                        , FormValidation.textInput False Username .username
                        ]
            in
            FormValidation.view formDefinition OnInput validator model.formState model.formData <|
                \form ->
                    Html.div
                        []
                        [ Html.text "Form heading"
                        , form.textInput FirstName "First name" []
                        , form.textInput LastName "Last name" []
                        , form.textInput Username "Username" []
                        , form.submitButton
                            { activeLabel = "Submit"
                            , submittingLabel = "Saving..."
                            , onClick = SubmitForm
                            }
                            []
                        ]
    in
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = view >> Html.toUnstyled
        }
        |> ProgramTest.start ()


all : Test
all =
    describe "Nri.Ui.FormValidation.V1"
        [ test "renders the provided form" <|
            \() ->
                start
                    |> expectViewHas [ text "Form heading" ]
        , test "submit button starts unfulfilled if there are required fields" <|
            \() ->
                start
                    |> expectButtonState "Submit" "unfulfilled"
        , test "clicking the unfulfilled submit button shows validation errors" <|
            \() ->
                start
                    |> clickButton "Submit"
                    |> expectViewHas [ text "First name is required" ]
        , test "submit button is enabled when all required fields are not blank" <|
            \() ->
                start
                    |> fillIn (TextInput.generateId "First name") "First name" "Jeffy"
                    |> fillIn (TextInput.generateId "Last name") "Last name" "Yffy"
                    |> expectButtonState "Submit" "enabled"
        , test "form starts without validation errors" <|
            \() ->
                start
                    |> expectViewHasNot [ text "First name is required" ]
        , test "clicking submit does not show errors for valid fields" <|
            \() ->
                start
                    |> fillIn (TextInput.generateId "First name") "First name" "Jeffy"
                    |> clickButton "Submit"
                    |> expectViewHasNot [ text "First name is required" ]
        , test "submitting with validation errors puts the button into error state" <|
            \() ->
                start
                    |> fillIn (TextInput.generateId "First name") "First name" " "
                    |> fillIn (TextInput.generateId "Last name") "Last name" "   "
                    |> clickButton "Submit"
                    |> expectButtonState "Submit" "error"
        , test "button is in the loading state after submitting" <|
            \() ->
                start
                    |> fillIn (TextInput.generateId "First name") "First name" "Balthazar"
                    |> fillIn (TextInput.generateId "Last name") "Last name" "Dough, Jr."
                    |> clickButton "Submit"
                    |> expectButtonState "Saving…" "loading"
        , test "textInputs are in the loading state after submitting" <|
            \() ->
                start
                    |> fillIn (TextInput.generateId "First name") "First name" "Balthazar"
                    |> fillIn (TextInput.generateId "Last name") "Last name" "Dough, Jr."
                    |> clickButton "Submit"
                    |> expectTextInputState "First name" "loading"
        , test "form can be reset after submitting" <|
            \() ->
                start
                    |> fillIn (TextInput.generateId "First name") "First name" "Balthazar"
                    |> fillIn (TextInput.generateId "Last name") "Last name" "Dough, Jr."
                    |> clickButton "Submit"
                    |> ProgramTest.update ResetFormState
                    |> expectButtonState "Submit" "enabled"
        , test "a field that's corrected stops being validated until the submit button is pressed again" <|
            \() ->
                start
                    |> clickButton "Submit"
                    |> ensureViewHas [ text "First name is required" ]
                    |> fillIn (TextInput.generateId "First name") "First name" "Balthazar"
                    |> ensureViewHasNot [ text "First name is required" ]
                    |> fillIn (TextInput.generateId "First name") "First name" ""
                    |> expectViewHasNot [ text "First name is required" ]
        ]


expectButtonState : String -> String -> ProgramTest.ProgramTest model msg effect -> Expectation
expectButtonState label stateString =
    expectView
        (Query.find [ tag "button", containing [ text label ] ]
            >> Query.has [ attribute (Html.Attributes.attribute "data-nri-button-state" stateString) ]
        )


expectTextInputState : String -> String -> ProgramTest.ProgramTest model msg effect -> Expectation
expectTextInputState label stateString =
    expectView
        (Query.find
            [ tag "input"
            , id (TextInput.generateId label)
            ]
            >> Query.has
                [ attribute (Html.Attributes.attribute "data-nri-input-state" stateString)
                ]
        )
