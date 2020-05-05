module Spec.Nri.Ui.FormValidation.V1 exposing (all)

import Accessibility.Styled as Html
import Nri.Ui.Button.V10 as Button
import Nri.Ui.FormValidation.V1 as FormValidation
import Nri.Ui.TextInput.V6 as TextInput
import ProgramTest exposing (expectViewHas)
import Test exposing (..)
import Test.Html.Selector exposing (text)


type alias ProgramTest =
    ProgramTest.ProgramTest FormModel FormMsg ()


type alias FormModel =
    { formData : UnvalidatedForm
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


type FormMsg
    = OnInput FormField String


start : ProgramTest
start =
    let
        init =
            { formData =
                { firstName = ""
                , lastName = ""
                , username = ""
                }
            }

        update msg model =
            case msg of
                OnInput field newString ->
                    let
                        formData =
                            model.formData
                    in
                    { model
                        | formData =
                            case field of
                                FirstName ->
                                    { formData | firstName = newString }

                                LastName ->
                                    { formData | lastName = newString }

                                Username ->
                                    { formData | username = newString }
                    }

        view model =
            FormValidation.view <|
                \_ ->
                    Html.div
                        []
                        [ Html.text "Form heading"
                        , TextInput.view "First name"
                            (TextInput.text (OnInput FirstName))
                            []
                            model.formData.firstName
                        , TextInput.view "Last name"
                            (TextInput.text (OnInput LastName))
                            []
                            model.formData.lastName
                        , TextInput.view "Username"
                            (TextInput.text (OnInput Username))
                            []
                            model.formData.username
                        , Button.button "Submit"
                            [ Button.disabled
                            ]
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
        ]
