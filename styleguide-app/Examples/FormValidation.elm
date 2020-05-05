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
                getString field =
                    case field of
                        FirstName ->
                            .firstName

                        LastName ->
                            .lastName

                        Username ->
                            .username
            in
            [ FormValidation.view getString OnInput validator model.formState model.formData <|
                \form ->
                    styled div
                        [ padding (px 25)
                        , border3 (px 3) dashed Colors.gray75
                        , borderRadius (px 10)
                        , maxWidth (px 250)
                        ]
                        []
                        [ form.textInput FirstName
                            "First name"
                            []
                        , form.textInput LastName
                            "Last name"
                            [ TextInput.css [ marginTop (px 10) ] ]
                        , form.textInput Username
                            "Username"
                            [ TextInput.css [ marginTop (px 10) ] ]
                        , form.submitButton "Submit"
                            SubmitForm
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
            in
            ( { model
                | formData =
                    case field of
                        FirstName ->
                            { formData | firstName = newString }

                        LastName ->
                            { formData | lastName = newString }

                        Username ->
                            { formData | username = newString }
              }
            , Cmd.none
            )

        SubmitForm ->
            ( { model | formState = FormValidation.submit model.formState }
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
