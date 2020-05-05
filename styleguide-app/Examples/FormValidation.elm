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


{-| -}
type alias State =
    { formData : UnvalidatedForm
    }


init : State
init =
    { formData =
        { firstName = ""
        , lastName = ""
        , username = ""
        }
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
            [ FormValidation.view getString OnInput [] model.formData <|
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
            ( model, Cmd.none )


type FormField
    = FirstName
    | LastName
    | Username


type alias UnvalidatedForm =
    { firstName : String
    , lastName : String
    , username : String
    }
