module Nri.Ui.FormValidation.V1 exposing
    ( FormState, init
    , submit
    , FormDefinition, form, FormInput, textInput
    , view
    )

{-| Our standard form validation flow.
See <https://paper.dropbox.com/doc/yes-Reusable-form-validation-in-Elm--AzJTO9829eQ201tVMhduNzjaAg-BylOxNMa6GEIpbP59qdZx>

@docs FormState, init
@docs submit
@docs FormDefinition, form, FormInput, textInput
@docs view

-}

import Accessibility.Styled exposing (Html)
import AssocList as Dict exposing (Dict)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.TextInput.V6 as TextInput
import Verify exposing (Validator)


{-| The private state of the FormValidation.
Create this with `init`,
and update it with `submit`.
You will need to pass this to `view` when you call it.

This data structure will not contain functions,
and is safe to store in your model.

-}
type FormState field
    = FormState
        -- NOTE: do not add functions to this record; it needs to remain safe to store in the caller's Model and for Elm to compare with (==)
        { showErrors : Bool
        , isSubmitting : Bool

        --{ showErrors : Set field
        --, showErrors : Dict field -> Animator Bool -- for animation
        }


{-| Creates the `FormState`. See [`FormState`](#FormState).
-}
init : FormState field
init =
    FormState
        { showErrors = False
        , isSubmitting = False
        }


{-| Use this in your update function when you get the Msg
produced by the "Submit" button of your form.
-}
submit :
    Validator ( field, String ) unvalidated validated
    -> unvalidated
    -> FormState field
    -> FormState field
submit validator formData (FormState formState) =
    case validator formData of
        Err _ ->
            FormState { formState | showErrors = True }

        Ok _ ->
            FormState { formState | isSubmitting = True }


{-| NOTE: this type internally contains functions, and thus should not be stored in your Model.

See [`form`](#form).

-}
type FormDefinition unvalidated field
    = FormDefinition
        { textInputs : Dict field (TextInputConfig unvalidated)
        }


{-| A form input provided to [`form`](#form).

Available options are: [`textInput`](#textInput).

-}
type FormInput unvalidated field
    = TextInput field (TextInputConfig unvalidated)


{-| PRIVATE
-}
type alias TextInputConfig unvalidated =
    { isRequired : Bool
    , getString : unvalidated -> String
    }


{-| A `TextInput.V6` input.
-}
textInput : Bool -> field -> (unvalidated -> String) -> FormInput unvalidated field
textInput isRequired field getString =
    TextInput field
        { isRequired = isRequired
        , getString = getString
        }


{-| Builds a `FormDefinition`. You need this for calling [`view`](#view).

NOTE: the resulting `FormDefinition` internally contains functions, and thus should not be stored in your Model.

-}
form : List (FormInput unvalidated field) -> FormDefinition unvalidated field
form inputs =
    let
        justTextInput input =
            case input of
                TextInput field config ->
                    Just ( field, config )
    in
    FormDefinition
        { textInputs =
            List.filterMap justTextInput inputs
                |> Dict.fromList
        }


{-| Used to render the form.
-}
view :
    FormDefinition unvalidated field
    -> (field -> String -> msg)
    -> Validator ( field, String ) unvalidated validated
    -> FormState field
    -> unvalidated
    ->
        ({ textInput :
            field -> String -> List (TextInput.Attribute msg) -> Html msg
         , submitButton :
            String -> msg -> List (Button.Attribute msg) -> Html msg
         }
         -> Html msg
        )
    -> Html msg
view (FormDefinition formDefinition) onInput validator (FormState formState) formData viewForm =
    let
        getString field =
            case Dict.get field formDefinition.textInputs of
                Just config ->
                    config.getString formData

                Nothing ->
                    -- NOTE: this could be avoided by having `form` take a function (field -> ...) instead of a list.
                    --       But would that API be harder to use?
                    "(Internal error: the FormDefinition does not include the requested field)"

        isRequiredAndBlank textInputConfig =
            textInputConfig.isRequired
                && (String.trim (textInputConfig.getString formData) == "")

        missingRequiredFields =
            List.any isRequiredAndBlank (Dict.values formDefinition.textInputs)

        errors =
            if formState.showErrors then
                case validator formData of
                    Ok _ ->
                        Dict.empty

                    Err ( first, rest ) ->
                        Dict.fromList (first :: rest)

            else
                Dict.empty

        errorFor field =
            -- TODO: support multiple errors
            Dict.get field errors
    in
    viewForm
        { textInput =
            \field label attr ->
                TextInput.view label
                    (TextInput.text (onInput field))
                    (List.filterMap identity
                        [ Just <| TextInput.errorMessage (errorFor field)
                        , if formState.isSubmitting then
                            Just TextInput.loading

                          else
                            Nothing
                        ]
                        ++ attr
                    )
                    (getString field)
        , submitButton =
            \label onClick attr ->
                Button.button label
                    ([ if formState.isSubmitting then
                        Button.loading

                       else if not (Dict.isEmpty errors) then
                        Button.error

                       else if missingRequiredFields then
                        Button.unfulfilled

                       else
                        Button.enabled
                     , Button.onClick onClick
                     ]
                        ++ attr
                    )
        }
