module Nri.Ui.FormValidation.V1 exposing
    ( FormState, init
    , onInput, submit, reset
    , FormDefinition, form, FormInput, textInput
    , view
    )

{-| Our standard form validation flow.
See <https://paper.dropbox.com/doc/yes-Reusable-form-validation-in-Elm--AzJTO9829eQ201tVMhduNzjaAg-BylOxNMa6GEIpbP59qdZx>

@docs FormState, init
@docs onInput, submit, reset
@docs FormDefinition, form, FormInput, textInput
@docs view

-}

import Accessibility.Styled exposing (Html)
import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
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
        { showErrors : Set field
        , isSubmitting : Bool

        --, showErrors : Dict field -> Animator Bool -- for animation
        }


{-| Creates the `FormState`. See [`FormState`](#FormState).
-}
init : FormState field
init =
    FormState
        { showErrors = Set.empty
        , isSubmitting = False
        }


{-| Use this in your update function when you get the Msg
produced by any of the validated fields in your form.
-}
onInput :
    Validator ( field, String ) unvalidated validated
    -> unvalidated
    -> FormState field
    -> FormState field
onInput validator newFormData (FormState formState) =
    let
        nowInvalidFields =
            case validator newFormData of
                Err ( first, rest ) ->
                    (first :: rest)
                        |> List.map Tuple.first
                        |> Set.fromList

                Ok _ ->
                    Set.empty
    in
    FormState
        { formState
            | showErrors = Set.intersect nowInvalidFields formState.showErrors
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
        Err ( first, rest ) ->
            FormState
                { formState
                    | showErrors =
                        (first :: rest)
                            |> List.map Tuple.first
                            |> Set.fromList
                }

        Ok _ ->
            FormState { formState | isSubmitting = True }


{-| Resets the form state.
This can be used to re-enable the form after it has been submitted
(and after the server has responded to the submission).
It is also common to not need `reset`,
for cases when the form or the modal containing the form
completely go away after the server responds successfully.

NOTE: This is independent of clearing the form data (current values) in the form.
You may want to reset the initial values of the fields at the same time,
but you don't have to if you don't want to.

-}
reset : FormState field
reset =
    -- NOTE: currently, reset is the same as init, but they are separate
    --       definitions because they are distinct events to the caller.
    --       It's also possible that reset and init might diverge to be slightly different in the future
    init


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
            { activeLabel : String
            , submittingLabel : String
            , onClick : msg
            }
            -> List (Button.Attribute msg)
            -> Html msg
         }
         -> Html msg
        )
    -> Html msg
view (FormDefinition formDefinition) onInput_ validator (FormState formState) formData viewForm =
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
            if Set.isEmpty formState.showErrors then
                Dict.empty

            else
                case validator formData of
                    Ok _ ->
                        Dict.empty

                    Err ( first, rest ) ->
                        Dict.fromList (first :: rest)

        errorFor field =
            if Set.member field formState.showErrors then
                -- TODO: support multiple errors
                Dict.get field errors

            else
                Nothing
    in
    viewForm
        { textInput =
            \field label attr ->
                TextInput.view label
                    (TextInput.text (onInput_ field))
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
            \config attr ->
                Button.button
                    (if formState.isSubmitting then
                        -- To ensure nice typography, we convert "..." to "…"
                        -- because that's a common mistake people make
                        String.replace "..." "…" config.submittingLabel

                     else
                        config.activeLabel
                    )
                    ([ if formState.isSubmitting then
                        Button.loading

                       else if not (Dict.isEmpty errors) then
                        Button.error

                       else if missingRequiredFields then
                        Button.unfulfilled

                       else
                        Button.enabled
                     , Button.onClick config.onClick
                     ]
                        ++ attr
                    )
        }
