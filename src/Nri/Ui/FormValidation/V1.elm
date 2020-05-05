module Nri.Ui.FormValidation.V1 exposing
    ( FormState, init
    , submit
    , view
    )

{-| Our standard form validation flow.
See <https://paper.dropbox.com/doc/yes-Reusable-form-validation-in-Elm--AzJTO9829eQ201tVMhduNzjaAg-BylOxNMa6GEIpbP59qdZx>

@docs FormState, init
@docs submit
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

        --{ showErrors : Set field
        --, showErrors : Dict field -> Animator Bool -- for animation
        }


{-| Creates the `FormState`. See [`FormState`](#FormState).
-}
init : FormState field
init =
    FormState
        { showErrors = False
        }


{-| Use this in your update function when you get the Msg
produced by the "Submit" button of your form.
-}
submit : FormState field -> FormState field
submit (FormState formState) =
    FormState { formState | showErrors = True }


{-| Used to render the form.
-}
view :
    (field -> unvalidated -> String)
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
view getString onInput validator (FormState formState) formData viewForm =
    let
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
                    ([ TextInput.errorMessage (errorFor field)
                     ]
                        ++ attr
                    )
                    (getString field formData)
        , submitButton =
            \label onClick attr ->
                Button.button label
                    ([ if Dict.isEmpty errors then
                        Button.unfulfilled

                       else
                        Button.error
                     , Button.onClick onClick
                     ]
                        ++ attr
                    )
        }
