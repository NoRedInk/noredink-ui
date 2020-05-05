module Nri.Ui.FormValidation.V1 exposing (view)

{-| Our standard form validation flow.
See <https://paper.dropbox.com/doc/yes-Reusable-form-validation-in-Elm--AzJTO9829eQ201tVMhduNzjaAg-BylOxNMa6GEIpbP59qdZx>

@docs view

-}

import Accessibility.Styled exposing (Html)
import AssocList as Dict exposing (Dict)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.TextInput.V6 as TextInput


{-| Used to render the form.
-}
view :
    (field -> unvalidated -> String)
    -> (field -> String -> msg)
    -> Dict field String
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
view getString onInput errors formData viewForm =
    let
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
