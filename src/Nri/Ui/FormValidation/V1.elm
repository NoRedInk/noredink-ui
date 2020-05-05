module Nri.Ui.FormValidation.V1 exposing (view)

{-| Our standard form validation flow.
See <https://paper.dropbox.com/doc/yes-Reusable-form-validation-in-Elm--AzJTO9829eQ201tVMhduNzjaAg-BylOxNMa6GEIpbP59qdZx>

@docs view

-}

import Accessibility.Styled exposing (Html)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.TextInput.V6 as TextInput


{-| Used to render the form.
-}
view :
    (field -> unvalidated -> String)
    -> (field -> String -> msg)
    -> List ( field, String )
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
            List.filter (Tuple.first >> (==) field) errors
                -- TODO: support multiple errors
                |> List.head
                |> Maybe.map Tuple.second
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
                    ([ if errors == [] then
                        Button.unfulfilled

                       else
                        Button.error
                     , Button.onClick onClick
                     ]
                        ++ attr
                    )
        }
