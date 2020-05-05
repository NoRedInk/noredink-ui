module Nri.Ui.FormValidation.V1 exposing (view)

{-| Our standard form validation flow.
See <https://paper.dropbox.com/doc/yes-Reusable-form-validation-in-Elm--AzJTO9829eQ201tVMhduNzjaAg-BylOxNMa6GEIpbP59qdZx>

@docs view

-}

import Accessibility.Styled exposing (Html)
import Nri.Ui.Button.V10 as Button


{-| Used to render the form.
-}
view :
    List error
    ->
        ({ submitButton :
            String -> msg -> List (Button.Attribute msg) -> Html msg
         }
         -> Html msg
        )
    -> Html msg
view errors viewForm =
    viewForm
        { submitButton =
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
