module Nri.Ui.FormValidation.V1 exposing (view)

{-| Our standard form validation flow.
See <https://paper.dropbox.com/doc/yes-Reusable-form-validation-in-Elm--AzJTO9829eQ201tVMhduNzjaAg-BylOxNMa6GEIpbP59qdZx>

@docs view

-}

import Accessibility.Styled exposing (Html)


{-| Used to render the form.
-}
view :
    ({}
     -> Html msg
    )
    -> Html msg
view viewForm =
    viewForm {}
