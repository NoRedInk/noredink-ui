module Nri.Ui.BorderlessButton.V1 exposing (button)

import Html.Styled as Html exposing (..)


button : String -> Html msg
button label =
    Html.button [] [ text label ]
