module Nri.Ui.BorderlessButton.V1 exposing (button)

import Css
import Html.Styled as Html exposing (..)
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1


button : String -> Html msg
button label =
    Nri.Ui.styled Html.button
        "borderless-button-v1"
        [ Css.cursor Css.pointer
        , -- Specifying the font can and should go away after bootstrap is removed from application.css
          Nri.Ui.Fonts.V1.baseFont
        , Css.backgroundImage Css.none
        , Css.textShadow Css.none
        , Css.property "transition" "background-color 0.2s, color 0.2s, box-shadow 0.2s, border 0.2s, border-width 0s"
        , Css.boxShadow Css.none
        , Css.border Css.zero
        , Css.disabled [ Css.cursor Css.notAllowed ]
        , Css.color Colors.azure
        , Css.backgroundColor Css.transparent
        , Css.fontWeight (Css.int 600)
        , Css.textAlign Css.left
        , Css.borderStyle Css.none
        , Css.hover [ Css.textDecoration Css.underline ]
        ]
        []
        [ text label ]
