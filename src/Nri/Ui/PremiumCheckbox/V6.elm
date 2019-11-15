module Nri.Ui.PremiumCheckbox.V6 exposing (view)

{-|

@docs view

This module is used when there may or may not be Premium
content to be "checked"!


# Changes from V5

  - Allow checkbox to show pennant, or not, based on bool
  - Remove PremiumWithWriting, it's only Premium now

-}

import Accessibility.Styled as Html exposing (Html)
import Css exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Checkbox.V5 as Checkbox
import Nri.Ui.Pennant.V1 exposing (premiumFlag)


{-| A checkbox that should be used for premium content

  - `onChange`: A message for when the user toggles the checkbox
  - `onLockedClick`: A message for when the user clicks a checkbox they don't have PremiumLevel for.
    If you get this message, you should show an `Nri.Ui.Premium.Model.view`

-}
view :
    { label : String
    , id : String
    , selected : Checkbox.IsSelected
    , disabled : Bool
    , isLocked : Bool
    , isPremium : Bool
    , onChange : Bool -> msg
    , onLockedClick : msg
    }
    -> Html msg
view config =
    Html.div
        [ css
            [ displayFlex
            , alignItems center
            ]
        ]
        [ Checkbox.viewWithLabel
            { identifier = config.id
            , label = config.label
            , setterMsg =
                if config.isLocked then
                    \_ -> config.onLockedClick

                else
                    config.onChange
            , selected = config.selected
            , disabled = config.disabled
            , theme =
                if config.isLocked then
                    Checkbox.Locked

                else
                    Checkbox.Square
            }
        , if config.isPremium then
            premiumFlag

          else
            Html.text ""
        ]
