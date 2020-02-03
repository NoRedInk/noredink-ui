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
import Nri.Ui.Checkbox.V6 as Checkbox
import Nri.Ui.Pennant.V1 exposing (premiumFlag)


{-| A checkbox that should be used for premium content

  - `onChange`: A message for when the user toggles the checkbox
  - `onLockedClick`: A message for when the user clicks a checkbox they don't have PremiumLevel for.
    If you get this message, you should show an `Nri.Ui.Premium.Model.view`

-}
view :
    { label : String
    , id : String
    , selected : Checkbox.Selected
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
        [ if config.isLocked then
            Checkbox.locked
                { identifier = config.id
                , label = config.label
                , anonymous = True
                , onClick = config.onLockedClick
                }
          else if config.disabled then
            Checkbox.disabled
                { identifier = config.id
                , label = config.label
                , anonymous = True
                , selected = config.selected
                }
          else
            Checkbox.checkbox
                { identifier = config.id
                , label = config.label
                , anonymous = True
                , onChange = config.onChange
                , selected = config.selected
                }
        , if config.isPremium then
            premiumFlag

          else
            Html.text ""
        ]
