module Nri.Ui.PremiumCheckbox.V6 exposing (view)

{-|

@docs view

This module is used when there may or may not be Premium
content to be "checked"!


# Patch changes

  - Use Nri.Ui.Pennant.V2.premiumFlag instead of Nri.Ui.Pennant.V1.premiumFlag


# Changes from V5

  - Allow checkbox to show pennant, or not, based on bool
  - Remove PremiumWithWriting, it's only Premium now

-}

import Accessibility.Styled as Html exposing (Html)
import Css
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Checkbox.V5 as Checkbox
import Nri.Ui.Pennant.V2 exposing (premiumFlag)
import Nri.Ui.Svg.V1 as Svg


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
            [ Css.displayFlex
            , Css.alignItems Css.center
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
                |> Svg.withLabel "Premium"
                |> Svg.withWidth (Css.px 25)
                |> Svg.withHeight (Css.px 30)
                |> Svg.withCss [ Css.marginLeft (Css.px 8) ]
                |> Svg.toHtml

          else
            Html.text ""
        ]
