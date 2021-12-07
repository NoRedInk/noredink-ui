module Nri.Ui.PremiumCheckbox.V7 exposing (view)

{-|

@docs view

This module is used when there may or may not be Premium
content to be "checked"!


# Changes from V6

  - Move the Premium pennant to the left of the checkbox

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
        [ if config.isPremium then
            premiumFlag
                |> Svg.withLabel "Premium"
                |> Svg.withWidth (Css.px iconWidth)
                |> Svg.withHeight (Css.px 30)
                |> Svg.withCss [ Css.marginRight (Css.px iconRightMargin) ]
                |> Svg.toHtml

          else
            -- left-align the checkbox with checkboxes that _do_ have the premium pennant
            Html.div [ css [ Css.width (Css.px (iconWidth + iconRightMargin)) ] ] []
        , Checkbox.viewWithLabel
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
        ]


iconWidth : Float
iconWidth =
    25


iconRightMargin : Float
iconRightMargin =
    8
