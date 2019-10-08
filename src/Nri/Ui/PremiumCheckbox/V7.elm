module Nri.Ui.PremiumCheckbox.V7 exposing (view)

{-|

@docs view

This module is used when there may or may not be Premium
content to be "checked"!


# Changes from V6

  - Compatibility with Nri.Ui.Checkbox.V6

-}

import Accessibility.Styled as Html exposing (Html)
import Css exposing (..)
import Html.Styled exposing (fromUnstyled)
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Checkbox.V6 as Checkbox
import Svg exposing (..)
import Svg.Attributes exposing (..)


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


premiumFlag : Html msg
premiumFlag =
    svg
        [ version "1.1"
        , id "Layer_1"
        , Svg.Attributes.width "25"
        , Svg.Attributes.height "19"
        , Svg.Attributes.viewBox "0 0 25 19"
        , Svg.Attributes.style "margin-left: 8px;"
        ]
        [ Svg.title [] [ text "Premium" ]
        , Svg.style [] [ text " .premium-flag-st0{fill:#FEC709;} .premium-flag-st1{fill:#146AFF;} " ]
        , g [ id "Page-1" ]
            [ g
                [ id "icon_x2F_p-mini-pennant-yellow"
                , Svg.Attributes.transform "translate(0.000000, -3.000000)"
                ]
                [ g
                    [ id "Group"
                    , Svg.Attributes.transform "translate(0.000000, 3.750000)"
                    ]
                    [ polygon
                        [ id "Fill-2"
                        , class "premium-flag-st0"
                        , points "12.7,0 0,0 0,13.8 0,15.8 0,17.5 7.3,17.5 24.8,17.5 19.4,8.1 24.8,0 "
                        ]
                        []
                    , Svg.path
                        [ id "P"
                        , class "premium-flag-st1"
                        , d "M7.5,3.8h4.2c1.1,0,1.9,0.3,2.5,0.8s0.9,1.2,0.9,2.1s-0.3,1.6-0.9,2.1c-0.6,0.5-1.4,0.8-2.5,0.8H9.3 v4.1H7.5V3.8z M11.5,8.1c0.6,0,1.1-0.1,1.4-0.4c0.3-0.3,0.5-0.6,0.5-1.1c0-0.5-0.2-0.9-0.5-1.1c-0.3-0.3-0.8-0.4-1.4-0.4H9.3v3 H11.5z"
                        ]
                        []
                    ]
                ]
            ]
        ]
        |> fromUnstyled
