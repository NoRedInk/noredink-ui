module Nri.Ui.PremiumCheckbox.V6 exposing (view, Pennant(..))

{-|

@docs view, Pennant

-}

import Accessibility.Styled as Html exposing (Html)
import Css exposing (..)
import Html.Styled exposing (fromUnstyled)
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Checkbox.V6 as Checkbox
import Svg exposing (..)
import Svg.Attributes exposing (..)


{-| Changes from V6:

  - inherited changes from Nri.Ui.Checkbox.V6
      - adjusts padding for all checkboxes to accommodate highlight style.
        Keep this in mind if using this module on an existing page -- make sure alignement, styling is consistent.
        Premium is the yellow "P" pennant
        PremiumWithWriting is the yellow "P+" pennant

-}
type Pennant
    = Premium
    | PremiumWithWriting


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
    , pennant : Pennant
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
        , case config.pennant of
            Premium ->
                premiumFlag

            PremiumWithWriting ->
                premiumWithWritingFlag
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


premiumWithWritingFlag : Html msg
premiumWithWritingFlag =
    svg
        [ Svg.Attributes.width "25"
        , Svg.Attributes.height "18"
        , Svg.Attributes.viewBox "0 0 25 18"
        , Svg.Attributes.style "margin-left: 8px;"
        ]
        [ Svg.title [] [ text "Premium with Writing" ]
        , g
            [ Svg.Attributes.fill "none"
            , fillRule "evenodd"
            ]
            [ Svg.path
                [ Svg.Attributes.fill "#FEC709"
                , d "M12.662 0H0v17.5h24.777l-5.384-9.423L24.777 0z"
                ]
                []
            , Svg.path
                [ Svg.Attributes.fill "#146AFF"
                , d "M2.5 3.75h4.249c1.054 0 1.874.254 2.461.763.587.509.88 1.203.88 2.083 0 .88-.296 1.577-.887 2.09-.591.514-1.41.77-2.454.77H4.274v4.084H2.5V3.75zm4.043 4.331c.614 0 1.079-.126 1.395-.378.316-.252.474-.616.474-1.093 0-.486-.155-.855-.467-1.107-.312-.252-.78-.378-1.402-.378h-2.27v2.956h2.27zM12.85 7h1.31c.152 0 .278.127.278.288V9.57h2.283c.152 0 .279.127.279.28v1.31a.281.281 0 0 1-.279.278h-2.283v2.283a.281.281 0 0 1-.278.279h-1.31a.281.281 0 0 1-.28-.279v-2.283h-2.282A.283.283 0 0 1 10 11.16V9.85c0-.153.127-.28.288-.28h2.282V7.288c0-.161.127-.288.28-.288z"
                ]
                []
            ]
        ]
        |> fromUnstyled
