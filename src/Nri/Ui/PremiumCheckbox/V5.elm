module Nri.Ui.PremiumCheckbox.V5 exposing (PremiumConfig, premium, Pennant(..))

{-|

@docs PremiumConfig, premium, Pennant

-}

import Accessibility.Styled as Html
import Css exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Checkbox.V5 as Checkbox
import Svg exposing (..)
import Svg.Attributes exposing (..)


{-|

  - `onChange`: A message for when the user toggles the checkbox
  - `onLockedClick`: A message for when the user clicks a checkbox they don't have PremiumLevel for.
    If you get this message, you should show an `Nri.Ui.Premium.Model.view`

-}
type alias PremiumConfig msg =
    { label : String
    , id : String
    , selected : Checkbox.IsSelected
    , disabled : Bool
    , isLocked : Bool
    , pennant : Maybe Pennant
    , onChange : Bool -> msg
    , onLockedClick : msg
    }


{-| Premium is the yellow "P" pennant
PremiumWithWriting is the yellow "P+" pennant
-}
type Pennant
    = Premium
    | PremiumWithWriting


{-| A checkbox that should be used for premium content
-}
premium : PremiumConfig msg -> Html.Html msg
premium config =
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
            Just pennant ->
                Html.div
                    [ Attributes.class "premium-checkbox-V5__PremiumClass"
                    , css
                        [ property "content" "''"
                        , Css.display inlineBlock
                        , Css.width (px 26)
                        , Css.height (px 24)
                        , marginLeft (px 8)

                        -- , backgroundImage
                        --     (case pennant of
                        --         Premium ->
                        --             assets.iconPremiumFlag_svg
                        --         PremiumWithWriting ->
                        --             assets.iconPremiumWithWritingFlag_svg
                        --     )
                        , backgroundRepeat noRepeat
                        , backgroundPosition center
                        ]
                    ]
                    []

            Nothing ->
                Html.text ""
        ]


premiumFlag : Svg msg
premiumFlag =
    svg
        [ version "1.1"
        , id "Layer_1"
        , x "0px"
        , y "0px"
        , Svg.Attributes.viewBox "0 0 25 19"
        , Svg.Attributes.style "enable-background:new 0 0 25 19;"
        ]
        [ Svg.style [] [ text " .st0{fill:#FEC709;} .st1{fill:#146AFF;} " ]
        , g [ id "Page-1" ]
            [ g [ id "icon_x2F_p-mini-pennant-yellow", Svg.Attributes.transform "translate(0.000000, -3.000000)" ]
                [ g
                    [ id "Group"
                    , Svg.Attributes.transform "translate(0.000000, 3.750000)"
                    ]
                    [ polygon
                        [ id "Fill-2"
                        , class "st0"
                        , points "12.7,0 0,0 0,13.8 0,15.8 0,17.5 7.3,17.5 24.8,17.5 19.4,8.1 24.8,0 "
                        ]
                        []
                    , Svg.path
                        [ id "P"
                        , class "st1"
                        , d "M7.5,3.8h4.2c1.1,0,1.9,0.3,2.5,0.8s0.9,1.2,0.9,2.1s-0.3,1.6-0.9,2.1c-0.6,0.5-1.4,0.8-2.5,0.8H9.3 v4.1H7.5V3.8z M11.5,8.1c0.6,0,1.1-0.1,1.4-0.4c0.3-0.3,0.5-0.6,0.5-1.1c0-0.5-0.2-0.9-0.5-1.1c-0.3-0.3-0.8-0.4-1.4-0.4H9.3v3 H11.5z"
                        ]
                        []
                    ]
                ]
            ]
        ]
