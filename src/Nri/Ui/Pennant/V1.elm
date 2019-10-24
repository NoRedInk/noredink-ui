module Nri.Ui.Pennant.V1 exposing (premiumFlag)

{-| Used for indicating Premium content
-}

import Accessibility.Styled exposing (Html)
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes as SvgAttr exposing (..)


{-| -}
premiumFlag : Html msg
premiumFlag =
    svg
        [ version "1.1"
        , id "Layer_1"
        , SvgAttr.width "25"
        , SvgAttr.height "19"
        , SvgAttr.viewBox "0 0 25 19"
        , SvgAttr.style "margin-left: 8px;"
        ]
        [ Svg.title [] [ text "Premium" ]
        , Svg.style [] [ text " .premium-flag-st0{fill:#FEC709;} .premium-flag-st1{fill:#146AFF;} " ]
        , g [ id "Page-1" ]
            [ g
                [ id "icon_x2F_p-mini-pennant-yellow"
                , SvgAttr.transform "translate(0.000000, -3.000000)"
                ]
                [ g
                    [ id "Group"
                    , SvgAttr.transform "translate(0.000000, 3.750000)"
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
