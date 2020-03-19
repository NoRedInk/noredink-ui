module Nri.Ui.Pennant.V2 exposing (premiumFlag, disabledPremiumFlag)

{-| Used for indicating Premium content

@docs premiumFlag, disabledPremiumFlag

-}

import Nri.Ui.Svg.V1 exposing (Svg)
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attributes


{-| -}
premiumFlag : Svg
premiumFlag =
    pennant "#FEC709" <|
        Svg.path
            [ Attributes.fill "#146AFF"
            , Attributes.d "M7.5,3.8h4.2c1.1,0,1.9,0.3,2.5,0.8s0.9,1.2,0.9,2.1s-0.3,1.6-0.9,2.1c-0.6,0.5-1.4,0.8-2.5,0.8H9.3 v4.1H7.5V3.8z M11.5,8.1c0.6,0,1.1-0.1,1.4-0.4c0.3-0.3,0.5-0.6,0.5-1.1c0-0.5-0.2-0.9-0.5-1.1c-0.3-0.3-0.8-0.4-1.4-0.4H9.3v3 H11.5z"
            ]
            []


{-| -}
disabledPremiumFlag : Nri.Ui.Svg.V1.Svg
disabledPremiumFlag =
    pennant "#AAAAAA" <|
        Svg.svg [ Attributes.x "1.3", Attributes.y "1" ]
            [ Svg.path
                [ Attributes.fill "#fafafa"
                , Attributes.d "M6,3.2h3.1c0.8,0,1.4,0.2,1.9,0.6s0.7,0.9,0.7,1.6C11.7,6,11.4,6.6,11,7S9.9,7.6,9.1,7.6H6.9V11H6V3.2z M9,6.8 c0.6,0,1-0.1,1.3-0.4c0.3-0.2,0.4-0.6,0.4-1.1c0-0.5-0.1-0.8-0.4-1.1C10,4,9.6,3.9,9,3.9H6.9v2.9H9z"
                ]
                []
            ]


pennant : String -> Svg.Svg Never -> Nri.Ui.Svg.V1.Svg
pennant backgroundColor letter =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.viewBox "0 0 25 16"
        ]
        [ Svg.polygon
            [ Attributes.fill backgroundColor
            , Attributes.points "12.7,0 0,0 0,13.8 0,15.8 0,17.5 7.3,17.5 24.8,17.5 19.4,8.1 24.8,0 "
            ]
            []
        , letter
        ]
        |> Nri.Ui.Svg.V1.fromHtml
