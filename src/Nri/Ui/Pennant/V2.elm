module Nri.Ui.Pennant.V2 exposing (premiumFlag, disabledPremiumFlag, expiredPremiumFlag)

{-| Used for indicating Premium content

@docs premiumFlag, disabledPremiumFlag, expiredPremiumFlag

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


{-| -}
expiredPremiumFlag : Nri.Ui.Svg.V1.Svg
expiredPremiumFlag =
    pennant "#F3336C" <|
        Svg.svg [ Attributes.y "-1" ]
            [ Svg.path
                [ Attributes.fill "#FFFFFF"
                , Attributes.d "M10,3.5 C13.31368,3.5 16,6.18632 16,9.5 C16,12.81368 13.31368,15.5 10,15.5 C6.68632,15.5 4,12.81368 4,9.5 C4,6.18632 6.68632,3.5 10,3.5 Z M10,4.7 C7.34908,4.7 5.2,6.84908 5.2,9.5 C5.2,12.15104 7.34908,14.3 10,14.3 C12.65104,14.3 14.8,12.15104 14.8,9.5 C14.8,6.84908 12.65104,4.7 10,4.7 Z M9.89064763,6.33193173 C10.2054819,6.33193173 10.4635792,6.62287155 10.4886511,6.99277328 C10.4894733,7.00490368 10.4900449,7.017119 10.4903597,7.02941176 L10.49,9.913 L12.0720499,10.5386187 C12.452437,10.654475 12.6546988,11.0970991 12.5752647,11.3581417 C12.4958305,11.6191843 12.08127,11.8740149 11.7009139,11.7582745 L9.89981773,11.0462509 C9.67584768,10.9780353 9.50566415,10.8228363 9.42448547,10.637987 C9.34254176,10.5169161 9.29064764,10.3645616 9.29064764,10.1992128 L9.29064763,7.05193173 C9.29064763,6.65437173 9.55920763,6.33193173 9.89064763,6.33193173 Z"
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
