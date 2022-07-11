module Nri.Ui.Pennant.V2 exposing (premiumFlag, disabledPremiumFlag, expiredPremiumFlag)

{-| Used for indicating Premium content

@docs premiumFlag, disabledPremiumFlag, expiredPremiumFlag

-}

import Nri.Ui.Colors.Extra exposing (toCssString)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Svg.V1 exposing (Svg)
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attributes


{-| -}
premiumFlag : Svg
premiumFlag =
    Nri.Ui.Svg.V1.init "0 0 25 18"
        [ Svg.g
            [ Attributes.stroke "none"
            , Attributes.strokeWidth "1"
            , Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            ]
            [ Svg.g
                [ Attributes.transform "translate(-39.000000, -420.000000)"
                ]
                [ Svg.g
                    [ Attributes.transform "translate(39.000000, 50.000000)"
                    ]
                    [ Svg.g
                        [ Attributes.transform "translate(0.000000, 370.000000)"
                        ]
                        [ Svg.polygon
                            [ Attributes.fill "#FEC709"
                            , Attributes.points "12.7757004 0 1.73472348e-16 0 1.73472348e-16 14.2404227 0 16.2706817 0 18 7.34267839 18 25 18 19.566978 9 25 0"
                            ]
                            []
                        , Svg.path
                            [ Attributes.d "M7.5,4.105 L11.74875,4.105 C12.8029219,4.105 13.6233304,4.35937246 14.21,4.868125 C14.7966696,5.37687754 15.09,6.0712456 15.09,6.95125 C15.09,7.8312544 14.794378,8.5279141 14.203125,9.04125 C13.611872,9.5545859 12.7937552,9.81125 11.74875,9.81125 L9.27375,9.81125 L9.27375,13.895 L7.5,13.895 L7.5,4.105 Z M11.5425,8.43625 C12.1566697,8.43625 12.6218734,8.31020959 12.938125,8.058125 C13.2543766,7.80604041 13.4125,7.44166905 13.4125,6.965 C13.4125,6.47916424 13.2566682,6.11020959 12.945,5.858125 C12.6333318,5.60604041 12.1658365,5.48 11.5425,5.48 L9.27375,5.48 L9.27375,8.43625 L11.5425,8.43625 Z"
                            , Attributes.fill "#146AFF"
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]


{-| -}
disabledPremiumFlag : Nri.Ui.Svg.V1.Svg
disabledPremiumFlag =
    Nri.Ui.Svg.V1.init "0 0 25 18"
        [ Svg.g
            [ Attributes.stroke "none"
            , Attributes.strokeWidth "1"
            , Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            ]
            [ Svg.g
                [ Attributes.transform "translate(-39.000000, -458.000000)"
                ]
                [ Svg.g
                    [ Attributes.transform "translate(39.000000, 50.000000)"
                    ]
                    [ Svg.g
                        [ Attributes.transform "translate(0.000000, 408.000000)"
                        ]
                        [ Svg.polygon
                            [ Attributes.fill "#BFBFBF"
                            , Attributes.points "12.7757004 0 1.73472348e-16 0 1.73472348e-16 14.2404227 0 16.2706817 0 18 7.34267839 18 25 18 19.566978 9 25 0"
                            ]
                            []
                        , Svg.path
                            [ Attributes.d "M7.5,4.105 L11.74875,4.105 C12.8029219,4.105 13.6233304,4.35937246 14.21,4.868125 C14.7966696,5.37687754 15.09,6.0712456 15.09,6.95125 C15.09,7.8312544 14.794378,8.5279141 14.203125,9.04125 C13.611872,9.5545859 12.7937552,9.81125 11.74875,9.81125 L9.27375,9.81125 L9.27375,13.895 L7.5,13.895 L7.5,4.105 Z M11.5425,8.43625 C12.1566697,8.43625 12.6218734,8.31020959 12.938125,8.058125 C13.2543766,7.80604041 13.4125,7.44166905 13.4125,6.965 C13.4125,6.47916424 13.2566682,6.11020959 12.945,5.858125 C12.6333318,5.60604041 12.1658365,5.48 11.5425,5.48 L9.27375,5.48 L9.27375,8.43625 L11.5425,8.43625 Z"
                            , Attributes.fill "#FFFFFF"
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]


{-| -}
expiredPremiumFlag : Nri.Ui.Svg.V1.Svg
expiredPremiumFlag =
    Nri.Ui.Svg.V1.init "0 0 25 18"
        [ Svg.g
            [ Attributes.stroke "none"
            , Attributes.strokeWidth "1"
            , Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            ]
            [ Svg.g
                [ Attributes.transform "translate(-39.000000, -496.000000)"
                ]
                [ Svg.g
                    [ Attributes.transform "translate(39.000000, 50.000000)"
                    ]
                    [ Svg.g
                        [ Attributes.transform "translate(0.000000, 446.000000)"
                        ]
                        [ Svg.polygon
                            [ Attributes.fill (toCssString Colors.red)
                            , Attributes.points "12.7757004 0 1.73472348e-16 0 1.73472348e-16 14.2404227 0 16.2706817 0 18 7.34267839 18 25 18 19.566978 9 25 0"
                            ]
                            []
                        , Svg.path
                            [ Attributes.d "M10,3 C13.31368,3 16,5.68632 16,9 C16,12.31368 13.31368,15 10,15 C6.68632,15 4,12.31368 4,9 C4,5.68632 6.68632,3 10,3 Z M10,4.2 C7.34908,4.2 5.2,6.34908 5.2,9 C5.2,11.65104 7.34908,13.8 10,13.8 C12.65104,13.8 14.8,11.65104 14.8,9 C14.8,6.34908 12.65104,4.2 10,4.2 Z M9.89064763,5.83193173 C10.2054819,5.83193173 10.4635792,6.12287155 10.4886511,6.49277328 C10.4894733,6.50490368 10.4900449,6.517119 10.4903597,6.52941176 L10.49,9.413 L12.0720499,10.0386187 C12.452437,10.154475 12.6546988,10.5970991 12.5752647,10.8581417 C12.4958305,11.1191843 12.08127,11.3740149 11.7009139,11.2582745 L9.89981773,10.5462509 C9.67584768,10.4780353 9.50566415,10.3228363 9.42448547,10.137987 C9.34254176,10.0169161 9.29064764,9.86456161 9.29064764,9.69921279 L9.29064763,6.55193173 C9.29064763,6.15437173 9.55920763,5.83193173 9.89064763,5.83193173 Z"
                            , Attributes.fill "#FFFFFF"
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]
