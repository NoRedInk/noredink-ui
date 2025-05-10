module Nri.Ui.Pennant.V4 exposing (contentPremiumFlag, activePremiumFlag, inactivePremiumFlag, expiredPremiumFlag, giftPremiumFlag)

{-| Changes from V3:

    - icons are now flush with bounding box

Used for indicating Premium content

@docs contentPremiumFlag, activePremiumFlag, inactivePremiumFlag, expiredPremiumFlag, giftPremiumFlag

-}

import Nri.Ui.Svg.V1 exposing (Svg)
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attributes


{-| Pennant to indicate that particular content is premium only
-}
contentPremiumFlag : Svg
contentPremiumFlag =
    Nri.Ui.Svg.V1.init "0 0 25 25"
        [ Svg.path
            [ Attributes.d "M1.60942 3C0.720563 3 0 3.70888 0 4.58333V20.4167C0 21.2911 0.720562 22 1.60942 22H23.3875C24.752 22 25.4974 20.4343 24.6239 19.403L19.6352 13.5136C19.1378 12.9265 19.1378 12.0735 19.6352 11.4864L24.6239 5.59696C25.4974 4.56569 24.752 3 23.3875 3H1.60942Z"
            , Attributes.fill "#FEC709"
            , Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            ]
            []
        ]


{-| Pennant to indicate that premium status is active
-}
activePremiumFlag : Svg
activePremiumFlag =
    Nri.Ui.Svg.V1.init "0 0 25 25"
        [ Svg.path
            [ Attributes.d "M1.60942 3C0.720563 3 0 3.70888 0 4.58333V20.4167C0 21.2911 0.720562 22 1.60942 22H23.3875C24.752 22 25.4974 20.4343 24.6239 19.403L19.6352 13.5136C19.1378 12.9265 19.1378 12.0735 19.6352 11.4864L24.6239 5.59696C25.4974 4.56569 24.752 3 23.3875 3H1.60942Z"
            , Attributes.fill "#FEC709"
            , Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            ]
            []
        , Svg.path
            [ Attributes.d "M6.50806 13.5115L8.14674 15.1501L13.0628 10"
            , Attributes.stroke "#004E95"
            , Attributes.strokeWidth "1.5"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| Pennant to indicate that premium status is inactive
-}
inactivePremiumFlag : Nri.Ui.Svg.V1.Svg
inactivePremiumFlag =
    Nri.Ui.Svg.V1.init "0 0 25 25"
        [ Svg.path
            [ Attributes.d "M1.60942 3C0.720563 3 0 3.70888 0 4.58333V20.4167C0 21.2911 0.720562 22 1.60942 22H23.3875C24.752 22 25.4974 20.4343 24.6239 19.403L19.6352 13.5136C19.1378 12.9265 19.1378 12.0735 19.6352 11.4864L24.6239 5.59696C25.4974 4.56569 24.752 3 23.3875 3H1.60942Z"
            , Attributes.fill "#707070"
            , Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            ]
            []
        , Svg.path
            [ Attributes.d "M9.6564 18.2C12.8886 18.2 15.5088 15.5809 15.5088 12.35C15.5088 9.11913 12.8886 6.5 9.6564 6.5C6.42418 6.5 3.80396 9.11913 3.80396 12.35C3.80396 15.5809 6.42418 18.2 9.6564 18.2Z"
            , Attributes.fill "white"
            ]
            []
        , Svg.path
            [ Attributes.d "M9.65674 10.5V12.822M9.65674 14.5636H9.66196"
            , Attributes.stroke "#707070"
            , Attributes.strokeWidth "1.25"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| Pennant to indicate that premium status has expired
-}
expiredPremiumFlag : Nri.Ui.Svg.V1.Svg
expiredPremiumFlag =
    Nri.Ui.Svg.V1.init "0 0 25 25"
        [ Svg.path
            [ Attributes.d "M1.60923 3C0.720477 3 0 3.70888 0 4.58333V20.4167C0 21.2911 0.720477 22 1.60923 22H23.3817C24.7466 22 25.4918 20.4333 24.6173 19.4022L19.6335 13.5259C19.1358 12.9391 19.1352 12.0862 19.6322 11.4987L24.6246 5.59615C25.4971 4.56468 24.7516 3 23.3877 3H1.60923Z"
            , Attributes.fill "#E70D4F"
            , Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            ]
            []
        , Svg.path
            [ Attributes.d "M14.5176 12.8495C14.5174 10.1615 12.3377 7.98244 9.64957 7.98244C6.96166 7.98267 4.78272 10.1616 4.78248 12.8495C4.78248 15.5376 6.96151 17.7173 9.64957 17.7176C12.3378 17.7176 14.5176 15.5378 14.5176 12.8495ZM9.15835 10.7062C9.15849 10.4352 9.37856 10.2152 9.64957 10.215C9.92077 10.215 10.1406 10.435 10.1408 10.7062V12.6461L11.0695 13.5748L11.1319 13.6516C11.2577 13.8423 11.2373 14.1016 11.0695 14.2694C10.9017 14.4373 10.6423 14.4577 10.4516 14.3318L10.3749 14.2694L9.30226 13.1968C9.21041 13.1047 9.15835 12.9796 9.15835 12.8495V10.7062ZM15.5 12.8495C15.5 16.0804 12.8804 18.7 9.64957 18.7C6.41893 18.6998 3.80005 16.0802 3.80005 12.8495C3.80028 9.61902 6.41907 7.00024 9.64957 7C12.8803 7 15.4998 9.61888 15.5 12.8495Z"
            , Attributes.fill "white"
            ]
            []
        ]


{-| Pennant to indicate premium content that is currently accessible through premium vouchers
-}
giftPremiumFlag : Svg
giftPremiumFlag =
    Nri.Ui.Svg.V1.init "0 0 25 26"
        [ Svg.g
            [ Attributes.stroke "none"
            , Attributes.strokeWidth "1"
            , Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            ]
            [ Svg.rect
                [ Attributes.x "1.25391"
                , Attributes.y "6.45862"
                , Attributes.width "22.4941"
                , Attributes.height "18.75"
                , Attributes.rx "3"
                , Attributes.fill "#FEC709"
                ]
                []
            , Svg.path
                [ Attributes.d "M10.75 13.9586C10.9643 13.9586 11.1786 14.1423 11.2354 14.3522L11.25 14.4586V24.7086C11.25 24.9229 11.0663 25.1372 10.8564 25.194L10.75 25.2086H2.5C1.8125 25.2086 1.33507 24.7885 1.26027 24.1408L1.25 23.9586V14.2086C1.25 14.1086 1.33 14.0086 1.426 13.9726L1.5 13.9586H10.75ZM23.5 13.9586C23.6 13.9586 23.7 14.0386 23.736 14.1346L23.75 14.2086V23.9586C23.75 24.6461 23.3299 25.1235 22.6821 25.1983L22.5 25.2086H14.25C14.0357 25.2086 13.8214 25.0249 13.7646 24.815L13.75 24.7086V14.4586C13.75 14.2443 13.9337 14.03 14.1436 13.9732L14.25 13.9586H23.5ZM17.5 0.208618C19.625 0.208618 21.25 1.83362 21.25 3.95862C21.25 4.84751 20.9537 5.63763 20.4489 6.2412L20.25 6.45862H23.75C24.4375 6.45862 24.9149 6.87876 24.9897 7.52647L25 7.70862V11.2086C25 11.3086 24.92 11.4086 24.824 11.4446L24.75 11.4586H14.25C14.0357 11.4586 13.8214 11.2749 13.7646 11.065L13.75 10.9586V6.95862C13.75 6.74433 13.5663 6.53005 13.3564 6.4732L13.25 6.45862H11.75C11.5357 6.45862 11.3214 6.64229 11.2646 6.8522L11.25 6.95862V10.9586C11.25 11.1729 11.0663 11.3872 10.8564 11.444L10.75 11.4586H0.25C0.15 11.4586 0.05 11.3786 0.014 11.2826L0 11.2086V7.70862C0 7.02112 0.420139 6.54369 1.06785 6.46889L1.25 6.45862H4.75C4.125 5.83362 3.75 4.95862 3.75 3.95862C3.75 1.83362 5.375 0.208618 7.5 0.208618C8.75 0.208618 11 1.33362 12.5 2.83362C14 1.33362 16.25 0.208618 17.5 0.208618ZM7.5 2.70862C6.75 2.70862 6.25 3.20862 6.25 3.95862C6.25 4.70862 6.75 5.20862 7.5 5.20862C7.5 5.20862 10.75 5.08362 11.125 4.95862C10.5 4.20862 8.25 2.70862 7.5 2.70862ZM17.5 2.70862C16.75 2.70862 14.625 4.08362 13.875 4.95862C14.25 5.08362 17.5 5.20862 17.5 5.20862C18.25 5.20862 18.75 4.70862 18.75 3.95862C18.75 3.20862 18.25 2.70862 17.5 2.70862Z"
                , Attributes.fill "#004E95"
                ]
                []
            ]
        ]
