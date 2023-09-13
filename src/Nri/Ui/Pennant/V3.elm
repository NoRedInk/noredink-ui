module Nri.Ui.Pennant.V3 exposing (..)

{-| Changes from V3:

    - premiumFlag replaced by a newly designed contentPremiumFlag and activePremiumFlag
    - disabledPremiumFlag updated to inactivePremiumFlag with new design
    - expiredPremiumFlag updated with new design
    - giftPremiumFlag added for vouchered content

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
    Nri.Ui.Svg.V1.init "0 0 25 15"
        [ Svg.g
            [ Attributes.stroke "none"
            , Attributes.strokeWidth "1"
            , Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            , Attributes.transform "translate(0, -5)"
            ]
            [ Svg.path
                [ Attributes.d "M12.682 6.5H4.25V18.5H9.09617H20.75L16.25 12.5L20.75 6.5H12.682Z"
                , Attributes.fill "#FEC709"
                ]
                []
            ]
        ]


{-| Pennant to indicate that premium status is active
-}
activePremiumFlag : Svg
activePremiumFlag =
    Nri.Ui.Svg.V1.init "0 0 25 25"
        [ Svg.g
            [ Attributes.stroke "none"
            , Attributes.strokeWidth "1"
            , Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            ]
            [ Svg.path
                [ Attributes.fill "#FEC709"
                , Attributes.d "M12.7426 5H1.5L1.5 21H7.96156H23.5L17.5 13L23.5 5H12.7426Z"
                ]
                []
            , Svg.path
                [ Attributes.d "M9.02393 16.9321C8.78616 16.9321 8.55672 16.8405 8.38464 16.6754L5.78495 14.189C5.41535 13.8361 5.4024 13.2505 5.75581 12.8813C6.10783 12.5122 6.69392 12.4988 7.06352 12.8522L8.98276 14.6872L13.8949 9.36618C14.2418 8.99011 14.827 8.96698 15.2021 9.31345C15.5777 9.66038 15.6013 10.2455 15.2549 10.6207L9.70392 16.6342C9.53508 16.8169 9.30101 16.9238 9.05261 16.9317C9.04335 16.9321 9.03364 16.9321 9.02393 16.9321Z"
                , Attributes.fill "#004E95"
                ]
                []
            ]
        ]


{-| Pennant to indicate that premium status is inactive
-}
inactivePremiumFlag : Nri.Ui.Svg.V1.Svg
inactivePremiumFlag =
    Nri.Ui.Svg.V1.init "0 0 25 18"
        [ Svg.g
            [ Attributes.stroke "none"
            , Attributes.strokeWidth "1"
            , Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            , Attributes.transform "translate(0, -3.5)"
            ]
            [ Svg.path
                [ Attributes.d "M12.7426 5H1.5L1.5 21H7.96156H23.5L17.5 13L23.5 5H12.7426Z"
                , Attributes.fill "#707070"
                ]
                []
            , Svg.circle
                [ Attributes.cx "9.73047"
                , Attributes.cy "13"
                , Attributes.r "6"
                , Attributes.fill "#FFFFFF"
                ]
                []
            , Svg.path
                [ Attributes.fill "#707070"
                , Attributes.d "M10.6124 16.2282C10.6124 16.6986 10.222 17.0799 9.74052 17.0799C9.25902 17.0799 8.86869 16.6986 8.86869 16.2282C8.86869 15.7579 9.25902 15.3766 9.74052 15.3766C10.222 15.3766 10.6124 15.7579 10.6124 16.2282Z"
                ]
                []
            , Svg.path
                [ Attributes.d "M9.76526 9.39995H9.72068C9.06272 9.39403 8.51891 9.91434 8.49219 10.5754C8.49219 10.8543 9.03708 14.1016 9.03708 14.1016C9.09551 14.4394 9.38957 14.6844 9.73059 14.6793H9.76031C10.1013 14.6844 10.3954 14.4394 10.4538 14.1016C10.4538 14.1016 10.969 10.8592 10.969 10.5754C10.9427 9.92396 10.4136 9.40737 9.76526 9.39995Z"
                , Attributes.fill "#707070"
                ]
                []
            ]
        ]


{-| Pennant to indicate that premium status has expired
-}
expiredPremiumFlag : Nri.Ui.Svg.V1.Svg
expiredPremiumFlag =
    Nri.Ui.Svg.V1.init "0 0 25 25"
        [ Svg.g
            [ Attributes.stroke "none"
            , Attributes.strokeWidth "1"
            , Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            ]
            [ Svg.path
                [ Attributes.fill "#E70D4F"
                , Attributes.d "M12.7426 5H1.5L1.5 21H7.96156H23.5L16.9 13.0108L23.5 5H12.7426Z"
                ]
                []
            , Svg.path
                [ Attributes.d "M9.5 7C6.18632 7 3.5 9.68632 3.5 13C3.5 16.3137 6.18632 19 9.5 19C12.8137 19 15.5 16.3137 15.5 13C15.5 9.68632 12.8137 7 9.5 7ZM9.5 17.8C6.84908 17.8 4.7 15.651 4.7 13C4.7 10.3491 6.84908 8.2 9.5 8.2C12.151 8.2 14.3 10.3491 14.3 13C14.3 15.651 12.151 17.8 9.5 17.8Z"
                , Attributes.fill "#FFFFFF"
                ]
                []
            , Svg.path
                [ Attributes.d "M9.39096 9.83192C9.05942 9.83192 8.79066 10.1007 8.79066 10.4322V13.7868C8.7851 13.8937 8.80853 13.9991 8.85634 14.0922C8.85746 14.0944 8.8586 14.0966 8.85976 14.0988C8.93156 14.2348 9.05408 14.3399 9.20214 14.3889L11.2876 15.2054C11.5963 15.3262 11.9446 15.1739 12.0654 14.8652C12.1863 14.5565 12.034 14.2083 11.7253 14.0874L9.99125 13.4085V10.4322C9.99125 10.1007 9.72249 9.83192 9.39096 9.83192Z"
                , Attributes.fill "#FFFFFF"
                ]
                []
            ]
        ]


{-| Pennant to indicate premium content that is currently accessible through premium vouchers
-}
giftPremiumFlag : Svg
giftPremiumFlag =
    Nri.Ui.Svg.V1.init "0 0 25 25"
        [ Svg.g
            [ Attributes.stroke "none"
            , Attributes.strokeWidth "1"
            , Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            ]
            [ Svg.rect
                [ Attributes.x "1.25391"
                , Attributes.y "6.25"
                , Attributes.width "22.4941"
                , Attributes.height "18.75"
                , Attributes.rx "3"
                , Attributes.fill "#FEC709"
                ]
                []
            , Svg.path
                [ Attributes.d "M10.75 13.75C10.9643 13.75 11.1786 13.9337 11.2354 14.1436L11.25 14.25V24.5C11.25 24.7143 11.0663 24.9286 10.8564 24.9854L10.75 25H2.5C1.8125 25 1.33507 24.5799 1.26027 23.9321L1.25 23.75V14C1.25 13.9 1.33 13.8 1.426 13.764L1.5 13.75H10.75ZM23.5 13.75C23.6 13.75 23.7 13.83 23.736 13.926L23.75 14V23.75C23.75 24.4375 23.3299 24.9149 22.6821 24.9897L22.5 25H14.25C14.0357 25 13.8214 24.8163 13.7646 24.6064L13.75 24.5V14.25C13.75 14.0357 13.9337 13.8214 14.1436 13.7646L14.25 13.75H23.5ZM17.5 0C19.625 0 21.25 1.625 21.25 3.75C21.25 4.63889 20.9537 5.42901 20.4489 6.03258L20.25 6.25H23.75C24.4375 6.25 24.9149 6.67014 24.9897 7.31785L25 7.5V11C25 11.1 24.92 11.2 24.824 11.236L24.75 11.25H14.25C14.0357 11.25 13.8214 11.0663 13.7646 10.8564L13.75 10.75V6.75C13.75 6.53571 13.5663 6.32143 13.3564 6.26458L13.25 6.25H11.75C11.5357 6.25 11.3214 6.43367 11.2646 6.64359L11.25 6.75V10.75C11.25 10.9643 11.0663 11.1786 10.8564 11.2354L10.75 11.25H0.25C0.15 11.25 0.05 11.17 0.014 11.074L0 11V7.5C0 6.8125 0.420139 6.33507 1.06785 6.26027L1.25 6.25H4.75C4.125 5.625 3.75 4.75 3.75 3.75C3.75 1.625 5.375 0 7.5 0C8.75 0 11 1.125 12.5 2.625C14 1.125 16.25 0 17.5 0ZM7.5 2.5C6.75 2.5 6.25 3 6.25 3.75C6.25 4.5 6.75 5 7.5 5C7.5 5 10.75 4.875 11.125 4.75C10.5 4 8.25 2.5 7.5 2.5ZM17.5 2.5C16.75 2.5 14.625 3.875 13.875 4.75C14.25 4.875 17.5 5 17.5 5C18.25 5 18.75 4.5 18.75 3.75C18.75 3 18.25 2.5 17.5 2.5Z"
                , Attributes.fill "#004E95"
                ]
                []
            ]
        ]
