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
    Nri.Ui.Svg.V1.init "0 0 25 19"
        [ Svg.g
            [ Attributes.stroke "none"
            , Attributes.strokeWidth "1"
            , Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            ]
            [ Svg.path
                [ Attributes.d "M12.7757 0.708618H0L0 18.7086H7.34268H25L17.5 9.70862L25 0.708618H12.7757Z"
                , Attributes.fill "#FEC709"
                ]
                []
            ]
        ]


{-| Pennant to indicate that premium status is active
-}
activePremiumFlag : Svg
activePremiumFlag =
    Nri.Ui.Svg.V1.init "0 0 25 19"
        [ Svg.g
            [ Attributes.stroke "none"
            , Attributes.strokeWidth "1"
            , Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            ]
            [ Svg.path
                [ Attributes.d "M12.7757 0.708618H0L0 18.7086H7.34268H25L17.5 9.70862L25 0.708618H12.7757Z"
                , Attributes.fill "#FEC709"
                ]
                []
            , Svg.path
                [ Attributes.d "M8.02442 13.6406C7.78665 13.6406 7.55721 13.549 7.38513 13.3839L4.78544 10.8975C4.41584 10.5446 4.40289 9.95895 4.7563 9.58981C5.10832 9.22068 5.69441 9.20726 6.06401 9.56067L7.98325 11.3957L12.8954 6.07468C13.2423 5.6986 13.8275 5.67547 14.2026 6.02195C14.5782 6.36888 14.6018 6.95404 14.2553 7.32919L8.70441 13.3427C8.53556 13.5254 8.3015 13.6323 8.0531 13.6401C8.04384 13.6406 8.03413 13.6406 8.02442 13.6406Z"
                , Attributes.fill "#004E95"
                ]
                []
            ]
        ]


{-| Pennant to indicate that premium status is inactive
-}
inactivePremiumFlag : Nri.Ui.Svg.V1.Svg
inactivePremiumFlag =
    Nri.Ui.Svg.V1.init "0 0 25 19"
        [ Svg.g
            [ Attributes.stroke "none"
            , Attributes.strokeWidth "1"
            , Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            , Attributes.transform "translate(0, -3.5)"
            ]
            [ Svg.path
                [ Attributes.d "M12.7757 3.70862H0L0 21.7086H7.34268H25L17.5 12.7086L25 3.70862H12.7757Z"
                , Attributes.fill "#707070"
                ]
                []
            , Svg.circle
                [ Attributes.cx "9"
                , Attributes.cy "12.7086"
                , Attributes.r "6"
                , Attributes.fill "white"
                ]
                []
            , Svg.path
                [ Attributes.d "M9.88176 15.937C9.88176 16.4073 9.49143 16.7886 9.00993 16.7886C8.52843 16.7886 8.1381 16.4073 8.1381 15.937C8.1381 15.4666 8.52843 15.0853 9.00993 15.0853C9.49143 15.0853 9.88176 15.4666 9.88176 15.937Z"
                , Attributes.fill "#707070"
                ]
                []
            , Svg.path
                [ Attributes.d "M9.03467 9.10869H8.99009C8.33213 9.10277 7.78832 9.62308 7.7616 10.2841C7.7616 10.563 8.30649 13.8103 8.30649 13.8103C8.36492 14.1481 8.65898 14.3931 9 14.388H9.02972C9.37073 14.3931 9.66479 14.1481 9.72322 13.8103C9.72322 13.8103 10.2384 10.568 10.2384 10.2841C10.2121 9.6327 9.68304 9.11611 9.03467 9.10869Z"
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
                [ Attributes.d "M12.7757 3.70862H0L0 21.7086H7.34268H25L17.5 12.7208L25 3.70862H12.7757Z"
                , Attributes.fill "#E70D4F"
                ]
                []
            , Svg.path
                [ Attributes.d "M9 6.70862C5.68632 6.70862 3 9.39494 3 12.7086C3 16.0223 5.68632 18.7086 9 18.7086C12.3137 18.7086 15 16.0223 15 12.7086C15 9.39494 12.3137 6.70862 9 6.70862ZM9 17.5086C6.34908 17.5086 4.2 15.3597 4.2 12.7086C4.2 10.0577 6.34908 7.90862 9 7.90862C11.651 7.90862 13.8 10.0577 13.8 12.7086C13.8 15.3597 11.651 17.5086 9 17.5086Z"
                , Attributes.fill "white"
                ]
                []
            , Svg.path
                [ Attributes.d "M8.89096 9.54054C8.55942 9.54054 8.29066 9.8093 8.29066 10.1408V13.4954C8.2851 13.6024 8.30853 13.7077 8.35634 13.8008C8.35746 13.803 8.3586 13.8052 8.35976 13.8074C8.43156 13.9434 8.55408 14.0485 8.70214 14.0975L10.7876 14.914C11.0963 15.0348 11.4446 14.8826 11.5654 14.5738C11.6863 14.2651 11.534 13.9169 11.2253 13.796L9.49125 13.1172V10.1408C9.49125 9.8093 9.22249 9.54054 8.89096 9.54054Z"
                , Attributes.fill "white"
                ]
                []
            ]
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
