module Nri.Ui.Pennant.V2 exposing (premiumFlag)

{-| Used for indicating Premium content

@docs premiumFlag

-}

import Nri.Ui.Svg.V1 as NriSvg exposing (Svg)
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attr


{-| -}
premiumFlag : Svg
premiumFlag =
    Svg.svg
        [ Attr.width "100%"
        , Attr.height "100%"
        , Attr.viewBox "0 0 25 19"
        ]
        [ Svg.polygon
            [ Attr.fill "#FEC709"
            , Attr.points "12.7,0 0,0 0,13.8 0,15.8 0,17.5 7.3,17.5 24.8,17.5 19.4,8.1 24.8,0 "
            ]
            []
        , Svg.path
            [ Attr.fill "#146AFF"
            , Attr.d "M7.5,3.8h4.2c1.1,0,1.9,0.3,2.5,0.8s0.9,1.2,0.9,2.1s-0.3,1.6-0.9,2.1c-0.6,0.5-1.4,0.8-2.5,0.8H9.3 v4.1H7.5V3.8z M11.5,8.1c0.6,0,1.1-0.1,1.4-0.4c0.3-0.3,0.5-0.6,0.5-1.1c0-0.5-0.2-0.9-0.5-1.1c-0.3-0.3-0.8-0.4-1.4-0.4H9.3v3 H11.5z"
            ]
            []
        ]
        |> NriSvg.fromHtml
