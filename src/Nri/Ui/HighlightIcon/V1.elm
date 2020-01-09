module Nri.Ui.HighlightIcon.V1 exposing (highlighter, eraser)

{-|

@docs highlighter, eraser

    import Html.Styled exposing (Html)
    import Nri.Ui.Colors.V1 as Colors
    import Nri.Ui.HighlightIcon.V1 as HighlightIcon
    import Nri.Ui.Svg.V1 as Svg

    view : Html msg
    view =
        HighlightIcon.highlighter
            |> Svg.withColor Colors.highlightPink
            |> Svg.toHtml

-}

import Nri.Ui.Svg.V1
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attributes


{-| -}
highlighter : Nri.Ui.Svg.V1.Svg
highlighter =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 0 20 19"
        ]
        [ Svg.g
            [ Attributes.stroke "none"
            , Attributes.strokeWidth "1"
            ]
            [ Svg.path
                [ Attributes.d "M9.41807931,13.4291766 L4.26622163,8.34000495 L14.5705927,0.195646008 L17.6613576,3.25018551 L9.41807931,13.4291766 Z M3.76264181,10.5348681 L3.75164785,8.84876664 L8.90416124,13.9394477 L7.1966016,13.9293264 L3.75153857,15.9749069 L1.69136377,13.9394542 L3.76264181,10.5348681 Z M1.69127635,14.9567702 L2.72092661,15.9749069 L2.20609055,16.4835649 L0.145915533,16.4835649 L1.69127635,14.9567702 Z M0.146003179,18.6438119 L0.146003179,17.2038918 L19.0888228,17.2038918 L19.0888228,18.6438119 L0.146003179,18.6438119 Z"
                ]
                []
            ]
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
eraser : Nri.Ui.Svg.V1.Svg
eraser =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 0 25 25"
        ]
        []
        |> Nri.Ui.Svg.V1.fromHtml
