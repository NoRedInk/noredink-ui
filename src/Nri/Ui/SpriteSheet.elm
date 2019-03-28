module Nri.Ui.SpriteSheet exposing (checkmark, exclamationMark, bulb)

{-|

@docs checkmark, exclamationMark, bulb

-}

import Html.Styled exposing (..)
import Nri.Ui.Svg.V1 as NriSvg
import Svg exposing (..)
import Svg.Attributes exposing (..)


{-| -}
exclamationMark : NriSvg.Svg
exclamationMark =
    Svg.svg
        [ viewBox "0 0 4 12", width "100%", height "100%" ]
        [ Svg.path
            [ d "M3.234 10.575a1.363 1.363 0 1 1-2.726 0 1.363 1.363 0 0 1 2.726 0zm.648-8.398a1.978 1.978 0 0 1-.007.047l-.834 5.294c-.079.53-.542.926-1.085.926h-.013a1.096 1.096 0 0 1-1.085-.926L.024 2.224A1.93 1.93 0 0 1 1.93 0h.04a1.94 1.94 0 0 1 1.912 1.663v.514z"
            , fill "currentcolor"
            , fillRule "evenodd"
            ]
            []
        ]
        |> fromUnstyled
        |> NriSvg.fromHtml


{-| -}
checkmark : NriSvg.Svg
checkmark =
    Svg.svg
        [ id "Layer_1"
        , x "0px"
        , y "0px"
        , viewBox "0 0 21.7 17.1"
        , Svg.Attributes.style "enable-background:new 0 0 21.7 17.1;"
        , width "100%"
        , height "100%"
        ]
        [ Svg.style [] [ Svg.text " .st0{fill:#FFFFFF;} " ]
        , Svg.path [ id "check-white", class "st0", d "M7.6,17.1c-0.5,0-1-0.2-1.4-0.6l-5.6-5.4c-0.8-0.8-0.8-2-0.1-2.8c0.8-0.8,2-0.8,2.8-0.1l4.1,4 L18.2,0.7c0.8-0.8,2-0.9,2.8-0.1s0.9,2,0.1,2.8l-12,13C8.7,16.9,8.2,17.1,7.6,17.1C7.7,17.1,7.6,17.1,7.6,17.1" ] []
        ]
        |> fromUnstyled
        |> NriSvg.fromHtml


{-| -}
bulb : NriSvg.Svg
bulb =
    Svg.svg [ id "Layer_1", viewBox "0 0 45 45", width "100%", height "100%" ]
        [ defs []
            [ Svg.style [] [ Svg.text ".cls-1,.cls-2{fill:none;}.cls-2{clip-rule:evenodd;}.cls-3{clip-path:url(#clip-path);}.cls-4{clip-path:url(#clip-path-2);}.cls-5{fill:#00488A;}.cls-6{clip-path:url(#clip-path-3);}.cls-7{fill:#fff;}.cls-8{clip-path:url(#clip-path-5);}.cls-9{clip-path:url(#clip-path-7);}.cls-10{clip-path:url(#clip-path-9);}.cls-11{clip-path:url(#clip-path-11);}.cls-12{clip-path:url(#clip-path-13);}.cls-13{clip-path:url(#clip-path-15);}.cls-14{clip-path:url(#clip-path-17);}.cls-15{clip-path:url(#clip-path-19);}.cls-16{clip-path:url(#clip-path-21);}.cls-17{clip-path:url(#clip-path-23);}.cls-18{clip-path:url(#clip-path-25);}.cls-19{clip-path:url(#clip-path-27);}.cls-20{clip-path:url(#clip-path-28);}.cls-21{clip-path:url(#clip-path-29);}.cls-22{clip-path:url(#clip-path-30);}.cls-23{clip-path:url(#clip-path-31);}.cls-24{clip-path:url(#clip-path-32);}.cls-25{clip-path:url(#clip-path-33);}.cls-26{clip-path:url(#clip-path-34);}.cls-27{clip-path:url(#clip-path-35);}" ]
            , Svg.clipPath [ id "clip-path" ] [ circle [ class "cls-1", cx "22.5", cy "22.5", r "22.5" ] [] ]
            , Svg.clipPath [ id "clip-path-2" ] [ rect [ class "cls-1", x "-204", y "-317", width "1024", height "874" ] [] ]
            , Svg.clipPath [ id "clip-path-3" ] [ Svg.path [ class "cls-2", d "M35.26,21.37h-3a.66.66,0,0,0,0,1.31h3a.66.66,0,1,0,0-1.31Z" ] [] ]
            , Svg.clipPath [ id "clip-path-5" ] [ Svg.path [ class "cls-2", d "M31.16,18a.66.66,0,0,0,.34-.09l2.63-1.52a.66.66,0,0,0-.67-1.15l-2.63,1.53A.66.66,0,0,0,31.16,18Z" ] [] ]
            , Svg.clipPath [ id "clip-path-7" ] [ Svg.path [ class "cls-2", d "M30,10.4a.66.66,0,0,0-.9.17l-1.72,2.51h0a.66.66,0,1,0,1.09.73l1.7-2.52a.66.66,0,0,0-.17-.9Z" ] [] ]
            , Svg.clipPath [ id "clip-path-9" ] [ Svg.path [ class "cls-2", d "M17,14.11a.66.66,0,0,0,.54-1l-1.72-2.51h0a.66.66,0,1,0-1.09.74l1.71,2.52a.66.66,0,0,0,.55.29Z" ] [] ]
            , Svg.clipPath [ id "clip-path-11" ] [ Svg.path [ class "cls-2", d "M14.14,16.73,11.51,15.2a.66.66,0,0,0-.66,1.14l2.63,1.52a.66.66,0,1,0,.66-1.14Z" ] [] ]
            , Svg.clipPath [ id "clip-path-13" ] [ Svg.path [ class "cls-2", d "M34.12,27.58,31.5,26.06a.66.66,0,0,0-.67,1.14l2.63,1.52a.66.66,0,0,0,.66-1.14Z" ] [] ]
            , Svg.clipPath [ id "clip-path-15" ] [ Svg.path [ class "cls-2", d "M13.48,26.06l-2.63,1.52h0a.66.66,0,1,0,.66,1.14l2.63-1.52a.66.66,0,0,0-.66-1.14Z" ] [] ]
            , Svg.clipPath [ id "clip-path-17" ] [ Svg.path [ class "cls-2", d "M13.44,22a.66.66,0,0,0-.66-.66h-3a.66.66,0,0,0,0,1.31h3a.66.66,0,0,0,.66-.66Z" ] [] ]
            , Svg.clipPath [ id "clip-path-19" ] [ Svg.path [ class "cls-2", d "M22.48,12.36a.66.66,0,0,0,.66-.66v-3a.66.66,0,1,0-1.31,0v3a.66.66,0,0,0,.66.66Z" ] [] ]
            , Svg.clipPath [ id "clip-path-21" ] [ Svg.path [ class "cls-2", d "M25.09,30H19.86a.66.66,0,1,0,0,1.31h5.23a.66.66,0,1,0,0-1.31Z" ] [] ]
            , Svg.clipPath [ id "clip-path-23" ] [ Svg.path [ class "cls-2", d "M21.12,33.89v.36a.75.75,0,0,0,.74.75h1.06a.75.75,0,0,0,.74-.74v-.35A1.62,1.62,0,0,0,25.2,32.3H19.74a1.62,1.62,0,0,0,1.38,1.59Z" ] [] ]
            , Svg.clipPath [ id "clip-path-25" ] [ Svg.path [ class "cls-2", d "M22.18,17.57a4.12,4.12,0,0,0-3.91,4.58h0a.65.65,0,1,1-1.3,0c.05-3,1.47-4.44,2.65-5.12a5.66,5.66,0,0,1,2.58-.74.66.66,0,0,1,0,1.31Zm.3-2.5a6.69,6.69,0,0,0-6.6,6.78h0a6.89,6.89,0,0,0,.48,2.54l.13.3h0a6.79,6.79,0,0,0,1.13,1.75L19.3,29h6.37l1.68-2.58a6.79,6.79,0,0,0,1.13-1.75l.13-.3h0a6.9,6.9,0,0,0,.48-2.54,6.69,6.69,0,0,0-6.61-6.78Z" ] [] ]
            , Svg.clipPath [ id "clip-path-27" ] [ circle [ class "cls-1", cx "475.87", cy "27.33", r "26.5" ] [] ]
            , Svg.clipPath [ id "clip-path-28" ] [ circle [ class "cls-1", cx "567.05", cy "27.3", r "26.5" ] [] ]
            , Svg.clipPath [ id "clip-path-29" ] [ ellipse [ class "cls-1", cx "549.01", cy "5.43", rx "10.01", ry "9.44" ] [] ]
            , Svg.clipPath [ id "clip-path-30" ] [ polygon [ class "cls-2", points "549.05 0.35 554.46 8.48 543.64 8.48 549.05 0.35" ] [] ]
            , Svg.clipPath [ id "clip-path-31" ] [ Svg.path [ class "cls-2", d "M531.25,12.22a1.38,1.38,0,0,0-.71,1.82l1.7,3.92a21.1,21.1,0,0,0-11.9-2.82,20.87,20.87,0,0,0-12.81,5.4,1.39,1.39,0,0,0-.11,1.95,1.36,1.36,0,0,0,1.93.11,18.19,18.19,0,0,1,11.16-4.7,18.39,18.39,0,0,1,10.74,2.68l-3.91,1.72a1.38,1.38,0,0,0-.7,1.82,1.36,1.36,0,0,0,1.8.72l6.94-3.05h0a1.39,1.39,0,0,0,.71-1.82l-3-7a1.36,1.36,0,0,0-1.8-.72Z" ] [] ]
            , Svg.clipPath [ id "clip-path-32" ] [ rect [ class "cls-1", x "460.46", y "140", width "25", height "20" ] [] ]
            , Svg.clipPath [ id "clip-path-33" ] [ rect [ class "cls-1", x "492.46", y "100", width "25", height "60" ] [] ]
            , Svg.clipPath [ id "clip-path-34" ] [ rect [ class "cls-1", x "523.46", y "114", width "25", height "46" ] [] ]
            , Svg.clipPath [ id "clip-path-35" ] [ rect [ class "cls-1", x "555.46", y "86", width "25", height "74" ] [] ]
            ]
        , g [ class "cls-3" ] [ g [ class "cls-4" ] [ rect [ class "cls-5", x "-5", y "-5", width "55", height "55" ] [] ] ]
        , g [ class "cls-6" ] [ g [ class "cls-4" ] [ rect [ class "cls-7", x "26.56", y "16.37", width "14.36", height "11.31" ] [] ] ]
        , g [ class "cls-8" ] [ g [ class "cls-4" ] [ rect [ class "cls-7", x "25.5", y "10.11", width "13.96", height "12.85" ] [] ] ]
        , g [ class "cls-9" ] [ g [ class "cls-4" ] [ rect [ class "cls-7", x "22.28", y "5.29", width "13.04", height "13.85" ] [] ] ]
        , g [ class "cls-10" ] [ g [ class "cls-4" ] [ rect [ class "cls-7", x "9.66", y "5.25", width "13.05", height "13.85" ] [] ] ]
        , g [ class "cls-11" ] [ g [ class "cls-4" ] [ rect [ class "cls-7", x "5.54", y "10.12", width "13.95", height "12.84" ] [] ] ]
        , g [ class "cls-12" ] [ g [ class "cls-4" ] [ rect [ class "cls-7", x "25.5", y "20.97", width "13.93", height "12.83" ] [] ] ]
        , g [ class "cls-13" ] [ g [ class "cls-4" ] [ rect [ class "cls-7", x "5.5", y "20.99", width "13.95", height "12.83" ] [] ] ]
        , g [ class "cls-14" ] [ g [ class "cls-4" ] [ rect [ class "cls-7", x "4.09", y "16.37", width "14.36", height "11.31" ] [] ] ]
        , g [ class "cls-15" ] [ g [ class "cls-4" ] [ rect [ class "cls-7", x "16.82", y "3", width "11.31", height "14.36" ] [] ] ]
        , g [ class "cls-16" ] [ g [ class "cls-4" ] [ rect [ class "cls-7", x "14.21", y "24.97", width "16.54", height "11.31" ] [] ] ]
        , g [ class "cls-17" ] [ g [ class "cls-4" ] [ rect [ class "cls-7", x "14.74", y "27.3", width "15.46", height "12.7" ] [] ] ]
        , g [ class "cls-18" ] [ g [ class "cls-4" ] [ rect [ class "cls-7", x "10.88", y "10.07", width "23.21", height "23.94" ] [] ] ]
        ]
        |> fromUnstyled
        |> NriSvg.fromHtml
