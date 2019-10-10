module Nri.Ui.UiIcon.V1 exposing
    ( unarchive, share, seeMore, preview, performance, openClose, download
    , edit
    , gear
    )

{-|

@docs unarchive, share, seeMore, preview, performance, openClose, download
@docs edit
@docs gear

    import Css
    import Html.Styled exposing (..)
    import Html.Styled.Attrbutes exposing (css)
    import Nri.Ui.Colors.V1 as Colors
    import Nri.Ui.Svg.V1 as Svg
    import Nri.Ui.UiIcon.V1 as UiIcon

    view : Html msg
    view =
        div [ css [ Css.color Colors.lichen ] ]
            [ Svg.toHtml UiIcon.unarchive ]

-}

import Nri.Ui.Svg.V1
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attributes


{-| -}
unarchive : Nri.Ui.Svg.V1.Svg
unarchive =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.viewBox "0 0 25 25"
        ]
        [ Svg.path
            [ Attributes.fill "currentcolor"
            , Attributes.fillRule "evenodd"
            , Attributes.d "M.858 22.7c.026.233.104.46.228.66.128.205.292.385.484.531.202.154.428.274.669.355.25.085.515.129.78.129h18.954a2.435 2.435 0 0 0 1.455-.484c.19-.145.356-.323.484-.526.124-.2.202-.425.228-.66l.85-7.696a1.716 1.716 0 0 0-.49-1.403 1.94 1.94 0 0 0-.653-.431 2.244 2.244 0 0 0-.841-.16H1.999a2.239 2.239 0 0 0-.84.16 1.93 1.93 0 0 0-.653.431 1.724 1.724 0 0 0-.4.634 1.72 1.72 0 0 0-.097.769l.849 7.69zm6.327-5.2a.906.906 0 0 1 .213-.3c.097-.09.209-.16.332-.206.132-.05.271-.076.413-.076a1.149 1.149 0 0 1 .764.282c.1.082.183.184.243.3.058.112.09.237.093.365l.042 1.376h6.427l.041-1.376h.001a.86.86 0 0 1 .09-.366.992.992 0 0 1 .24-.3c.102-.088.22-.158.346-.205a1.162 1.162 0 0 1 .832 0c.123.047.236.116.331.205a.834.834 0 0 1 .282.666l-.112 2.283a.837.837 0 0 1-.099.35.973.973 0 0 1-.234.284 1.176 1.176 0 0 1-.739.26H8.307c-.137 0-.275-.024-.404-.071a1.119 1.119 0 0 1-.348-.181.967.967 0 0 1-.234-.285.828.828 0 0 1-.1-.349l-.112-2.283a.83.83 0 0 1 .076-.374zM18.635 2.81h-2.183l.065-.962A2.715 2.715 0 0 0 16.35.717c0-.031-.026-.06-.038-.092l3.493.001c.588 0 1.089.427 1.187 1.012l1.616 9.548h-2.433l-1.358-8.204a.193.193 0 0 0-.19-.162l.007-.009zM8.932.707h.001a2.713 2.713 0 0 0-.167 1.139l.066.962H6.649a.194.194 0 0 0-.19.164l-1.358 8.214H2.668l1.616-9.545A1.209 1.209 0 0 1 5.474.628h3.492c-.014.037-.03.058-.04.09l.007-.01zM8.795 4.68a.812.812 0 0 0-.064.263.767.767 0 0 0 .182.542.844.844 0 0 0 .161.152 1.172 1.172 0 0 0 .432.179c.08.015.163.024.245.024h1.16l-.132 4.095a.91.91 0 0 0 .275.686 1.037 1.037 0 0 0 .719.29h1.73a1.044 1.044 0 0 0 .719-.29.91.91 0 0 0 .275-.686l-.133-4.097h1.213c.095 0 .19-.012.281-.034.09-.022.178-.056.26-.101a.839.839 0 0 0 .357-.36.782.782 0 0 0 .06-.547.845.845 0 0 0-.115-.255.916.916 0 0 0-.091-.116L13.413 1.2a.833.833 0 0 0-.154-.135 1.136 1.136 0 0 0-1.24 0 .851.851 0 0 0-.155.135L8.952 4.417a.939.939 0 0 0-.164.252l.007.01z"
            ]
            []
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
share : Nri.Ui.Svg.V1.Svg
share =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 0 30 30"
        ]
        [ Svg.path
            [ Attributes.d "M18.5,0.1H1.8C0.3,0.1,0,0.5,0,1.9v26.2C0,29.7,0.4,30,1.8,30h26.3c1.5,0,1.8-0.3,1.8-1.8V12.3l-3.1,2.6v12H3.1V3.2h12.4L18.5,0.1z M23,9.9v4.2l7-7l-7-7v3.5C6.8,4.2,6.8,19.7,6.8,19.7S11.6,10.6,23,9.9z"
            ]
            []
        , Svg.path
            [ Attributes.fill "none"
            , Attributes.d "M18.5,0.1H1.8C0.3,0.1,0,0.5,0,1.9v26.2C0,29.7,0.4,30,1.8,30h26.3c1.5,0,1.8-0.3,1.8-1.8V12.3l-3.1,2.6v12H3.1V3.2h12.4L18.5,0.1z M23,9.9v4.2l7-7l-7-7v3.5C6.8,4.2,6.8,19.7,6.8,19.7S11.6,10.6,23,9.9z"
            ]
            []
        , Svg.path
            [ Attributes.fill "none"
            , Attributes.d "M-793.3-401V715h1024V-401H-793.3z"
            ]
            []
        , Svg.path
            [ Attributes.fill "none"
            , Attributes.d "M-815.4-385.9v1116h1024v-1116L-815.4-385.9L-815.4-385.9z"
            ]
            []
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
seeMore : Nri.Ui.Svg.V1.Svg
seeMore =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 0 30 30"
        ]
        [ Svg.ellipse [ Attributes.cx "8.1", Attributes.cy "15", Attributes.rx "2.3", Attributes.ry "2.2" ] []
        , Svg.ellipse [ Attributes.cx "15", Attributes.cy "15", Attributes.rx "2.3", Attributes.ry "2.2" ] []
        , Svg.ellipse [ Attributes.cx "21.5", Attributes.cy "15", Attributes.rx "2.3", Attributes.ry "2.2" ] []
        , Svg.path [ Attributes.d "M28.3,0H1.9C1.1,0,0.7,0.1,0.4,0.4C0.1,0.7,0,1.1,0,1.9v26.2C0,29.7,0.4,30,1.8,30H27h1.1h0.1c0.1,0,0.2,0,0.3,0c0,0,0.1,0,0.1,0c0.1,0,0.3,0,0.4-0.1c0,0,0,0,0,0c0.8-0.2,1-0.6,1-1.7V1.9C30.1,0.4,29.7,0,28.3,0z M26.8,27H3.1v-0.1V3.2V3.1h0.1H27v23.8V27H26.8z" ] []
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
preview : Nri.Ui.Svg.V1.Svg
preview =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 5 25 15"
        , Attributes.fillRule "evenodd"
        ]
        [ Svg.path [ Attributes.d "M12.5,5 C18.0555556,5 25,12.5 25,12.5 C25,12.5 18.0555556,20 12.5,20 C6.94444444,20 0,12.5 0,12.5 C3.2637037,9.26571429 7.62444444,5.19964286 12.5,5 Z M12.5,8.48214286 C10.1981481,8.48214286 8.33333333,10.28 8.33333333,12.5 C8.33333333,14.7196429 10.1981481,16.5178571 12.5,16.5178571 C14.8018519,16.5178571 16.6666667,14.7196429 16.6666667,12.5 C16.6666667,10.28 14.8018519,8.48214286 12.5,8.48214286 Z M12.5,14.5089286 C11.35,14.5089286 10.4166667,13.6085714 10.4166667,12.5 C10.4166667,11.3910714 11.35,10.4910714 12.5,10.4910714 C13.65,10.4910714 14.5833333,11.3910714 14.5833333,12.5 C14.5833333,13.6085714 13.65,14.5089286 12.5,14.5089286 Z" ] [] ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
performance : Nri.Ui.Svg.V1.Svg
performance =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 0 30 30"
        ]
        [ Svg.polygon [ Attributes.points "22.1,24.6 22.1,8.4 17.8,7.1 17.8,24.6 " ] []
        , Svg.polygon [ Attributes.points "24.2,7.7 24.2,24.6 28.5,24.6 28.5,5 26.3,3.5 " ] []
        , Svg.polygon [ Attributes.points "5,15.6 5,24.6 9.3,24.6 9.3,12.3 5.8,16.5 " ] []
        , Svg.polygon [ Attributes.points "11.4,24.6 15.7,24.6 15.7,6.5 14.5,6.2 11.4,9.8 " ] []
        , Svg.path [ Attributes.fill "none", Attributes.d "M33.6,26.9H30v1.2c0,1.6-0.4,1.8-1.8,1.8H1.8C0.4,30,0,29.7,0,28.2V1.9c0-1.4,0.3-1.8,1.8-1.8H3v-5.5h30.6V26.9z" ] []
        , Svg.path [ Attributes.d "M3.1,26.9V3.2V0.1H1.8C0.3,0.1,0,0.5,0,1.9v26.2C0,29.7,0.4,30,1.8,30h26.3c1.5,0,1.8-0.3,1.8-1.8V27h-3.1H3.1V26.9z" ] []
        , Svg.path [ Attributes.fill "none", Attributes.d "M-715-401V715H309V-401H-715z" ] []
        , Svg.path [ Attributes.fill "none", Attributes.d "M-737.2-385.9v1116h1024v-1116L-737.2-385.9L-737.2-385.9z" ] []
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
openClose : Nri.Ui.Svg.V1.Svg
openClose =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 0 14 12"
        ]
        [ Svg.path [ Attributes.d "M8.3,10.6c0,0.2-0.1,0.9,0.3,0.9h2.9c1.4,0,2.6-1.2,2.6-2.6V2.6C14,1.2,12.8,0,11.4,0 H8.6C8.4,0,8.3,0.1,8.3,0.3c0,0.3-0.1,0.9,0.3,0.9h2.9c0.8,0,1.4,0.6,1.4,1.4v6.3c0,0.8-0.6,1.4-1.4,1.4H8.9 C8.6,10.3,8.3,10.2,8.3,10.6z M0,5.7C0,5.9,0.1,6,0.2,6.1L5,11c0.1,0.1,0.2,0.2,0.4,0.2c0.3,0,0.6-0.3,0.6-0.6V8h4 c0.3,0,0.6-0.3,0.6-0.6V4c0-0.3-0.3-0.6-0.6-0.6H6V0.9c0-0.3-0.3-0.6-0.6-0.6C5.3,0.3,5.1,0.3,5,0.5L0.2,5.3C0.1,5.4,0,5.6,0,5.7z" ] [] ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
download : Nri.Ui.Svg.V1.Svg
download =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 0 21 21"
        ]
        [ Svg.path
            [ Attributes.fillRule "evenodd"
            , Attributes.d "M17.719 12.467H21v5.25a1.968 1.968 0 0 1-1.969 1.97H1.97A1.968 1.968 0 0 1 0 17.716v-5.25h3.281v3.938H17.72v-3.938zM5.647 9.17h.001a1.024 1.024 0 0 1-.082-.332.967.967 0 0 1 .046-.352A1.037 1.037 0 0 1 6 7.962c.08-.057.166-.104.257-.14a1.642 1.642 0 0 1 .597-.115h1.462l-.167-5.163a1.148 1.148 0 0 1 .347-.865 1.307 1.307 0 0 1 .906-.365h2.18c.172 0 .343.034.503.1.15.06.287.151.404.265a1.148 1.148 0 0 1 .347.865l-.168 5.165h1.529c.12 0 .24.015.354.043.114.027.225.07.328.127a1.058 1.058 0 0 1 .45.453.985.985 0 0 1 .076.69 1.065 1.065 0 0 1-.06.166 1.01 1.01 0 0 1-.2.302l-3.676 4.064a1.05 1.05 0 0 1-.194.17 1.432 1.432 0 0 1-1.326.126 1.29 1.29 0 0 1-.236-.126 1.073 1.073 0 0 1-.197-.17L5.845 9.5a1.183 1.183 0 0 1-.207-.318l.009-.013z"
            ]
            []
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
edit : Nri.Ui.Svg.V1.Svg
edit =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 0 30 30"
        ]
        [ Svg.path [ Attributes.d "M27.3,7.9l-5.2-5.2l2.3-2.3c0.5-0.5,1.2-0.5,1.7,0L29.7,4c0.5,0.5,0.5,1.2,0,1.7L27.3,7.9z M25.9,9.4L8.6,26.6l-5.2-5.2L20.6,4.1L25.9,9.4z M0,30l1.9-7L7,28.1L0,30z" ] []
        , Svg.path [ Attributes.fill "none", Attributes.d "M-753.8-401V715h1024V-401H-753.8z" ] []
        , Svg.path [ Attributes.fill "none", Attributes.d "M-775.9-385.9v1116h1024v-1116L-775.9-385.9L-775.9-385.9z" ] []
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
gear : Nri.Ui.Svg.V1.Svg
gear =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 0 25 25"
        ]
        [ Svg.path
            [ Attributes.fillRule "evenodd"
            , Attributes.d "M3.282 14.744A9.583 9.583 0 0 0 4.52 17.62l-1.557 1.565c-.7.699-.693 1.115-.077 1.73l1.519 1.52c.62.623 1.045.61 1.73-.077l1.635-1.641c.77.443 1.603.782 2.487 1v2c0 .993.3 1.282 1.167 1.282h2.154c.878 0 1.167-.309 1.167-1.282v-2a9.582 9.582 0 0 0 2.487-1l1.672 1.68c.693.686 1.109.699 1.73.083l1.526-1.526c.607-.608.62-1.025-.084-1.73l-1.602-1.602a9.417 9.417 0 0 0 1.243-2.878h2.116c.88-.001 1.168-.31 1.168-1.283v-1.924c0-.95-.25-1.282-1.167-1.282h-2.115a9.582 9.582 0 0 0-1-2.487l1.526-1.519c.673-.673.731-1.09.083-1.731l-1.525-1.526c-.61-.61-1.046-.602-1.731.083L17.62 4.52a9.583 9.583 0 0 0-2.877-1.237v-2c0-.95-.25-1.282-1.167-1.282h-2.154c-.866 0-1.167.314-1.167 1.282v2A9.583 9.583 0 0 0 7.38 4.52l-1.45-1.443c-.685-.685-1.121-.692-1.73-.083L2.672 4.52c-.648.64-.59 1.058.083 1.731l1.52 1.52a9.765 9.765 0 0 0-.994 2.486H1.167C.3 10.256 0 10.57 0 11.538v1.924c0 .993.3 1.282 1.167 1.282h2.115zm5.55-2.244a3.666 3.666 0 0 1 7.334 0 3.667 3.667 0 1 1-7.334 0z"
            ]
            []
        ]
        |> Nri.Ui.Svg.V1.fromHtml
