module Nri.Ui.Logo.V1 exposing
    ( noredink
    , clever, cleverC, cleverLibrary
    , googleClassroom, googleG
    , canvas
    , canvasCircle
    , schoology
    , schoologyCircle
    , facebook, twitter
    )

{-|

@docs noredink
@docs clever, cleverC, cleverLibrary
@docs googleClassroom, googleG
@docs canvas
@docs canvasCircle
@docs schoology
@docs schoologyCircle
@docs facebook, twitter

-}

import Nri.Ui.Svg.V1
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attributes


{-| -}
noredink : Nri.Ui.Svg.V1.Svg
noredink =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 0 109 24"
        ]
        [ Svg.g
            [ Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            ]
            [ Svg.path
                [ Attributes.fill "#F3336C"
                , Attributes.d "M4.29 6.03v2.048h.065c.943-1.723 2.568-2.503 4.453-2.503 2.795 0 4.453 1.527 4.453 4.972v12.97H8.776v-12.06c0-1.755-.586-2.437-1.918-2.437-1.528 0-2.373.943-2.373 2.892v11.604H0V6.03h4.29zM22.559 20.916c1.82 0 2.404-1.788 2.404-6.143 0-4.355-.584-6.143-2.404-6.143-2.21 0-2.405 2.568-2.405 6.143 0 3.575.195 6.143 2.405 6.143zm0-15.341c5.395-.098 6.89 3.12 6.89 9.198 0 5.98-1.755 9.198-6.89 9.198-5.396.098-6.89-3.12-6.89-9.198 0-5.98 1.754-9.198 6.89-9.198z"
                ]
                []
            , Svg.path
                [ Attributes.fill "#333"
                , Attributes.d "M32.246 6.257h1.95v2.698h.065c.748-1.918 2.34-3.088 4.356-3.088.227 0 .455.033.682.098v1.95a4.878 4.878 0 0 0-.942-.097c-2.145 0-4.16 1.56-4.16 4.907v10.791h-1.95V6.257zM49.994 13.342c-.065-4.29-1.268-5.85-3.673-5.85-2.405 0-3.608 1.56-3.673 5.85h7.346zm1.918 4.454c-.293 3.672-2.308 6.11-5.558 6.11-3.64 0-5.786-2.535-5.786-9.035 0-5.981 2.145-9.004 5.948-9.004 3.836 0 5.558 2.633 5.558 8.386v.715h-9.426v.813c0 4.972 1.755 6.5 3.673 6.5 2.048 0 3.315-1.463 3.64-4.485h1.95zM60.266 22.28c1.983 0 3.77-1.007 3.77-7.41 0-6.37-1.787-7.378-3.77-7.378-1.95 0-3.77 1.008-3.77 7.379 0 6.402 1.82 7.41 3.77 7.41zm3.965-1.624h-.065c-.52 1.983-2.177 3.25-4.225 3.25-3.802 0-5.525-3.055-5.525-9.035 0-5.948 1.723-9.004 5.525-9.004 2.145 0 3.608 1.17 4.03 2.99h.065V.31h1.95v23.206h-1.755v-2.86z"
                ]
                []
            , Svg.path
                [ Attributes.fill "#F3336C"
                , Attributes.d "M69.336 6.03h4.486v17.486h-4.486V6.03zm0-5.981h4.486v3.835h-4.486V.05zM76.975 6.03h4.29v2.048h.065c.944-1.723 2.568-2.503 4.453-2.503 2.795 0 4.453 1.527 4.453 4.972v12.97H85.75v-12.06c0-1.755-.585-2.437-1.917-2.437-1.527 0-2.373.943-2.373 2.892v11.604h-4.485V6.03zM97.876.31v12.253h.065l4.518-6.533h4.94l-5.037 6.89 5.785 10.596h-4.94l-3.739-7.183-1.592 2.08v5.103H93.39V.31z"
                ]
                []
            ]
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
facebook : Nri.Ui.Svg.V1.Svg
facebook =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 0 10 19"
        ]
        [ Svg.path
            [ Attributes.d "M10 3.1H8.2c-1.4 0-1.7.7-1.7 1.6v2.1h3.4l-.5 3.4H6.5v8.6H2.9v-8.6H0V6.9h2.9V4.4C2.9 1.6 4.7 0 7.3 0c1.3 0 2.4.1 2.7.1v3z"
            ]
            []
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
clever : Nri.Ui.Svg.V1.Svg
clever =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 0 87 20"
        ]
        [ Svg.g
            [ Attributes.fillRule "evenodd"
            ]
            [ Svg.path
                [ Attributes.d "M20.476 13.846a3.623 3.623 0 0 1-1.303-.726l.407-.836c.38.308.777.532 1.188.671.41.14.872.209 1.386.209.586 0 1.04-.11 1.363-.33.323-.22.485-.532.485-.935 0-.337-.15-.592-.451-.764-.301-.173-.778-.332-1.43-.479-.624-.132-1.133-.284-1.53-.457-.396-.172-.707-.403-.934-.692-.228-.29-.341-.662-.341-1.117 0-.455.12-.856.363-1.205.242-.348.584-.62 1.028-.813.444-.195.955-.292 1.534-.292.55 0 1.066.084 1.546.253.48.169.882.407 1.204.715l-.407.836c-.359-.3-.73-.522-1.11-.666a3.449 3.449 0 0 0-1.221-.214c-.565 0-1.009.12-1.331.358a1.146 1.146 0 0 0-.485.973c0 .352.142.62.424.803.282.183.735.348 1.358.495.653.147 1.181.3 1.584.462.404.161.726.381.968.66.243.279.363.638.363 1.078 0 .455-.12.85-.363 1.188-.242.337-.588.6-1.039.786-.451.188-.98.281-1.59.281-.608 0-1.164-.08-1.666-.242zm10.698-2.596h-3.96c.036 1.298.626 1.947 1.77 1.947.639 0 1.221-.209 1.75-.627l.34.792c-.249.22-.566.394-.951.522a3.65 3.65 0 0 1-1.16.193c-.888 0-1.584-.255-2.09-.764-.507-.51-.76-1.209-.76-2.096 0-.565.112-1.067.336-1.507.224-.44.537-.781.94-1.023a2.62 2.62 0 0 1 1.376-.363c.748 0 1.336.242 1.765.726.429.484.643 1.155.643 2.013v.187zm-3.41-1.727c-.265.25-.433.605-.506 1.067h2.937c-.045-.47-.187-.827-.43-1.072-.242-.246-.568-.369-.979-.369-.418 0-.758.125-1.023.374zm5.637 4.202a2.352 2.352 0 0 1-.952-.995c-.22-.43-.33-.93-.33-1.502s.116-1.078.347-1.518c.231-.44.555-.781.974-1.023.418-.242.905-.363 1.463-.363.388 0 .762.064 1.122.193.359.128.648.302.869.522l-.341.803c-.521-.41-1.045-.616-1.573-.616-.536 0-.954.174-1.255.522-.3.349-.45.838-.45 1.469 0 .63.15 1.116.45 1.457.301.342.72.512 1.255.512.542 0 1.066-.205 1.573-.616l.34.803c-.234.22-.533.392-.896.517a3.48 3.48 0 0 1-1.139.187c-.557 0-1.043-.117-1.457-.352zm9.355-5.269V14h-1.078v-.924a1.84 1.84 0 0 1-.725.742 2.103 2.103 0 0 1-1.046.259c-1.334 0-2.001-.74-2.001-2.222V8.456h1.1v3.388c0 .455.093.79.28 1.007.187.216.471.324.852.324.455 0 .82-.147 1.095-.44.275-.293.413-.682.413-1.166V8.456h1.11zm4.917-.055l-.022 1.012a1.855 1.855 0 0 0-.649-.11c-.506 0-.885.152-1.138.456-.253.305-.38.688-.38 1.15V14h-1.1v-3.982c0-.58-.029-1.1-.087-1.562h1.034l.098 1.001c.147-.367.374-.647.682-.841a1.898 1.898 0 0 1 1.035-.292c.168 0 .344.026.527.077zm5.468 2.849h-3.96c.036 1.298.626 1.947 1.77 1.947.639 0 1.221-.209 1.75-.627l.34.792c-.249.22-.566.394-.951.522a3.65 3.65 0 0 1-1.16.193c-.888 0-1.584-.255-2.09-.764-.507-.51-.76-1.209-.76-2.096 0-.565.112-1.067.336-1.507.224-.44.537-.781.94-1.023a2.62 2.62 0 0 1 1.375-.363c.749 0 1.337.242 1.766.726.429.484.644 1.155.644 2.013v.187zm-3.41-1.727c-.265.25-.433.605-.507 1.067h2.938c-.045-.47-.187-.827-.43-1.072-.242-.246-.568-.369-.978-.369-.419 0-.76.125-1.023.374zm8.618 4.323a3.623 3.623 0 0 1-1.303-.726l.407-.836c.38.308.777.532 1.188.671.41.14.872.209 1.386.209.586 0 1.04-.11 1.364-.33.322-.22.483-.532.483-.935 0-.337-.15-.592-.45-.764-.301-.173-.778-.332-1.43-.479-.624-.132-1.133-.284-1.53-.457-.396-.172-.707-.403-.934-.692-.228-.29-.342-.662-.342-1.117 0-.455.121-.856.363-1.205.243-.348.585-.62 1.029-.813.444-.195.955-.292 1.535-.292.55 0 1.065.084 1.545.253.48.169.882.407 1.205.715l-.407.836c-.36-.3-.73-.522-1.111-.666a3.449 3.449 0 0 0-1.221-.214c-.565 0-1.009.12-1.331.358a1.146 1.146 0 0 0-.485.973c0 .352.142.62.424.803.282.183.735.348 1.358.495.653.147 1.181.3 1.584.462.404.161.726.381.968.66.243.279.364.638.364 1.078 0 .455-.121.85-.364 1.188-.242.337-.588.6-1.039.786-.451.188-.98.281-1.59.281-.608 0-1.164-.08-1.666-.242zm10.929-5.39l-2.586 5.995c-.286.653-.643 1.131-1.072 1.435-.429.305-.959.508-1.59.611l-.242-.858c.521-.117.915-.27 1.183-.457s.482-.467.644-.841l.198-.462-2.333-5.423h1.178l1.737 4.29 1.772-4.29h1.11zm5.796 2.09V14h-1.11v-3.388c0-.484-.094-.836-.281-1.056-.187-.22-.486-.33-.897-.33-.469 0-.845.147-1.127.44-.282.293-.424.686-.424 1.177V14h-1.1v-3.982c0-.58-.029-1.1-.088-1.562h1.035l.099.957c.176-.352.43-.621.764-.809.334-.187.713-.28 1.139-.28 1.327 0 1.99.74 1.99 2.222zm2.536 3.179a2.352 2.352 0 0 1-.951-.995c-.22-.43-.33-.93-.33-1.502s.115-1.078.346-1.518c.231-.44.555-.781.974-1.023.418-.242.905-.363 1.463-.363.388 0 .762.064 1.122.193.359.128.648.302.869.522l-.341.803c-.521-.41-1.045-.616-1.573-.616-.536 0-.954.174-1.254.522-.301.349-.451.838-.451 1.469 0 .63.15 1.116.45 1.457.301.342.72.512 1.255.512.542 0 1.066-.205 1.573-.616l.34.803c-.234.22-.533.392-.896.517a3.48 3.48 0 0 1-1.139.187c-.557 0-1.043-.117-1.457-.352zM5 9.656C5 6.533 7.322 4 10.65 4c2.044 0 3.266.684 4.273 1.678l-1.517 1.756c-.836-.761-1.688-1.227-2.771-1.227-1.827 0-3.143 1.522-3.143 3.387 0 1.896 1.285 3.45 3.143 3.45 1.238 0 1.997-.498 2.848-1.275L15 13.308c-1.115 1.196-2.353 1.942-4.443 1.942C7.368 15.25 5 12.78 5 9.656z"
                ]
                []
            ]
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
cleverC : Nri.Ui.Svg.V1.Svg
cleverC =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.viewBox "0 0 39 44"
        ]
        [ Svg.g
            [ Attributes.stroke "none"
            , Attributes.strokeWidth "1"
            , Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            ]
            [ Svg.g
                [ Attributes.fill "#436CF2"
                , Attributes.fillRule "nonzero"
                ]
                [ Svg.path [ Attributes.d "M0,21.7580775 C0,9.74321254 8.96637318,0 21.8178825,0 C29.708078,0 34.4301716,2.63016953 38.3153078,6.45581374 L32.4575445,13.2103396 C29.2296376,10.2814579 25.9422388,8.48824593 21.7580775,8.48824593 C14.7045264,8.48824593 9.62360245,14.3460092 9.62360245,21.5188573 C9.62360245,28.8113154 14.5849163,34.7890019 21.7580775,34.7890019 C26.5399762,34.7890019 29.4688578,32.8761798 32.7565697,29.8874931 L38.614333,35.8050615 C34.3105615,40.407545 29.5286628,43.2769347 21.4590522,43.2769347 C9.1454752,43.2769347 0,33.7726293 0,21.7580775 Z" ] [] ]
            ]
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
cleverLibrary : Nri.Ui.Svg.V1.Svg
cleverLibrary =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.viewBox "0 0 580 198"
        ]
        [ Svg.g [ Attributes.stroke "none", Attributes.strokeWidth "1", Attributes.fill "none", Attributes.fillRule "evenodd" ]
            [ Svg.g [ Attributes.transform "translate(0.591000, 0.620000)" ]
                [ Svg.rect [ Attributes.fill "#004E95", Attributes.x "0", Attributes.y "0", Attributes.width "579", Attributes.height "197", Attributes.rx "20" ] []
                , Svg.g [ Attributes.transform "translate(72.226665, 38.500000)", Attributes.fill "#FFFFFF" ]
                    [ Svg.path [ Attributes.d "M-7.10542736e-15,60.9449215 C-7.10542736e-15,27.9721331 24.6067124,1.233515 59.8755314,1.233515 C81.528854,1.233515 94.4878507,8.45157562 105.149957,18.9504166 L89.0743052,37.4870838 C80.2158544,29.4492577 71.1941379,24.5280871 59.7114065,24.5280871 C40.3541146,24.5280871 26.4103682,40.6037392 26.4103682,60.2884217 C26.4103682,80.301354 40.0258647,96.7061154 59.7114065,96.7061154 C72.8345281,96.7061154 80.8723542,91.4566949 89.89493,83.2547438 L105.970582,99.4945209 C94.1596008,112.125268 81.0364791,119.999828 58.8907817,119.999828 C25.0982279,119.999828 -7.10542736e-15,93.9168505 -7.10542736e-15,60.9449215 Z" ] []
                    , Svg.polygon [ Attributes.points "108.264894 119.750204 133.198997 119.750204 133.198997 0 108.264894 0" ] []
                    , Svg.path [ Attributes.d "M179.45654,48.4776122 C169.121824,48.4776122 162.396138,55.8597978 160.427498,67.1784043 L197.993207,67.1784043 C196.516942,56.0239227 189.955381,48.4776122 179.45654,48.4776122 M221.94342,82.7625408 L160.755748,82.7625408 C163.216763,94.0811473 171.090464,99.9862083 182.245805,99.9862083 C190.611022,99.9862083 196.681067,97.3619277 203.570878,90.9644919 L217.842015,103.596098 C209.640064,113.76583 197.829082,120 181.917555,120 C155.506328,120 135.98577,101.463333 135.98577,74.5605898 L135.98577,74.2323399 C135.98577,49.134112 153.865937,28.4646798 179.45654,28.4646798 C208.820298,28.4646798 222.27167,51.2668771 222.27167,76.20098 L222.27167,76.5292299 C222.27167,78.9893856 222.107545,80.4656508 221.94342,82.7625408" ] []
                    , Svg.polygon [ Attributes.points "269.693641 119.546723 247.056428 119.546723 212.443249 30.9647926 238.853617 30.9647926 258.539159 89.8555743 278.387966 30.9647926 304.306819 30.9647926" ] []
                    , Svg.path [ Attributes.d "M336.615718,48.4776122 C326.281002,48.4776122 319.555316,55.8597978 317.586676,67.1784043 L355.152385,67.1784043 C353.67612,56.0239227 347.114559,48.4776122 336.615718,48.4776122 M379.102597,82.7625408 L317.914926,82.7625408 C320.375941,94.0811473 328.249642,99.9862083 339.404983,99.9862083 C347.770199,99.9862083 353.840245,97.3619277 360.730055,90.9644919 L375.001192,103.596098 C366.799241,113.76583 354.98826,120 339.076733,120 C312.665505,120 293.144948,101.463333 293.144948,74.5605898 L293.144948,74.2323399 C293.144948,49.134112 311.025115,28.4646798 336.615718,28.4646798 C365.979476,28.4646798 379.430847,51.2668771 379.430847,76.20098 L379.430847,76.5292299 C379.430847,78.9893856 379.266722,80.4656508 379.102597,82.7625408" ] []
                    , Svg.path [ Attributes.d "M433.234529,56.2657277 C416.666502,56.2657277 406.495911,66.2721939 406.495911,87.269876 L406.495911,119.74943 L381.561808,119.74943 L381.561808,31.8239996 L406.495911,31.8239996 L406.495911,49.540042 C411.581206,37.4008107 419.783157,29.5271096 434.546669,30.1827501 L434.546669,56.2657277 L433.234529,56.2657277 Z" ] []
                    ]
                ]
            ]
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
twitter : Nri.Ui.Svg.V1.Svg
twitter =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 0 20 16"
        ]
        [ Svg.path
            [ Attributes.d "M17.9 4.5c0 5.3-4.1 11.4-11.6 11.4-2.3 0-4.5-.7-6.3-1.8h1c1.9 0 3.7-.6 5.1-1.7-1.8 0-3.3-1.2-3.8-2.8.3 0 .5.1.8.1.4 0 .7 0 1.1-.1C2.3 9.2.9 7.6.9 5.7c.5.2 1.1.4 1.8.4C1.6 5.4.9 4.1.9 2.7c0-.7.2-1.4.6-2 2 2.4 5 4 8.4 4.2-.2-.3-.2-.6-.2-.9 0-2.2 1.8-4 4.1-4 1.2 0 2.2.5 3 1.3.9-.2 1.8-.5 2.6-1-.3.9-.9 1.7-1.8 2.2.8-.1 1.6-.3 2.3-.6-.6.8-1.3 1.5-2 2.1v.5z"
            ]
            []
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
googleClassroom : Nri.Ui.Svg.V1.Svg
googleClassroom =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 0 20 20"
        ]
        [ Svg.g
            [ Attributes.stroke "none"
            , Attributes.strokeWidth "1"
            , Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            ]
            [ Svg.g [ Attributes.transform "translate(-302.000000, -217.000000)" ]
                [ Svg.g [ Attributes.transform "translate(66.000000, 207.000000)" ]
                    [ Svg.g [ Attributes.transform "translate(224.000000, 0.000000)" ]
                        [ Svg.image
                            [ Attributes.x "12"
                            , Attributes.y "10"
                            , Attributes.width "20"
                            , Attributes.height "20"
                            , Attributes.xlinkHref
                                "data:image/png;base64,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"
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
googleG : Nri.Ui.Svg.V1.Svg
googleG =
    Svg.svg
        [ Attributes.viewBox "0 0 43 44" ]
        [ Svg.defs []
            [ Svg.style []
                [ Svg.text ".googleG-icon-clip-path-class{clip-path:url(#googleG-icon-clip-path);}" ]
            , Svg.clipPath [ Attributes.id "googleG-icon-clip-path" ]
                [ Svg.path
                    [ Attributes.fill "none"
                    , Attributes.d "M42.5,18H22v8.5H33.8C32.7,31.9,28.1,35,22,35A13,13,0,0,1,22,9a12.72,12.72,0,0,1,8.1,2.9l6.4-6.4A22,22,0,1,0,22,44c11,0,21-8,21-22A18.25,18.25,0,0,0,42.5,18Z"
                    ]
                    []
                ]
            ]
        , Svg.g [ Attributes.class "googleG-icon-clip-path-class" ] [ Svg.path [ Attributes.fill "#fbbc05", Attributes.d "M-2,35V9L15,22Z" ] [] ]
        , Svg.g [ Attributes.class "googleG-icon-clip-path-class" ] [ Svg.path [ Attributes.fill "#ea4335", Attributes.d "M-2,9,15,22l7-6.1L46,12V-2H-2Z" ] [] ]
        , Svg.g [ Attributes.class "googleG-icon-clip-path-class" ] [ Svg.path [ Attributes.fill "#34a853", Attributes.d "M-2,35,28,12l7.9,1L46-2V46H-2Z" ] [] ]
        , Svg.g [ Attributes.class "googleG-icon-clip-path-class" ] [ Svg.path [ Attributes.fill "#4285f4", Attributes.d "M46,46,15,22l-4-3L46,9Z" ] [] ]
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
canvas : Nri.Ui.Svg.V1.Svg
canvas =
    Svg.svg
        [ Attributes.viewBox "200 250 400 115"
        , Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "#E72429"
        ]
        [ Svg.g []
            [ Svg.g []
                [ Svg.path [ Attributes.d "M220.1,306.8c0-7-5.2-12.7-12-13.5c-1.1,4.3-1.7,8.8-1.7,13.5c0,4.7,0.6,9.2,1.7,13.5 C214.9,319.5,220.1,313.7,220.1,306.8z" ] []
                , Svg.path [ Attributes.d "M228,302.5c-2.4,0-4.3,1.9-4.3,4.3c0,2.4,1.9,4.3,4.3,4.3c2.4,0,4.3-1.9,4.3-4.3 C232.3,304.4,230.4,302.5,228,302.5z" ] []
                , Svg.path [ Attributes.d "M287,306.8c0,7,5.2,12.7,12,13.5c1.1-4.3,1.7-8.8,1.7-13.5c0-4.7-0.6-9.2-1.7-13.5 C292.2,294.1,287,299.8,287,306.8z" ] []
                , Svg.path [ Attributes.d "M279,302.5c-2.4,0-4.3,1.9-4.3,4.3c0,2.4,1.9,4.3,4.3,4.3c2.4,0,4.3-1.9,4.3-4.3 C283.3,304.4,281.4,302.5,279,302.5z" ] []
                , Svg.path [ Attributes.d "M253.4,340.3c-7,0-12.7,5.2-13.5,12c4.3,1.1,8.8,1.7,13.5,1.7c4.7,0,9.2-0.6,13.5-1.7 C266.1,345.6,260.4,340.3,253.4,340.3z" ] []
                , Svg.path [ Attributes.d "M253.4,328.1c-2.4,0-4.3,1.9-4.3,4.3c0,2.4,1.9,4.3,4.3,4.3c2.4,0,4.3-1.9,4.3-4.3 C257.7,330.1,255.8,328.1,253.4,328.1z" ] []
                , Svg.path [ Attributes.d "M253.4,273.4c7,0,12.7-5.2,13.5-12c-4.3-1.1-8.8-1.7-13.5-1.7c-4.7,0-9.2,0.6-13.5,1.7 C240.7,268.2,246.5,273.4,253.4,273.4z" ] []
                , Svg.path [ Attributes.d "M253.4,277.1c-2.4,0-4.3,1.9-4.3,4.3c0,2.4,1.9,4.3,4.3,4.3c2.4,0,4.3-1.9,4.3-4.3 C257.7,279,255.8,277.1,253.4,277.1z" ] []
                , Svg.path [ Attributes.d "M277.1,330.5c-4.9,4.9-5.3,12.6-1.1,18c7.9-4.6,14.5-11.2,19.1-19.1C289.8,325.2,282,325.6,277.1,330.5z" ] []
                , Svg.path [ Attributes.d "M268.5,321.8c-1.7,1.7-1.7,4.4,0,6c1.7,1.7,4.4,1.7,6,0c1.7-1.7,1.7-4.4,0-6 C272.9,320.2,270.2,320.2,268.5,321.8z" ] []
                , Svg.path [ Attributes.d "M229.8,283.2c4.9-4.9,5.3-12.6,1.1-18c-7.9,4.6-14.5,11.2-19.1,19.1C217.2,288.4,224.9,288.1,229.8,283.2z" ] []
                , Svg.path [ Attributes.d "M232.4,285.7c-1.7,1.7-1.7,4.4,0,6c1.7,1.7,4.4,1.7,6,0c1.7-1.7,1.7-4.4,0-6 C236.8,284.1,234.1,284.1,232.4,285.7z" ] []
                , Svg.path [ Attributes.d "M277.1,283.1c4.9,4.9,12.6,5.3,18,1.1c-4.6-7.9-11.2-14.5-19.1-19.1C271.8,270.5,272.2,278.2,277.1,283.1z" ] []
                , Svg.path [ Attributes.d "M274.5,291.7c1.7-1.7,1.7-4.4,0-6c-1.7-1.7-4.4-1.7-6,0c-1.7,1.7-1.7,4.4,0,6 C270.1,293.4,272.8,293.4,274.5,291.7z" ] []
                , Svg.path [ Attributes.d "M229.8,330.4c-4.9-4.9-12.6-5.3-18-1.1c4.6,7.9,11.2,14.5,19.1,19.1C235.1,343.1,234.7,335.3,229.8,330.4z" ] []
                , Svg.path [ Attributes.d "M232.4,321.8c-1.7,1.7-1.7,4.4,0,6c1.7,1.7,4.4,1.7,6,0c1.7-1.7,1.7-4.4,0-6 C236.8,320.1,234,320.1,232.4,321.8z" ] []
                ]
            , Svg.g []
                [ Svg.path [ Attributes.d "M363.1,326.7c0,9.2-6,12.7-15.1,12.7h-0.2c-9.1,0-15.1-3.3-15.1-12.7v-35.9c0-8.9,6-12.7,15.1-12.7h0.2 c9.1,0,15.1,3.8,15.1,12.7v7.1h-9.9v-5.6c0-4.2-2.1-5.5-5.2-5.5s-5.2,1.3-5.2,5.5v32.8c0,4.2,2.1,5.5,5.2,5.5s5.2-1.3,5.2-5.5v-7 h9.9V326.7z" ] []
                , Svg.path [ Attributes.d "M398.2,325.3h-10.9l-2.1,13.3h-9.6l11.1-59.9h12.5l11.3,59.9h-10.1L398.2,325.3z M397,317.1l-4.2-26.6 l-4.2,26.6H397z" ] []
                , Svg.path [ Attributes.d "M424,338.7v-59.9h9.9l12.6,37v-37h9.1v59.9h-9.4l-13.1-38.9v38.9H424z" ] []
                , Svg.path [ Attributes.d "M491.3,338.7h-10.9l-11.3-59.9h10.1l6.9,44.4l6.9-44.4h9.6L491.3,338.7z" ] []
                , Svg.path [ Attributes.d "M532.2,325.3h-10.9l-2.1,13.3h-9.6l11.1-59.9h12.5l11.3,59.9h-10.1L532.2,325.3z M530.9,317.1l-4.2-26.6 l-4.2,26.6H530.9z" ] []
                , Svg.path [ Attributes.d "M575.7,296.3v-4.5c0-3.8-2.1-5.1-5-5.1c-2.9,0-5,1.4-5,5.1v4.1c0,3.1,1,4.4,3.8,6.1l7,3.9 c5.7,3.3,9.2,5.9,9.2,12.3v8.6c0,9.2-5.7,12.5-14.8,12.5h-0.2c-9.1,0-14.8-3.2-14.8-12.5v-6.3h9.8v5c0,3.7,2.1,5.3,5.1,5.3 s5.1-1.6,5.1-5.3v-4.7c0-3.1-0.9-4.6-3.9-6.2l-6.9-3.9c-5.8-3.3-9.2-6.1-9.2-12.3v-7.9c0-8.9,6.5-12.3,14.7-12.3h0.2 c8.2,0,14.7,3.4,14.7,12.3v5.9H575.7z" ] []
                ]
            ]
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
canvasCircle : Nri.Ui.Svg.V1.Svg
canvasCircle =
    Svg.svg
        [ Attributes.viewBox "0 0 200 200"
        , Attributes.width "100%"
        , Attributes.height "100%"
        ]
        [ Svg.path
            [ Attributes.fill "#D64027"
            , Attributes.d "M29.2 100c0-14.9-11.2-26.9-25.5-28.4C1.5 80.6 0 89.6 0 100s1.5 19.4 3.7 28.4C18 126.9 29.2 114.2 29.2 100L29.2 100zM46.4 90.3c5 0 9 4 9 9s-4 9-9 9 -9-4-9-9S41.5 90.3 46.4 90.3zM170.8 100c0 14.9 11.2 26.9 25.5 28.4 2.2-9 3.7-18.7 3.7-28.4s-1.5-19.4-3.7-28.4C182 73.1 170.8 85.1 170.8 100L170.8 100zM151.3 90.3c5 0 9 4 9 9s-4 9-9 9c-5 0-9-4-9-9S146.3 90.3 151.3 90.3zM99.6 170.9c-15 0-27 11.2-28.5 25.4 9 2.2 18.7 3.7 28.5 3.7s19.5-1.5 28.5-3.7C126.6 182.1 114.6 170.9 99.6 170.9L99.6 170.9zM98.9 142.5c5 0 9 4 9 9 0 4.9-4 9-9 9 -5 0-9-4-9-9C89.9 146.5 93.9 142.5 98.9 142.5zM99.6 29.1c15 0 27-11.2 28.5-25.4 -9-2.2-18.7-3.7-28.5-3.7S80.1 1.5 71.2 3.7C72.7 17.9 84.6 29.1 99.6 29.1L99.6 29.1zM98.9 38.1c5 0 9 4 9 9s-4 9-9 9c-5 0-9-4-9-9S93.9 38.1 98.9 38.1zM149.8 150c-10.5 10.4-11.2 26.9-2.2 38.1 16.5-9.7 30.7-23.9 40.4-40.3C176.8 138.8 160.3 139.6 149.8 150L149.8 150zM136.3 127.6c5 0 9 4 9 9 0 4.9-4 9-9 9 -5 0-9-4-9-9C127.3 131.6 131.4 127.6 136.3 127.6zM49.4 50c10.5-10.4 11.2-26.9 2.2-38.1C35.2 21.6 21 35.8 11.2 52.2 22.5 61.2 39 60.4 49.4 50L49.4 50zM61.4 53c5 0 9 4 9 9s-4 9-9 9 -9-4-9-9S56.5 53 61.4 53zM149.8 50c10.5 10.4 27 11.2 38.2 2.2 -9.7-16.4-24-30.6-40.4-40.3C138.6 23.1 139.3 39.6 149.8 50L149.8 50zM136.3 53c5 0 9 4 9 9s-4 9-9 9c-5 0-9-4-9-9S131.4 53 136.3 53zM49.4 150c-10.5-10.4-27-11.2-38.2-2.2 9.7 16.4 24 30.6 40.4 40.3C60.7 176.1 59.9 160.4 49.4 150L49.4 150zM61.4 127.6c5 0 9 4 9 9 0 4.9-4 9-9 9s-9-4-9-9C52.4 131.6 56.5 127.6 61.4 127.6z"
            ]
            []
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
schoology : Nri.Ui.Svg.V1.Svg
schoology =
    Svg.svg
        [ Attributes.viewBox "0 0 928 163"
        , Attributes.width "100%"
        , Attributes.height "100%"
        ]
        [ Svg.g [ Attributes.fillRule "nonzero" ]
            [ Svg.path
                [ Attributes.d "M81.5 163C36.6 163 0 126.4 0 81.5S36.6 0 81.5 0 163 36.6 163 81.5c0 45-36.5 81.5-81.5 81.5zm0-149.4c-37.5 0-68 30.5-68 68s30.5 68 68 68 68-30.5 68-68c0-37.6-30.5-68-68-68z"
                , Attributes.fill "#50ade1"
                ]
                []
            , Svg.path
                [ Attributes.d "M78.2 86.3c-13.4-3.2-29.9-6.9-29.9-24.2 0-15.2 12.9-24.8 32.2-24.8 15.9 0 32.9 7.6 35.7 27.2l-15.2 2c-.2-5.1-.5-9.2-6-13.5-5.5-4.2-11.8-4.9-16.1-4.9-11 0-16.4 6.5-16.4 12.2 0 8 9 10.4 20 13.1l8 1.9c9.9 2.3 27.8 6.6 27.8 24.1 0 13.6-12 26.3-35 26.3-9.4 0-19.1-1.9-25.8-6.7-2.7-2-10.8-8.7-12.4-22.6L61 93.9c-.2 3.7-.2 10.8 6.2 16.3 4.9 4.2 11.2 4.8 16.8 4.8 12.4 0 19.6-4.8 19.6-13.8 0-9.5-7.2-11.3-17.3-13.3l-8.1-1.6z"
                , Attributes.fill "#333"
                ]
                []
            ]
        , Svg.g [ Attributes.fill "#333", Attributes.fillRule "nonzero" ]
            [ Svg.path [ Attributes.d "M814.9 98.5h-23v-9.2H830v43.2h-9.5v-10s1.5-1.8 0 .1c-6.9 8.1-16.8 11.7-28.9 11.7-29.7 0-40.3-22.1-40.3-42 0-23 12.2-43.7 39.1-43.7 13 0 32.1 4.6 37.4 27.1l-15.1 1.4c-.5-3.3-3.4-18.5-21.8-18.5-23.5 0-24.5 25.5-24.5 32.7 0 8.8 1.7 16.3 5.3 21.8 4.6 7.4 11.6 10.8 20.1 10.8 19.7 0 21.9-16.1 22.8-22.5l.3-2.9zM241.2 96.7c-13-3.1-29-6.7-29-23.5 0-14.8 12.5-24 31.2-24 15.4 0 31.9 7.4 34.6 26.4l-14.7 1.9c-.2-5-.5-8.9-5.8-13.1-5.3-4.1-11.5-4.8-15.6-4.8-10.7 0-16 6.3-16 11.8 0 7.7 8.7 10.1 19.4 12.7L253 86c9.6 2.2 26.9 6.4 26.9 23.3 0 13.2-11.7 25.5-33.9 25.5-9.1 0-18.5-1.9-25-6.5-2.6-1.9-10.5-8.4-12-22l15.4-2.4c-.2 3.6-.2 10.5 6 15.8 4.8 4.1 10.8 4.6 16.3 4.6 12 0 19-4.6 19-13.4 0-9.2-7-11-16.8-12.9l-7.7-1.3zM366.1 103.2c-.5 3.3-1.4 7.7-4.6 13.7-6.7 12.3-18.2 17.8-33.1 17.8-29.7 0-40.3-22.1-40.3-42 0-23 12.2-43.8 39.1-43.8 13 0 32.1 4.6 37.4 27.1l-15.1 1.4c-.5-3.3-3.4-18.5-21.8-18.5-23.5 0-24.5 25.6-24.5 32.8 0 8.7 1.7 16.3 5.3 21.8 4.6 7.4 11.7 10.8 20.1 10.8 19.7 0 21.9-16.1 22.8-22.5l14.7 1.4zM374.3 11.1h13.9v48.5C396.8 50.5 405 49 412.7 49c17.8 0 24.9 10 27.3 17.9 1.6 5.1 1.6 10.5 1.6 18.5v47h-13.9V89.7c0-10.6 0-16.8-3.3-21.8-3.4-5-8.9-6.9-14.2-6.9-8.8 0-18.2 4.8-20.9 17.3-1 4.5-1 8.8-1 14.7v39.3h-14V11.1zM494.7 134.7c-28 0-39.8-20.9-39.8-42.2 0-18 8.9-43.6 40.3-43.6 26.8 0 39.1 20.8 38.9 42.9-.1 24.6-14.7 42.9-39.4 42.9zm23.7-54.2c-3.1-15.9-13.5-20.6-22.8-20.6-18.4 0-25.7 14.2-25.7 32.8 0 17 7 31 24.7 31 22.8 0 24.5-23.7 24.7-31.9.1-5.1-.4-8.9-.9-11.3zM583.4 134.7c-28 0-39.8-20.9-39.8-42.2 0-18 8.9-43.6 40.3-43.6 26.8 0 39.1 20.8 39 42.9-.3 24.6-14.8 42.9-39.5 42.9zM607 80.5c-3.1-15.9-13.5-20.6-22.8-20.6-18.4 0-25.7 14.2-25.7 32.8 0 17 7 31 24.7 31 22.8 0 24.5-23.7 24.7-31.9.2-5.1-.3-8.9-.9-11.3zM636.7 11.2h13.2v121.1h-13.2zM703.4 134.7c-27.9 0-39.8-20.9-39.8-42.2 0-18 8.9-43.6 40.3-43.6 26.8 0 39.1 20.8 38.9 42.9-.1 24.6-14.7 42.9-39.4 42.9zm23.7-54.2c-3.1-15.9-13.6-20.6-22.8-20.6-18.4 0-25.7 14.2-25.7 32.8 0 17 7 31 24.7 31 22.8 0 24.5-23.7 24.7-31.9.1-5.1-.4-8.9-.9-11.3zM900.8 50.4h-13l-20.9 33-20.4-33h-15.1l28.9 44.4v37.5h13.1V94.8l29.5-44.4h-2.1zM927.2 58.3c0 5-4 8.9-9.1 8.9s-9.1-3.9-9.1-8.9 4.1-8.8 9.2-8.8c5-.1 9 3.8 9 8.8zm-15.9 0c0 3.9 3 7.1 6.9 7.1 3.8.1 6.7-3.1 6.7-7s-2.9-7.1-6.9-7.1c-3.8-.1-6.7 3.1-6.7 7zm5.4 4.6h-2V54c.8-.1 1.9-.3 3.4-.3 1.7 0 2.4.3 3 .7.5.4.9 1 .9 1.9 0 1.1-.8 1.8-1.8 2.1v.1c.9.3 1.3 1 1.6 2.2.3 1.3.5 1.8.6 2.2h-2.2c-.3-.3-.4-1.1-.7-2.2-.2-.9-.7-1.4-1.8-1.4h-1v3.6zm.1-5h1c1.1 0 2-.4 2-1.3 0-.8-.6-1.4-1.9-1.4-.5 0-.9.1-1.1.1v2.6z" ] []
            ]
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
schoologyCircle : Nri.Ui.Svg.V1.Svg
schoologyCircle =
    Svg.svg
        [ Attributes.viewBox "0 0 163 163"
        , Attributes.width "100%"
        , Attributes.height "100%"
        ]
        [ Svg.g []
            [ Svg.path
                [ Attributes.d "M81.5 163C36.6 163 0 126.4 0 81.5S36.6 0 81.5 0 163 36.6 163 81.5c0 45-36.5 81.5-81.5 81.5zm0-149.4c-37.5 0-68 30.5-68 68s30.5 68 68 68 68-30.5 68-68c0-37.6-30.5-68-68-68z"
                , Attributes.fill "#50ade1"
                ]
                []
            , Svg.path
                [ Attributes.d "M78.2 86.3c-13.4-3.2-29.9-6.9-29.9-24.2 0-15.2 12.9-24.8 32.2-24.8 15.9 0 32.9 7.6 35.7 27.2l-15.2 2c-.2-5.1-.5-9.2-6-13.5-5.5-4.2-11.8-4.9-16.1-4.9-11 0-16.4 6.5-16.4 12.2 0 8 9 10.4 20 13.1l8 1.9c9.9 2.3 27.8 6.6 27.8 24.1 0 13.6-12 26.3-35 26.3-9.4 0-19.1-1.9-25.8-6.7-2.7-2-10.8-8.7-12.4-22.6L61 93.9c-.2 3.7-.2 10.8 6.2 16.3 4.9 4.2 11.2 4.8 16.8 4.8 12.4 0 19.6-4.8 19.6-13.8 0-9.5-7.2-11.3-17.3-13.3l-8.1-1.6z"
                , Attributes.fill "#333"
                ]
                []
            ]
        ]
        |> Nri.Ui.Svg.V1.fromHtml
