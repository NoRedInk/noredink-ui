module Nri.Ui.Logo.V1 exposing
    ( noredink
    , facebook, twitter, clever, cleverC, googleClassroom, googleG
    )

{-|

@docs noredink
@docs facebook, twitter, clever, cleverC, googleClassroom, googleG

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
                [ Svg.text ".cls-2{clip-path:url(#clip-path);}" ]
            , Svg.clipPath [ Attributes.id "clip-path" ]
                [ Svg.path
                    [ Attributes.fill "none"
                    , Attributes.d "M42.5,18H22v8.5H33.8C32.7,31.9,28.1,35,22,35A13,13,0,0,1,22,9a12.72,12.72,0,0,1,8.1,2.9l6.4-6.4A22,22,0,1,0,22,44c11,0,21-8,21-22A18.25,18.25,0,0,0,42.5,18Z"
                    ]
                    []
                ]
            ]
        , Svg.g [ Attributes.class "cls-2" ] [ Svg.path [ Attributes.fill "#fbbc05", Attributes.d "M-2,35V9L15,22Z" ] [] ]
        , Svg.g [ Attributes.class "cls-2" ] [ Svg.path [ Attributes.fill "#ea4335", Attributes.d "M-2,9,15,22l7-6.1L46,12V-2H-2Z" ] [] ]
        , Svg.g [ Attributes.class "cls-2" ] [ Svg.path [ Attributes.fill "#34a853", Attributes.d "M-2,35,28,12l7.9,1L46-2V46H-2Z" ] [] ]
        , Svg.g [ Attributes.class "cls-2" ] [ Svg.path [ Attributes.fill "#4285f4", Attributes.d "M46,46,15,22l-4-3L46,9Z" ] [] ]
        ]
        |> Nri.Ui.Svg.V1.fromHtml
