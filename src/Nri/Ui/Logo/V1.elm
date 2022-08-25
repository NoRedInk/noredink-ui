module Nri.Ui.Logo.V1 exposing
    ( noredink, noredinkMonochrome
    , clever, cleverBlue, cleverC, cleverWhite
    , google, googleClassroom, googleClassroomFull, googleG
    , canvas
    , canvasCircle
    , schoology
    , schoologyCircle
    , facebook, twitter
    )

{-|

@docs noredink, noredinkMonochrome
@docs clever, cleverBlue, cleverC, cleverWhite
@docs google, googleClassroom, googleClassroomFull, googleG
@docs canvas
@docs canvasCircle
@docs schoology
@docs schoologyCircle
@docs facebook, twitter

-}

import Css
import Nri.Ui.Colors.Extra exposing (toCssString)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Svg.V1
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attributes


{-| -}
noredink : Nri.Ui.Svg.V1.Svg
noredink =
    Nri.Ui.Svg.V1.init "0 0 109 24"
        [ Svg.g
            [ Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            ]
            [ Svg.path
                [ Attributes.fill (toCssString Colors.red)
                , Attributes.d noD
                ]
                []
            , Svg.path
                [ Attributes.fill "#333"
                , Attributes.d redD
                ]
                []
            , Svg.path
                [ Attributes.fill (toCssString Colors.red)
                , Attributes.d inkD
                ]
                []
            ]
        ]


{-| -}
noredinkMonochrome : Nri.Ui.Svg.V1.Svg
noredinkMonochrome =
    Nri.Ui.Svg.V1.init "0 0 109 24"
        [ Svg.path [ Attributes.d noD ] []
        , Svg.path [ Attributes.d redD ] []
        , Svg.path [ Attributes.d inkD ] []
        ]


noD : String
noD =
    "M4.29 6.03v2.048h.065c.943-1.723 2.568-2.503 4.453-2.503 2.795 0 4.453 1.527 4.453 4.972v12.97H8.776v-12.06c0-1.755-.586-2.437-1.918-2.437-1.528 0-2.373.943-2.373 2.892v11.604H0V6.03h4.29zM22.559 20.916c1.82 0 2.404-1.788 2.404-6.143 0-4.355-.584-6.143-2.404-6.143-2.21 0-2.405 2.568-2.405 6.143 0 3.575.195 6.143 2.405 6.143zm0-15.341c5.395-.098 6.89 3.12 6.89 9.198 0 5.98-1.755 9.198-6.89 9.198-5.396.098-6.89-3.12-6.89-9.198 0-5.98 1.754-9.198 6.89-9.198z"


redD : String
redD =
    "M32.246 6.257h1.95v2.698h.065c.748-1.918 2.34-3.088 4.356-3.088.227 0 .455.033.682.098v1.95a4.878 4.878 0 0 0-.942-.097c-2.145 0-4.16 1.56-4.16 4.907v10.791h-1.95V6.257zM49.994 13.342c-.065-4.29-1.268-5.85-3.673-5.85-2.405 0-3.608 1.56-3.673 5.85h7.346zm1.918 4.454c-.293 3.672-2.308 6.11-5.558 6.11-3.64 0-5.786-2.535-5.786-9.035 0-5.981 2.145-9.004 5.948-9.004 3.836 0 5.558 2.633 5.558 8.386v.715h-9.426v.813c0 4.972 1.755 6.5 3.673 6.5 2.048 0 3.315-1.463 3.64-4.485h1.95zM60.266 22.28c1.983 0 3.77-1.007 3.77-7.41 0-6.37-1.787-7.378-3.77-7.378-1.95 0-3.77 1.008-3.77 7.379 0 6.402 1.82 7.41 3.77 7.41zm3.965-1.624h-.065c-.52 1.983-2.177 3.25-4.225 3.25-3.802 0-5.525-3.055-5.525-9.035 0-5.948 1.723-9.004 5.525-9.004 2.145 0 3.608 1.17 4.03 2.99h.065V.31h1.95v23.206h-1.755v-2.86z"


inkD : String
inkD =
    "M69.336 6.03h4.486v17.486h-4.486V6.03zm0-5.981h4.486v3.835h-4.486V.05zM76.975 6.03h4.29v2.048h.065c.944-1.723 2.568-2.503 4.453-2.503 2.795 0 4.453 1.527 4.453 4.972v12.97H85.75v-12.06c0-1.755-.585-2.437-1.917-2.437-1.527 0-2.373.943-2.373 2.892v11.604h-4.485V6.03zM97.876.31v12.253h.065l4.518-6.533h4.94l-5.037 6.89 5.785 10.596h-4.94l-3.739-7.183-1.592 2.08v5.103H93.39V.31z"


{-| -}
facebook : Nri.Ui.Svg.V1.Svg
facebook =
    Nri.Ui.Svg.V1.init "0 0 10 19"
        [ Svg.path
            [ Attributes.d "M10 3.1H8.2c-1.4 0-1.7.7-1.7 1.6v2.1h3.4l-.5 3.4H6.5v8.6H2.9v-8.6H0V6.9h2.9V4.4C2.9 1.6 4.7 0 7.3 0c1.3 0 2.4.1 2.7.1v3z"
            ]
            []
        ]


{-| -}
clever : Nri.Ui.Svg.V1.Svg
clever =
    Nri.Ui.Svg.V1.init "0 0 87 20"
        [ Svg.g
            [ Attributes.fillRule "evenodd"
            ]
            [ Svg.path
                [ Attributes.d "M20.476 13.846a3.623 3.623 0 0 1-1.303-.726l.407-.836c.38.308.777.532 1.188.671.41.14.872.209 1.386.209.586 0 1.04-.11 1.363-.33.323-.22.485-.532.485-.935 0-.337-.15-.592-.451-.764-.301-.173-.778-.332-1.43-.479-.624-.132-1.133-.284-1.53-.457-.396-.172-.707-.403-.934-.692-.228-.29-.341-.662-.341-1.117 0-.455.12-.856.363-1.205.242-.348.584-.62 1.028-.813.444-.195.955-.292 1.534-.292.55 0 1.066.084 1.546.253.48.169.882.407 1.204.715l-.407.836c-.359-.3-.73-.522-1.11-.666a3.449 3.449 0 0 0-1.221-.214c-.565 0-1.009.12-1.331.358a1.146 1.146 0 0 0-.485.973c0 .352.142.62.424.803.282.183.735.348 1.358.495.653.147 1.181.3 1.584.462.404.161.726.381.968.66.243.279.363.638.363 1.078 0 .455-.12.85-.363 1.188-.242.337-.588.6-1.039.786-.451.188-.98.281-1.59.281-.608 0-1.164-.08-1.666-.242zm10.698-2.596h-3.96c.036 1.298.626 1.947 1.77 1.947.639 0 1.221-.209 1.75-.627l.34.792c-.249.22-.566.394-.951.522a3.65 3.65 0 0 1-1.16.193c-.888 0-1.584-.255-2.09-.764-.507-.51-.76-1.209-.76-2.096 0-.565.112-1.067.336-1.507.224-.44.537-.781.94-1.023a2.62 2.62 0 0 1 1.376-.363c.748 0 1.336.242 1.765.726.429.484.643 1.155.643 2.013v.187zm-3.41-1.727c-.265.25-.433.605-.506 1.067h2.937c-.045-.47-.187-.827-.43-1.072-.242-.246-.568-.369-.979-.369-.418 0-.758.125-1.023.374zm5.637 4.202a2.352 2.352 0 0 1-.952-.995c-.22-.43-.33-.93-.33-1.502s.116-1.078.347-1.518c.231-.44.555-.781.974-1.023.418-.242.905-.363 1.463-.363.388 0 .762.064 1.122.193.359.128.648.302.869.522l-.341.803c-.521-.41-1.045-.616-1.573-.616-.536 0-.954.174-1.255.522-.3.349-.45.838-.45 1.469 0 .63.15 1.116.45 1.457.301.342.72.512 1.255.512.542 0 1.066-.205 1.573-.616l.34.803c-.234.22-.533.392-.896.517a3.48 3.48 0 0 1-1.139.187c-.557 0-1.043-.117-1.457-.352zm9.355-5.269V14h-1.078v-.924a1.84 1.84 0 0 1-.725.742 2.103 2.103 0 0 1-1.046.259c-1.334 0-2.001-.74-2.001-2.222V8.456h1.1v3.388c0 .455.093.79.28 1.007.187.216.471.324.852.324.455 0 .82-.147 1.095-.44.275-.293.413-.682.413-1.166V8.456h1.11zm4.917-.055l-.022 1.012a1.855 1.855 0 0 0-.649-.11c-.506 0-.885.152-1.138.456-.253.305-.38.688-.38 1.15V14h-1.1v-3.982c0-.58-.029-1.1-.087-1.562h1.034l.098 1.001c.147-.367.374-.647.682-.841a1.898 1.898 0 0 1 1.035-.292c.168 0 .344.026.527.077zm5.468 2.849h-3.96c.036 1.298.626 1.947 1.77 1.947.639 0 1.221-.209 1.75-.627l.34.792c-.249.22-.566.394-.951.522a3.65 3.65 0 0 1-1.16.193c-.888 0-1.584-.255-2.09-.764-.507-.51-.76-1.209-.76-2.096 0-.565.112-1.067.336-1.507.224-.44.537-.781.94-1.023a2.62 2.62 0 0 1 1.375-.363c.749 0 1.337.242 1.766.726.429.484.644 1.155.644 2.013v.187zm-3.41-1.727c-.265.25-.433.605-.507 1.067h2.938c-.045-.47-.187-.827-.43-1.072-.242-.246-.568-.369-.978-.369-.419 0-.76.125-1.023.374zm8.618 4.323a3.623 3.623 0 0 1-1.303-.726l.407-.836c.38.308.777.532 1.188.671.41.14.872.209 1.386.209.586 0 1.04-.11 1.364-.33.322-.22.483-.532.483-.935 0-.337-.15-.592-.45-.764-.301-.173-.778-.332-1.43-.479-.624-.132-1.133-.284-1.53-.457-.396-.172-.707-.403-.934-.692-.228-.29-.342-.662-.342-1.117 0-.455.121-.856.363-1.205.243-.348.585-.62 1.029-.813.444-.195.955-.292 1.535-.292.55 0 1.065.084 1.545.253.48.169.882.407 1.205.715l-.407.836c-.36-.3-.73-.522-1.111-.666a3.449 3.449 0 0 0-1.221-.214c-.565 0-1.009.12-1.331.358a1.146 1.146 0 0 0-.485.973c0 .352.142.62.424.803.282.183.735.348 1.358.495.653.147 1.181.3 1.584.462.404.161.726.381.968.66.243.279.364.638.364 1.078 0 .455-.121.85-.364 1.188-.242.337-.588.6-1.039.786-.451.188-.98.281-1.59.281-.608 0-1.164-.08-1.666-.242zm10.929-5.39l-2.586 5.995c-.286.653-.643 1.131-1.072 1.435-.429.305-.959.508-1.59.611l-.242-.858c.521-.117.915-.27 1.183-.457s.482-.467.644-.841l.198-.462-2.333-5.423h1.178l1.737 4.29 1.772-4.29h1.11zm5.796 2.09V14h-1.11v-3.388c0-.484-.094-.836-.281-1.056-.187-.22-.486-.33-.897-.33-.469 0-.845.147-1.127.44-.282.293-.424.686-.424 1.177V14h-1.1v-3.982c0-.58-.029-1.1-.088-1.562h1.035l.099.957c.176-.352.43-.621.764-.809.334-.187.713-.28 1.139-.28 1.327 0 1.99.74 1.99 2.222zm2.536 3.179a2.352 2.352 0 0 1-.951-.995c-.22-.43-.33-.93-.33-1.502s.115-1.078.346-1.518c.231-.44.555-.781.974-1.023.418-.242.905-.363 1.463-.363.388 0 .762.064 1.122.193.359.128.648.302.869.522l-.341.803c-.521-.41-1.045-.616-1.573-.616-.536 0-.954.174-1.254.522-.301.349-.451.838-.451 1.469 0 .63.15 1.116.45 1.457.301.342.72.512 1.255.512.542 0 1.066-.205 1.573-.616l.34.803c-.234.22-.533.392-.896.517a3.48 3.48 0 0 1-1.139.187c-.557 0-1.043-.117-1.457-.352zM5 9.656C5 6.533 7.322 4 10.65 4c2.044 0 3.266.684 4.273 1.678l-1.517 1.756c-.836-.761-1.688-1.227-2.771-1.227-1.827 0-3.143 1.522-3.143 3.387 0 1.896 1.285 3.45 3.143 3.45 1.238 0 1.997-.498 2.848-1.275L15 13.308c-1.115 1.196-2.353 1.942-4.443 1.942C7.368 15.25 5 12.78 5 9.656z"
                ]
                []
            ]
        ]


{-| -}
cleverBlue : Nri.Ui.Svg.V1.Svg
cleverBlue =
    Nri.Ui.Svg.V1.init "0 0 1023 285"
        [ Svg.g
            [ Attributes.version "1.1"
            , Attributes.x "0px"
            , Attributes.y "0px"
            , Attributes.style "enable-background:new 0 0 1023 285;"
            , Attributes.xmlSpace "preserve"
            ]
            [ Svg.style
                [ Attributes.type_ "text/css"
                ]
                [ Svg.text ".st0{fill-rule:evenodd;clip-rule:evenodd;fill:#436CF2;}" ]
            , Svg.title []
                [ Svg.text "wordmark/blue" ]
            , Svg.desc []
                [ Svg.text "Created with Sketch." ]
            , Svg.g []
                [ Svg.g []
                    [ Svg.path
                        [ Attributes.class "st0"
                        , Attributes.d "M0.2,145.2c0-78,57.9-141.3,140.9-141.3c50.9,0,81.4,17.1,106.5,41.9l-37.8,43.9\n\t\t\tc-20.8-19-42.1-30.7-69.1-30.7c-45.5,0-78.4,38-78.4,84.6c0,47.4,32,86.2,78.4,86.2c30.9,0,49.8-12.4,71-31.8l37.8,38.4\n\t\t\tc-27.8,29.9-58.7,48.5-110.8,48.5C59.2,284.9,0.2,223.2,0.2,145.2z"
                        ]
                        []
                    , Svg.polygon
                        [ Attributes.class "st0"
                        , Attributes.points "254.9,0.9 313.6,0.9 313.6,284.3 254.9,284.3 \t\t"
                        ]
                        []
                    , Svg.path
                        [ Attributes.class "st0"
                        , Attributes.d "M422.4,115.7c-24.3,0-40.1,17.5-44.8,44.3h88.4C462.6,133.5,447.1,115.7,422.4,115.7\n\t\t\t M522.4,196.8h-144c5.8,26.8,24.3,40.8,50.6,40.8c19.7,0,34-6.2,50.2-21.4l33.6,29.9c-19.3,24.1-47.1,38.8-84.5,38.8\n\t\t\tc-62.1,0-108.1-43.9-108.1-107.5v-0.8c0-59.4,42.1-108.3,102.3-108.3c69.1,0,100.7,54,100.7,113v0.8\n\t\t\tC523.2,187.9,522.8,191.4,522.4,196.8"
                        ]
                        []
                    , Svg.polyline
                        [ Attributes.class "st0"
                        , Attributes.points "634.8,283.8 581.5,283.8 500.1,74.2 562.2,74.2 608.5,213.6 655.2,74.2 716.2,74.2\n\t\t\t634.8,283.8 \t\t"
                        ]
                        []
                    , Svg.path
                        [ Attributes.class "st0"
                        , Attributes.d "M792.2,115.7c-24.3,0-40.1,17.5-44.8,44.3h88.4C832.4,133.5,816.9,115.7,792.2,115.7\n\t\t\t M892.2,196.8h-144c5.8,26.8,24.3,40.8,50.6,40.8c19.7,0,34-6.2,50.2-21.4l33.6,29.9c-19.3,24.1-47.1,38.8-84.5,38.8\n\t\t\tc-62.1,0-108.1-43.9-108.1-107.5v-0.8c0-59.4,42.1-108.3,102.3-108.3c69.1,0,100.7,54,100.7,113v0.8\n\t\t\tC893,187.9,892.6,191.4,892.2,196.8"
                        ]
                        []
                    , Svg.path
                        [ Attributes.class "st0"
                        , Attributes.d "M1019.6,134.1c-39,0-62.9,23.7-62.9,73.4v76.9H898V76.2h58.7v41.9c12-28.7,31.3-47.4,66-45.8\n\t\t\tv61.7H1019.6"
                        ]
                        []
                    ]
                ]
            ]
        ]


{-| -}
cleverC : Nri.Ui.Svg.V1.Svg
cleverC =
    Nri.Ui.Svg.V1.init "0 0 39 44"
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


{-| -}
cleverWhite : Nri.Ui.Svg.V1.Svg
cleverWhite =
    Nri.Ui.Svg.V1.init "0 0 1023 285"
        [ Svg.g
            [ Attributes.version "1.1"
            , Attributes.x "0px"
            , Attributes.y "0px"
            , Attributes.style "enable-background:new 0 0 1023 285;"
            , Attributes.xmlSpace "preserve"
            ]
            [ Svg.style
                [ Attributes.type_ "text/css"
                ]
                [ Svg.text ".st1{fill-rule:evenodd;clip-rule:evenodd;fill:#FFFFFF;}" ]
            , Svg.title []
                [ Svg.text "wordmark/blue" ]
            , Svg.desc []
                [ Svg.text "Created with Sketch." ]
            , Svg.g []
                [ Svg.g []
                    [ Svg.path
                        [ Attributes.class "st1"
                        , Attributes.d "M0.2,145.2c0-78,57.9-141.3,140.9-141.3c50.9,0,81.4,17.1,106.5,41.9l-37.8,43.9\n\t\t\tc-20.8-19-42.1-30.7-69.1-30.7c-45.5,0-78.4,38-78.4,84.6c0,47.4,32,86.2,78.4,86.2c30.9,0,49.8-12.4,71-31.8l37.8,38.4\n\t\t\tc-27.8,29.9-58.7,48.5-110.8,48.5C59.2,284.9,0.2,223.2,0.2,145.2z"
                        ]
                        []
                    , Svg.polygon
                        [ Attributes.class "st1"
                        , Attributes.points "254.9,0.9 313.6,0.9 313.6,284.3 254.9,284.3 \t\t"
                        ]
                        []
                    , Svg.path
                        [ Attributes.class "st1"
                        , Attributes.d "M422.4,115.7c-24.3,0-40.1,17.5-44.8,44.3h88.4C462.6,133.5,447.1,115.7,422.4,115.7\n\t\t\t M522.4,196.8h-144c5.8,26.8,24.3,40.8,50.6,40.8c19.7,0,34-6.2,50.2-21.4l33.6,29.9c-19.3,24.1-47.1,38.8-84.5,38.8\n\t\t\tc-62.1,0-108.1-43.9-108.1-107.5v-0.8c0-59.4,42.1-108.3,102.3-108.3c69.1,0,100.7,54,100.7,113v0.8\n\t\t\tC523.2,187.9,522.8,191.4,522.4,196.8"
                        ]
                        []
                    , Svg.polyline
                        [ Attributes.class "st1"
                        , Attributes.points "634.8,283.8 581.5,283.8 500.1,74.2 562.2,74.2 608.5,213.6 655.2,74.2 716.2,74.2\n\t\t\t634.8,283.8 \t\t"
                        ]
                        []
                    , Svg.path
                        [ Attributes.class "st1"
                        , Attributes.d "M792.2,115.7c-24.3,0-40.1,17.5-44.8,44.3h88.4C832.4,133.5,816.9,115.7,792.2,115.7\n\t\t\t M892.2,196.8h-144c5.8,26.8,24.3,40.8,50.6,40.8c19.7,0,34-6.2,50.2-21.4l33.6,29.9c-19.3,24.1-47.1,38.8-84.5,38.8\n\t\t\tc-62.1,0-108.1-43.9-108.1-107.5v-0.8c0-59.4,42.1-108.3,102.3-108.3c69.1,0,100.7,54,100.7,113v0.8\n\t\t\tC893,187.9,892.6,191.4,892.2,196.8"
                        ]
                        []
                    , Svg.path
                        [ Attributes.class "st1"
                        , Attributes.d "M1019.6,134.1c-39,0-62.9,23.7-62.9,73.4v76.9H898V76.2h58.7v41.9c12-28.7,31.3-47.4,66-45.8\n\t\t\tv61.7H1019.6"
                        ]
                        []
                    ]
                ]
            ]
        ]


{-| -}
twitter : Nri.Ui.Svg.V1.Svg
twitter =
    Nri.Ui.Svg.V1.init "0 0 20 16"
        [ Svg.path
            [ Attributes.d "M17.9 4.5c0 5.3-4.1 11.4-11.6 11.4-2.3 0-4.5-.7-6.3-1.8h1c1.9 0 3.7-.6 5.1-1.7-1.8 0-3.3-1.2-3.8-2.8.3 0 .5.1.8.1.4 0 .7 0 1.1-.1C2.3 9.2.9 7.6.9 5.7c.5.2 1.1.4 1.8.4C1.6 5.4.9 4.1.9 2.7c0-.7.2-1.4.6-2 2 2.4 5 4 8.4 4.2-.2-.3-.2-.6-.2-.9 0-2.2 1.8-4 4.1-4 1.2 0 2.2.5 3 1.3.9-.2 1.8-.5 2.6-1-.3.9-.9 1.7-1.8 2.2.8-.1 1.6-.3 2.3-.6-.6.8-1.3 1.5-2 2.1v.5z"
            ]
            []
        ]


{-| -}
google : Nri.Ui.Svg.V1.Svg
google =
    Nri.Ui.Svg.V1.init "0 0 270 90"
        [ Svg.g
            [ Attributes.stroke "none"
            , Attributes.strokeWidth "1"
            , Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            ]
            [ Svg.g
                [ Attributes.transform "translate(0.360000, 0.470000)"
                , Attributes.fillRule "nonzero"
                ]
                [ Svg.path
                    [ Attributes.d "M115.39,46.71 C115.39,59.48 105.4,68.89 93.14,68.89 C80.88,68.89 70.89,59.48 70.89,46.71 C70.89,33.85 80.88,24.53 93.14,24.53 C105.4,24.53 115.39,33.85 115.39,46.71 Z M105.65,46.71 C105.65,38.73 99.86,33.27 93.14,33.27 C86.42,33.27 80.63,38.73 80.63,46.71 C80.63,54.61 86.42,60.15 93.14,60.15 C99.86,60.15 105.65,54.6 105.65,46.71 Z"
                    , Attributes.fill "#EA4335"
                    ]
                    []
                , Svg.path
                    [ Attributes.d "M163.39,46.71 C163.39,59.48 153.4,68.89 141.14,68.89 C128.88,68.89 118.89,59.48 118.89,46.71 C118.89,33.86 128.88,24.53 141.14,24.53 C153.4,24.53 163.39,33.85 163.39,46.71 Z M153.65,46.71 C153.65,38.73 147.86,33.27 141.14,33.27 C134.42,33.27 128.63,38.73 128.63,46.71 C128.63,54.61 134.42,60.15 141.14,60.15 C147.86,60.15 153.65,54.6 153.65,46.71 Z"
                    , Attributes.fill "#FBBC05"
                    ]
                    []
                , Svg.path
                    [ Attributes.d "M209.39,25.87 L209.39,65.69 C209.39,82.07 199.73,88.76 188.31,88.76 C177.56,88.76 171.09,81.57 168.65,75.69 L177.13,72.16 C178.64,75.77 182.34,80.03 188.3,80.03 C195.61,80.03 200.14,75.52 200.14,67.03 L200.14,63.84 L199.8,63.84 C197.62,66.53 193.42,68.88 188.12,68.88 C177.03,68.88 166.87,59.22 166.87,46.79 C166.87,34.27 177.03,24.53 188.12,24.53 C193.41,24.53 197.61,26.88 199.8,29.49 L200.14,29.49 L200.14,25.88 L209.39,25.88 L209.39,25.87 Z M200.83,46.79 C200.83,38.98 195.62,33.27 188.99,33.27 C182.27,33.27 176.64,38.98 176.64,46.79 C176.64,54.52 182.27,60.15 188.99,60.15 C195.62,60.15 200.83,54.52 200.83,46.79 Z"
                    , Attributes.fill "#4285F4"
                    ]
                    []
                , Svg.polygon
                    [ Attributes.fill "#34A853"
                    , Attributes.points "224.64 2.53 224.64 67.53 215.14 67.53 215.14 2.53"
                    ]
                    []
                , Svg.path
                    [ Attributes.d "M261.66,54.01 L269.22,59.05 C266.78,62.66 260.9,68.88 250.74,68.88 C238.14,68.88 228.73,59.14 228.73,46.7 C228.73,33.51 238.22,24.52 249.65,24.52 C261.16,24.52 266.79,33.68 268.63,38.63 L269.64,41.15 L239.99,53.43 C242.26,57.88 245.79,60.15 250.74,60.15 C255.7,60.15 259.14,57.71 261.66,54.01 Z M238.39,46.03 L258.21,37.8 C257.12,35.03 253.84,33.1 249.98,33.1 C245.03,33.1 238.14,37.47 238.39,46.03 L238.39,46.03 Z"
                    , Attributes.fill "#EA4335"
                    ]
                    []
                , Svg.path
                    [ Attributes.d "M34.93,40.94 L34.93,31.53 L66.64,31.53 C66.95,33.17 67.11,35.11 67.11,37.21 C67.11,44.27 65.18,53 58.96,59.22 C52.91,65.52 45.18,68.88 34.94,68.88 C15.96,68.88 -1.42108547e-14,53.42 -1.42108547e-14,34.44 C-1.42108547e-14,15.46 15.96,0 34.94,0 C45.44,0 52.92,4.12 58.54,9.49 L51.9,16.13 C47.87,12.35 42.41,9.41 34.93,9.41 C21.07,9.41 10.23,20.58 10.23,34.44 C10.23,48.3 21.07,59.47 34.93,59.47 C43.92,59.47 49.04,55.86 52.32,52.58 C54.98,49.92 56.73,46.12 57.42,40.93 L34.93,40.94 Z"
                    , Attributes.fill "#4285F4"
                    ]
                    []
                ]
            ]
        ]


{-| -}
googleClassroom : Nri.Ui.Svg.V1.Svg
googleClassroom =
    Nri.Ui.Svg.V1.init "0 0 20 20"
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


{-| -}
googleClassroomFull : Nri.Ui.Svg.V1.Svg
googleClassroomFull =
    Nri.Ui.Svg.V1.init "0 0 857 96"
        [ Svg.g
            [ Attributes.stroke "none"
            , Attributes.strokeWidth "1"
            , Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            ]
            [ Svg.g
                [ Attributes.transform "translate(0.000000, 0.000000)"
                ]
                [ Svg.image
                    [ Attributes.x "-5.68434189e-14"
                    , Attributes.y "0"
                    , Attributes.width "96"
                    , Attributes.height "96"
                    , Attributes.xlinkHref
                        "data:image/png;base64,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"
                    ]
                    []
                , Svg.g
                    [ Attributes.transform "translate(130.590000, 12.440000)"
                    , Attributes.fill "#727272"
                    , Attributes.fillRule "nonzero"
                    ]
                    [ Svg.path
                        [ Attributes.d "M52.445,29.835 L52.445,58.905 C49.6683333,60.265 46.325,61.3416667 42.415,62.135 C38.505,62.9283333 34.51,63.325 30.43,63.325 C24.1966667,63.325 18.785,62.0783333 14.195,59.585 C9.605,57.0916667 6.09166667,53.5216667 3.655,48.875 C1.21833333,44.2283333 0,38.675 0,32.215 C0,25.8683333 1.21833333,20.3716667 3.655,15.725 C6.09166667,11.0783333 9.5625,7.50833333 14.0675,5.015 C18.5725,2.52166667 23.8566667,1.275 29.92,1.275 C34.17,1.275 38.1508333,1.89833333 41.8625,3.145 C45.5741667,4.39166667 48.6766667,6.14833333 51.17,8.415 L47.43,16.405 C44.5966667,14.195 41.82,12.6083333 39.1,11.645 C36.38,10.6816667 33.405,10.2 30.175,10.2 C23.9416667,10.2 19.2241667,12.07 16.0225,15.81 C12.8208333,19.55 11.22,25.0183333 11.22,32.215 C11.22,39.6383333 12.8633333,45.2341667 16.15,49.0025 C19.4366667,52.7708333 24.3383333,54.655 30.855,54.655 C34.8216667,54.655 38.7316667,54.1166667 42.585,53.04 L42.585,37.74 L29.495,37.74 L29.495,29.835 L52.445,29.835 Z"
                        ]
                        []
                    , Svg.path
                        [ Attributes.d "M83.47,63.24 C79.1066667,63.24 75.2675,62.3333333 71.9525,60.52 C68.6375,58.7066667 66.0875,56.1141667 64.3025,52.7425 C62.5175,49.3708333 61.625,45.4183333 61.625,40.885 C61.625,36.3516667 62.5175,32.3991667 64.3025,29.0275 C66.0875,25.6558333 68.6375,23.0633333 71.9525,21.25 C75.2675,19.4366667 79.1066667,18.53 83.47,18.53 C87.7766667,18.53 91.5733333,19.4366667 94.86,21.25 C98.1466667,23.0633333 100.6825,25.6558333 102.4675,29.0275 C104.2525,32.3991667 105.145,36.3516667 105.145,40.885 C105.145,45.4183333 104.2525,49.3708333 102.4675,52.7425 C100.6825,56.1141667 98.1466667,58.7066667 94.86,60.52 C91.5733333,62.3333333 87.7766667,63.24 83.47,63.24 Z M83.385,54.91 C87.0116667,54.91 89.7741667,53.7341667 91.6725,51.3825 C93.5708333,49.0308333 94.52,45.5316667 94.52,40.885 C94.52,36.295 93.5566667,32.7958333 91.63,30.3875 C89.7033333,27.9791667 86.9833333,26.775 83.47,26.775 C79.9,26.775 77.1516667,27.9791667 75.225,30.3875 C73.2983333,32.7958333 72.335,36.295 72.335,40.885 C72.335,45.5316667 73.2841667,49.0308333 75.1825,51.3825 C77.0808333,53.7341667 79.815,54.91 83.385,54.91 Z"
                        ]
                        []
                    , Svg.path
                        [ Attributes.d "M133.79,63.24 C129.426667,63.24 125.5875,62.3333333 122.2725,60.52 C118.9575,58.7066667 116.4075,56.1141667 114.6225,52.7425 C112.8375,49.3708333 111.945,45.4183333 111.945,40.885 C111.945,36.3516667 112.8375,32.3991667 114.6225,29.0275 C116.4075,25.6558333 118.9575,23.0633333 122.2725,21.25 C125.5875,19.4366667 129.426667,18.53 133.79,18.53 C138.096667,18.53 141.893333,19.4366667 145.18,21.25 C148.466667,23.0633333 151.0025,25.6558333 152.7875,29.0275 C154.5725,32.3991667 155.465,36.3516667 155.465,40.885 C155.465,45.4183333 154.5725,49.3708333 152.7875,52.7425 C151.0025,56.1141667 148.466667,58.7066667 145.18,60.52 C141.893333,62.3333333 138.096667,63.24 133.79,63.24 Z M133.705,54.91 C137.331667,54.91 140.094167,53.7341667 141.9925,51.3825 C143.890833,49.0308333 144.84,45.5316667 144.84,40.885 C144.84,36.295 143.876667,32.7958333 141.95,30.3875 C140.023333,27.9791667 137.303333,26.775 133.79,26.775 C130.22,26.775 127.471667,27.9791667 125.545,30.3875 C123.618333,32.7958333 122.655,36.295 122.655,40.885 C122.655,45.5316667 123.604167,49.0308333 125.5025,51.3825 C127.400833,53.7341667 130.135,54.91 133.705,54.91 Z"
                        ]
                        []
                    , Svg.path
                        [ Attributes.d "M206.125,19.635 L206.125,61.2 C206.125,67.8866667 204.283333,72.9583333 200.6,76.415 C196.916667,79.8716667 191.505,81.6 184.365,81.6 C177.735,81.6 171.898333,80.2116667 166.855,77.435 L168.555,69.36 C173.768333,72.08 178.953333,73.44 184.11,73.44 C191.76,73.44 195.585,69.6433333 195.585,62.05 L195.585,53.55 C194.281667,55.8733333 192.383333,57.7575 189.89,59.2025 C187.396667,60.6475 184.591667,61.37 181.475,61.37 C177.735,61.37 174.405833,60.4775 171.4875,58.6925 C168.569167,56.9075 166.3025,54.3858333 164.6875,51.1275 C163.0725,47.8691667 162.265,44.1433333 162.265,39.95 C162.265,35.7566667 163.0725,32.0308333 164.6875,28.7725 C166.3025,25.5141667 168.569167,22.9925 171.4875,21.2075 C174.405833,19.4225 177.735,18.53 181.475,18.53 C184.648333,18.53 187.4675,19.1958333 189.9325,20.5275 C192.3975,21.8591667 194.281667,23.715 195.585,26.095 L195.585,19.635 L206.125,19.635 Z M184.28,53.04 C187.793333,53.04 190.555833,51.8783333 192.5675,49.555 C194.579167,47.2316667 195.585,44.03 195.585,39.95 C195.585,35.87 194.593333,32.6683333 192.61,30.345 C190.626667,28.0216667 187.85,26.86 184.28,26.86 C180.766667,26.86 178.004167,28.0216667 175.9925,30.345 C173.980833,32.6683333 172.975,35.87 172.975,39.95 C172.975,44.03 173.980833,47.2316667 175.9925,49.555 C178.004167,51.8783333 180.766667,53.04 184.28,53.04 Z"
                        ]
                        []
                    , Svg.polygon
                        [ Attributes.points "217.6 62.56 217.6 0 228.14 0 228.14 62.56"
                        ]
                        []
                    , Svg.path
                        [ Attributes.d "M277.355,41.65 L247.775,41.65 C248.001667,46.24 249.149167,49.6258333 251.2175,51.8075 C253.285833,53.9891667 256.36,55.08 260.44,55.08 C265.143333,55.08 269.506667,53.55 273.53,50.49 L276.59,57.8 C274.55,59.4433333 272.0425,60.7608333 269.0675,61.7525 C266.0925,62.7441667 263.075,63.24 260.015,63.24 C252.988333,63.24 247.463333,61.2566667 243.44,57.29 C239.416667,53.3233333 237.405,47.8833333 237.405,40.97 C237.405,36.6066667 238.283333,32.725 240.04,29.325 C241.796667,25.925 244.261667,23.2758333 247.435,21.3775 C250.608333,19.4791667 254.206667,18.53 258.23,18.53 C264.123333,18.53 268.784167,20.4425 272.2125,24.2675 C275.640833,28.0925 277.355,33.3483333 277.355,40.035 L277.355,41.65 Z M258.485,26.18 C255.651667,26.18 253.3425,27.0158333 251.5575,28.6875 C249.7725,30.3591667 248.625,32.7816667 248.115,35.955 L268.09,35.955 C267.75,32.725 266.7725,30.2883333 265.1575,28.645 C263.5425,27.0016667 261.318333,26.18 258.485,26.18 Z"
                        ]
                        []
                    , Svg.path
                        [ Attributes.d "M337.875,63.325 C331.925,63.325 326.725833,62.05 322.2775,59.5 C317.829167,56.95 314.415,53.3375 312.035,48.6625 C309.655,43.9875 308.465,38.505 308.465,32.215 C308.465,25.9816667 309.655,20.5275 312.035,15.8525 C314.415,11.1775 317.829167,7.57916667 322.2775,5.0575 C326.725833,2.53583333 331.925,1.275 337.875,1.275 C341.955,1.275 345.794167,1.9125 349.3925,3.1875 C352.990833,4.4625 355.98,6.23333333 358.36,8.5 L354.79,16.49 C352.07,14.28 349.364167,12.6791667 346.6725,11.6875 C343.980833,10.6958333 341.105,10.2 338.045,10.2 C332.208333,10.2 327.689167,12.0983333 324.4875,15.895 C321.285833,19.6916667 319.685,25.1316667 319.685,32.215 C319.685,39.355 321.271667,44.8375 324.445,48.6625 C327.618333,52.4875 332.151667,54.4 338.045,54.4 C341.105,54.4 343.980833,53.9041667 346.6725,52.9125 C349.364167,51.9208333 352.07,50.32 354.79,48.11 L358.36,56.1 C355.98,58.3666667 352.990833,60.1375 349.3925,61.4125 C345.794167,62.6875 341.955,63.325 337.875,63.325 Z"
                        ]
                        []
                    , Svg.polygon
                        [ Attributes.points "367.88 62.56 367.88 0 378.42 0 378.42 62.56"
                        ]
                        []
                    , Svg.path
                        [ Attributes.d "M431.12,19.635 L431.12,62.56 L420.665,62.56 L420.665,55.675 C419.361667,58.055 417.491667,59.9108333 415.055,61.2425 C412.618333,62.5741667 409.841667,63.24 406.725,63.24 C402.985,63.24 399.67,62.3333333 396.78,60.52 C393.89,58.7066667 391.651667,56.1283333 390.065,52.785 C388.478333,49.4416667 387.685,45.56 387.685,41.14 C387.685,36.72 388.4925,32.7958333 390.1075,29.3675 C391.7225,25.9391667 393.975,23.2758333 396.865,21.3775 C399.755,19.4791667 403.041667,18.53 406.725,18.53 C409.841667,18.53 412.618333,19.1958333 415.055,20.5275 C417.491667,21.8591667 419.361667,23.715 420.665,26.095 L420.665,19.635 L431.12,19.635 Z M409.615,54.91 C413.128333,54.91 415.848333,53.6916667 417.775,51.255 C419.701667,48.8183333 420.665,45.39 420.665,40.97 C420.665,36.4366667 419.701667,32.9516667 417.775,30.515 C415.848333,28.0783333 413.1,26.86 409.53,26.86 C406.016667,26.86 403.2825,28.1208333 401.3275,30.6425 C399.3725,33.1641667 398.395,36.6633333 398.395,41.14 C398.395,45.56 399.3725,48.96 401.3275,51.34 C403.2825,53.72 406.045,54.91 409.615,54.91 Z"
                        ]
                        []
                    , Svg.path
                        [ Attributes.d "M458.405,63.24 C450.641667,63.24 444.465,61.4266667 439.875,57.8 L442.85,50.32 C447.44,53.72 452.681667,55.42 458.575,55.42 C461.125,55.42 463.094167,54.995 464.4825,54.145 C465.870833,53.295 466.565,52.105 466.565,50.575 C466.565,49.2716667 466.083333,48.2375 465.12,47.4725 C464.156667,46.7075 462.513333,46.0416667 460.19,45.475 L453.05,43.86 C445.23,42.16 441.32,38.165 441.32,31.875 C441.32,29.2683333 442.056667,26.9591667 443.53,24.9475 C445.003333,22.9358333 447.071667,21.3633333 449.735,20.23 C452.398333,19.0966667 455.486667,18.53 459,18.53 C462.06,18.53 465.006667,18.9975 467.84,19.9325 C470.673333,20.8675 473.166667,22.2133333 475.32,23.97 L472.175,31.195 C467.868333,28.0216667 463.448333,26.435 458.915,26.435 C456.478333,26.435 454.58,26.8883333 453.22,27.795 C451.86,28.7016667 451.18,29.9483333 451.18,31.535 C451.18,32.7816667 451.590833,33.7733333 452.4125,34.51 C453.234167,35.2466667 454.636667,35.87 456.62,36.38 L463.93,37.995 C468.18,38.9583333 471.2825,40.4458333 473.2375,42.4575 C475.1925,44.4691667 476.17,47.09 476.17,50.32 C476.17,54.2866667 474.583333,57.4316667 471.41,59.755 C468.236667,62.0783333 463.901667,63.24 458.405,63.24 Z"
                        ]
                        []
                    , Svg.path
                        [ Attributes.d "M500.99,63.24 C493.226667,63.24 487.05,61.4266667 482.46,57.8 L485.435,50.32 C490.025,53.72 495.266667,55.42 501.16,55.42 C503.71,55.42 505.679167,54.995 507.0675,54.145 C508.455833,53.295 509.15,52.105 509.15,50.575 C509.15,49.2716667 508.668333,48.2375 507.705,47.4725 C506.741667,46.7075 505.098333,46.0416667 502.775,45.475 L495.635,43.86 C487.815,42.16 483.905,38.165 483.905,31.875 C483.905,29.2683333 484.641667,26.9591667 486.115,24.9475 C487.588333,22.9358333 489.656667,21.3633333 492.32,20.23 C494.983333,19.0966667 498.071667,18.53 501.585,18.53 C504.645,18.53 507.591667,18.9975 510.425,19.9325 C513.258333,20.8675 515.751667,22.2133333 517.905,23.97 L514.76,31.195 C510.453333,28.0216667 506.033333,26.435 501.5,26.435 C499.063333,26.435 497.165,26.8883333 495.805,27.795 C494.445,28.7016667 493.765,29.9483333 493.765,31.535 C493.765,32.7816667 494.175833,33.7733333 494.9975,34.51 C495.819167,35.2466667 497.221667,35.87 499.205,36.38 L506.515,37.995 C510.765,38.9583333 513.8675,40.4458333 515.8225,42.4575 C517.7775,44.4691667 518.755,47.09 518.755,50.32 C518.755,54.2866667 517.168333,57.4316667 513.995,59.755 C510.821667,62.0783333 506.486667,63.24 500.99,63.24 Z"
                        ]
                        []
                    , Svg.path
                        [ Attributes.d "M550.97,18.615 C552.613333,18.615 554.03,18.8416667 555.22,19.295 L555.135,28.985 C553.321667,28.2483333 551.451667,27.88 549.525,27.88 C545.898333,27.88 543.135833,28.9283333 541.2375,31.025 C539.339167,33.1216667 538.39,35.8983333 538.39,39.355 L538.39,62.56 L527.85,62.56 L527.85,31.79 C527.85,27.2566667 527.623333,23.205 527.17,19.635 L537.115,19.635 L537.965,27.2 C539.041667,24.4233333 540.741667,22.2983333 543.065,20.825 C545.388333,19.3516667 548.023333,18.615 550.97,18.615 Z"
                        ]
                        []
                    , Svg.path
                        [ Attributes.d "M579.785,63.24 C575.421667,63.24 571.5825,62.3333333 568.2675,60.52 C564.9525,58.7066667 562.4025,56.1141667 560.6175,52.7425 C558.8325,49.3708333 557.94,45.4183333 557.94,40.885 C557.94,36.3516667 558.8325,32.3991667 560.6175,29.0275 C562.4025,25.6558333 564.9525,23.0633333 568.2675,21.25 C571.5825,19.4366667 575.421667,18.53 579.785,18.53 C584.091667,18.53 587.888333,19.4366667 591.175,21.25 C594.461667,23.0633333 596.9975,25.6558333 598.7825,29.0275 C600.5675,32.3991667 601.46,36.3516667 601.46,40.885 C601.46,45.4183333 600.5675,49.3708333 598.7825,52.7425 C596.9975,56.1141667 594.461667,58.7066667 591.175,60.52 C587.888333,62.3333333 584.091667,63.24 579.785,63.24 Z M579.7,54.91 C583.326667,54.91 586.089167,53.7341667 587.9875,51.3825 C589.885833,49.0308333 590.835,45.5316667 590.835,40.885 C590.835,36.295 589.871667,32.7958333 587.945,30.3875 C586.018333,27.9791667 583.298333,26.775 579.785,26.775 C576.215,26.775 573.466667,27.9791667 571.54,30.3875 C569.613333,32.7958333 568.65,36.295 568.65,40.885 C568.65,45.5316667 569.599167,49.0308333 571.4975,51.3825 C573.395833,53.7341667 576.13,54.91 579.7,54.91 Z"
                        ]
                        []
                    , Svg.path
                        [ Attributes.d "M630.105,63.24 C625.741667,63.24 621.9025,62.3333333 618.5875,60.52 C615.2725,58.7066667 612.7225,56.1141667 610.9375,52.7425 C609.1525,49.3708333 608.26,45.4183333 608.26,40.885 C608.26,36.3516667 609.1525,32.3991667 610.9375,29.0275 C612.7225,25.6558333 615.2725,23.0633333 618.5875,21.25 C621.9025,19.4366667 625.741667,18.53 630.105,18.53 C634.411667,18.53 638.208333,19.4366667 641.495,21.25 C644.781667,23.0633333 647.3175,25.6558333 649.1025,29.0275 C650.8875,32.3991667 651.78,36.3516667 651.78,40.885 C651.78,45.4183333 650.8875,49.3708333 649.1025,52.7425 C647.3175,56.1141667 644.781667,58.7066667 641.495,60.52 C638.208333,62.3333333 634.411667,63.24 630.105,63.24 Z M630.02,54.91 C633.646667,54.91 636.409167,53.7341667 638.3075,51.3825 C640.205833,49.0308333 641.155,45.5316667 641.155,40.885 C641.155,36.295 640.191667,32.7958333 638.265,30.3875 C636.338333,27.9791667 633.618333,26.775 630.105,26.775 C626.535,26.775 623.786667,27.9791667 621.86,30.3875 C619.933333,32.7958333 618.97,36.295 618.97,40.885 C618.97,45.5316667 619.919167,49.0308333 621.8175,51.3825 C623.715833,53.7341667 626.45,54.91 630.02,54.91 Z"
                        ]
                        []
                    , Svg.path
                        [ Attributes.d "M711.62,18.53 C716.493333,18.53 720.12,19.9891667 722.5,22.9075 C724.88,25.8258333 726.07,30.26 726.07,36.21 L726.07,62.56 L715.445,62.56 L715.445,36.55 C715.445,33.15 714.8925,30.6991667 713.7875,29.1975 C712.6825,27.6958333 710.855,26.945 708.305,26.945 C705.358333,26.945 703.049167,27.9791667 701.3775,30.0475 C699.705833,32.1158333 698.87,34.9633333 698.87,38.59 L698.87,62.56 L688.245,62.56 L688.245,36.55 C688.245,33.2066667 687.664167,30.77 686.5025,29.24 C685.340833,27.71 683.513333,26.945 681.02,26.945 C678.073333,26.945 675.75,27.9791667 674.05,30.0475 C672.35,32.1158333 671.5,34.9633333 671.5,38.59 L671.5,62.56 L660.96,62.56 L660.96,31.79 C660.96,27.2566667 660.733333,23.205 660.28,19.635 L670.225,19.635 L670.99,26.435 C672.236667,23.885 674.035833,21.93 676.3875,20.57 C678.739167,19.21 681.445,18.53 684.505,18.53 C691.078333,18.53 695.356667,21.25 697.34,26.69 C698.7,24.14 700.640833,22.1425 703.1625,20.6975 C705.684167,19.2525 708.503333,18.53 711.62,18.53 Z"
                        ]
                        []
                    ]
                ]
            ]
        ]


{-| -}
googleG : Nri.Ui.Svg.V1.Svg
googleG =
    Nri.Ui.Svg.V1.init "0 0 43 44"
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


{-| -}
canvas : Nri.Ui.Svg.V1.Svg
canvas =
    Nri.Ui.Svg.V1.init "200 250 400 115"
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
        |> Nri.Ui.Svg.V1.withColor (Css.hex "#E72429")


{-| -}
canvasCircle : Nri.Ui.Svg.V1.Svg
canvasCircle =
    Nri.Ui.Svg.V1.init "0 0 200 200"
        [ Svg.path
            [ Attributes.fill "#D64027"
            , Attributes.d "M29.2 100c0-14.9-11.2-26.9-25.5-28.4C1.5 80.6 0 89.6 0 100s1.5 19.4 3.7 28.4C18 126.9 29.2 114.2 29.2 100L29.2 100zM46.4 90.3c5 0 9 4 9 9s-4 9-9 9 -9-4-9-9S41.5 90.3 46.4 90.3zM170.8 100c0 14.9 11.2 26.9 25.5 28.4 2.2-9 3.7-18.7 3.7-28.4s-1.5-19.4-3.7-28.4C182 73.1 170.8 85.1 170.8 100L170.8 100zM151.3 90.3c5 0 9 4 9 9s-4 9-9 9c-5 0-9-4-9-9S146.3 90.3 151.3 90.3zM99.6 170.9c-15 0-27 11.2-28.5 25.4 9 2.2 18.7 3.7 28.5 3.7s19.5-1.5 28.5-3.7C126.6 182.1 114.6 170.9 99.6 170.9L99.6 170.9zM98.9 142.5c5 0 9 4 9 9 0 4.9-4 9-9 9 -5 0-9-4-9-9C89.9 146.5 93.9 142.5 98.9 142.5zM99.6 29.1c15 0 27-11.2 28.5-25.4 -9-2.2-18.7-3.7-28.5-3.7S80.1 1.5 71.2 3.7C72.7 17.9 84.6 29.1 99.6 29.1L99.6 29.1zM98.9 38.1c5 0 9 4 9 9s-4 9-9 9c-5 0-9-4-9-9S93.9 38.1 98.9 38.1zM149.8 150c-10.5 10.4-11.2 26.9-2.2 38.1 16.5-9.7 30.7-23.9 40.4-40.3C176.8 138.8 160.3 139.6 149.8 150L149.8 150zM136.3 127.6c5 0 9 4 9 9 0 4.9-4 9-9 9 -5 0-9-4-9-9C127.3 131.6 131.4 127.6 136.3 127.6zM49.4 50c10.5-10.4 11.2-26.9 2.2-38.1C35.2 21.6 21 35.8 11.2 52.2 22.5 61.2 39 60.4 49.4 50L49.4 50zM61.4 53c5 0 9 4 9 9s-4 9-9 9 -9-4-9-9S56.5 53 61.4 53zM149.8 50c10.5 10.4 27 11.2 38.2 2.2 -9.7-16.4-24-30.6-40.4-40.3C138.6 23.1 139.3 39.6 149.8 50L149.8 50zM136.3 53c5 0 9 4 9 9s-4 9-9 9c-5 0-9-4-9-9S131.4 53 136.3 53zM49.4 150c-10.5-10.4-27-11.2-38.2-2.2 9.7 16.4 24 30.6 40.4 40.3C60.7 176.1 59.9 160.4 49.4 150L49.4 150zM61.4 127.6c5 0 9 4 9 9 0 4.9-4 9-9 9s-9-4-9-9C52.4 131.6 56.5 127.6 61.4 127.6z"
            ]
            []
        ]


{-| -}
schoology : Nri.Ui.Svg.V1.Svg
schoology =
    Nri.Ui.Svg.V1.init "0 0 928 163"
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


{-| -}
schoologyCircle : Nri.Ui.Svg.V1.Svg
schoologyCircle =
    Nri.Ui.Svg.V1.init "0 0 163 163"
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
