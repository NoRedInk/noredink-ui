module Nri.Ui.StickerIcon.V1 exposing (exclamation)

{-| }

@docs exclamation

-}

import Css
import Nri.Ui.Svg.V1 exposing (Svg, init)
import Svg.Styled as Svg
import Svg.Styled.Attributes exposing (..)


type alias StickerIcon =
    { simple : Svg
    , detailed : Svg
    }


position : Float -> Float -> Css.Style
position x y =
    Css.transform (Css.translate2 (Css.px x) (Css.px y))


exclamation : StickerIcon
exclamation =
    let
        styles x y =
            [ Css.fill (Css.hex "#20FE27")
            , Css.property "stroke-width" "20px"
            , Css.property "stroke" "#000000"
            , position x y
            ]

        shadowStyles x y =
            [ Css.fill (Css.hex "#000000")
            , position x y
            ]

        core x y =
            Svg.path
                [ css (styles x y)
                , d "M23.4999 348C32.2999 438.4 70.1665 463 87.9999 464H119C135.5 464 157.5 376.5 168.5 348C179.5 319.5 225 105.5 215.5 54.5001C206 3.50011 157.5 13.5001 130.5 4.00011C103.5 -5.49989 52.9999 4.00011 17.4999 34.5001C-18.0001 65.0001 12.4999 235 23.4999 348Z"
                ]
                []

        coreShadow x y =
            Svg.path
                [ css (shadowStyles x y)
                , d "M199.501 68.0825C182.841 20.0919 137.334 9.97144 117.001 0.503906C124.334 23.0301 142.001 83.0673 142.001 117.542C142.001 160.636 82.001 365.82 67.501 407.934C55.901 441.626 12.8343 450.701 0.500977 451.028C10.3343 452.007 44.501 464.739 67.501 451.028C106.001 428.076 120.001 382.96 155.501 272.287C191.001 161.615 208.001 92.5675 199.501 68.0825Z"
                ]
                []

        point x y =
            Svg.ellipse
                [ css (styles x y)
                , cx "63"
                , cy "60"
                , rx "63"
                , ry "60"
                ]
                []

        pointShadow x y =
            Svg.path
                [ css (shadowStyles x y)
                , d "M102.501 18.5445C86.317 6.86047 53.8314 -1.22523 32.8108 0.963756C42.67 6.43623 65.5003 19.9459 78.001 31.7664C86.3489 39.6601 93.0904 57.9229 84.1531 92.9789C74.6659 130.192 27.5323 130.004 0.000976562 128.727C15.0688 146.786 54.1609 145.711 68.001 144.397C85.3011 142.756 115.405 127.378 126.008 97.2792C136.611 67.1807 127.711 36.7451 102.501 18.5445Z"
                ]
                []

        backgroundStyles x y =
            [ Css.fill (Css.hex "#008430")
            , position x y
            ]
    in
    { simple =
        init "0 0 800 800"
            [ -- Backgroud
              Svg.path
                [ css (backgroundStyles 180 0)
                , d "M26.4108 71.9986C-47.5892 167.196 56.4133 467.075 65.9121 535.744C75.4109 604.414 22.9128 630.257 50.9121 711.547C85.0411 810.633 260.448 837.817 308.912 734.073C343.912 659.149 308.912 614.096 308.912 580.797C308.912 495.15 517.412 206.176 406.912 71.9986C349.338 2.08793 118.911 -46.9985 26.4108 71.9986Z"
                ]
                []
            , core 273.81 82.74
            , coreShadow 345.5 95.5
            , point 290 607.24
            , pointShadow 324 598.61
            ]
    , detailed =
        init "0 0 800 800"
            [ -- Background
              Svg.path
                [ css (backgroundStyles 65.41 33.97)
                , d "M14.0002 399.5C39.4438 436.728 84.0002 432.5 107 423.5C63.0002 459.333 -28.341 619.869 66.5 691C130.5 739 261.5 691 261.5 691C261.5 691 333.336 810 484 657C737.569 399.5 713.5 294.5 662 255.5C607.602 214.305 532 243.5 532 243.5C532 243.5 654.5 118.5 597.5 52.4999C513.296 -45.0001 369.5 8.99991 214.5 105.5C45.9324 210.447 -33.5 330 14.0002 399.5Z"
                ]
                []
            , -- Accents
              Svg.g
                [ stroke "black", strokeWidth "20", strokeLinecap "round", fill "none", css [ position 87 45 ] ]
                [ Svg.path
                    [ d "M184 225.5L10 144.5" ]
                    []
                , Svg.path
                    [ d "M62 10V92.5L142 74.5L156 164.5H183.5"
                    ]
                    []
                ]
            , core 320.81 45.47
            , coreShadow 392.5 58.5
            , point 337 581
            , pointShadow 371 572.18
            ]
    }



{-
   exclamationSimple : Svg
   exclamationSimple =
       Debug.todo ""


   exclamationDetailed : Svg
   exclamationDetailed =
       Debug.todo ""


   lightBulbSimple : Svg
   lightBulbSimple =
       Debug.todo ""


   lightBulbDetailed : Svg
   lightBulbDetailed =
       Debug.todo ""


   questionMarkSimple : Svg
   questionMarkSimple =
       Debug.todo ""


   questionMarkDetailed : Svg
   questionMarkDetailed =
       Debug.todo ""


   heartSimple : Svg
   heartSimple =
       Debug.todo ""


   heartDetailed : Svg
   heartDetailed =
       Debug.todo ""


   starSimple : Svg
   starSimple =
       Debug.todo ""


   starDetailed : Svg
   starDetailed =
       Debug.todo ""

-}
