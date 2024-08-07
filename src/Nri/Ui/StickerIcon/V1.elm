module Nri.Ui.StickerIcon.V1 exposing (exclamation, lightBulb, questionMark, heart, star)

{-| }

Sticker Icon's were designed to be used for annotating passages. Each icon has a
"simple" and a "detailed" variant. The "simple" variant should look better when scaled down.


# Changelog


## Patch changes

  - Lighten the background colors across all stickers for better contrast
  - Move detailed -> expressive. Add new detailed stickers
  - Add animated stickers

@docs exclamation, lightBulb, questionMark, heart, star

-}

import Css
import Css.Animations
import Css.Media
import Nri.Ui.Svg.V1 exposing (Svg, init)
import Svg.Styled as Svg
import Svg.Styled.Attributes exposing (..)


type alias StickerIcon =
    { simple : Svg
    , detailed : Svg
    , expressive : Svg
    , animated : Svg
    }


position : Float -> Float -> Css.Style
position x y =
    Css.transform (translate2 x y)


translate2 : Float -> Float -> Css.Transform {}
translate2 x y =
    Css.translate2 (Css.px x) (Css.px y)


strokePath : String -> List Css.Style -> Float -> Float -> Svg.Svg msg
strokePath path styles x y =
    Svg.path
        [ strokeWidth "20"
        , stroke "#000000"
        , strokeLinecap "round"
        , fill "none"

        -- HERE BE DRAGONS
        -- Figma reports distances to the box differently for strokes vs paths, so we need
        -- to adjust here by half the stroke width
        , css (position (x - 10) (y - 10) :: styles)
        , d path
        ]
        []


{-| A helper for building animations.

NOTE: Animations are ONLY applied if prefers-reduced-motion is set to "no-preference". To use this properly
you should set the final keyframe style on the object. That combined with `animation-fill-mode: backwards` will make
the styles work both with and without prefers-reduced-motion

-}
withAnimation : { delayMS : Float, durationMS : Float, keyframes : List ( Int, List Css.Animations.Property ) } -> Css.Style
withAnimation { delayMS, durationMS, keyframes } =
    Css.Media.withMediaQuery [ "(prefers-reduced-motion: no-preference)" ]
        [ Css.property "animation-fill-mode" "backwards"
        , Css.property "animation-timing-function" "ease-in-out"
        , Css.animationDelay (Css.ms delayMS)
        , Css.animationDuration (Css.ms durationMS)
        , Css.animationName (Css.Animations.keyframes keyframes)
        ]


{-| -}
exclamation : StickerIcon
exclamation =
    let
        core x y =
            Svg.path
                [ fill "#20FE27"
                , css [ position x y ]
                , d "M23.4999 348C32.2999 438.4 70.1665 463 87.9999 464H119C135.5 464 157.5 376.5 168.5 348C179.5 319.5 225 105.5 215.5 54.5001C206 3.50011 157.5 13.5001 130.5 4.00011C103.5 -5.49989 52.9999 4.00011 17.4999 34.5001C-18.0001 65.0001 12.4999 235 23.4999 348Z"
                ]
                []

        coreStroke =
            strokePath "M121.809 466.672C109.506 478.975 93.2109 473.786 78.386 468.654C61.1417 462.685 56.0068 442.254 51.8998 426.853C45.1002 401.354 35.5921 374.821 31.3594 348.655C28.6722 332.043 22.7518 314.822 21.3595 298.115C20.1516 283.62 18.7928 268.661 16.4046 254.332C12.5383 231.134 13.1614 206.076 13.1614 182.531C13.1614 154.591 8.86767 127.444 10.8191 99.2882C12.5971 73.6342 18.0456 53.1539 32.6207 32.442C43.721 16.6679 62.772 16.0078 79.5572 11.8115C87.6819 9.78037 97.2461 11.0007 105.593 11.0007C118.462 11.0007 132.345 11.6815 145.232 12.7124C165.199 14.3098 187.263 22.2051 201.268 36.9464C217.201 53.7186 222.349 72.5754 222.349 95.4144C222.349 141.809 213.811 188.921 201.268 233.521C193.129 262.458 188.45 292.445 180.908 321.538C177.296 335.468 173.731 350.689 168.836 364.151C164.974 374.771 163.81 386.652 159.106 396.943C155.667 404.465 152.711 413.94 150.998 422.078C149.446 429.448 143.31 434.797 141.358 441.627C138.428 451.884 129.215 459.618 123.431 468.294" []

        coreShadow x y =
            Svg.path
                [ fill "black"
                , css [ position x y ]
                , d "M199.501 68.0825C182.841 20.0919 137.334 9.97144 117.001 0.503906C124.334 23.0301 142.001 83.0673 142.001 117.542C142.001 160.636 82.001 365.82 67.501 407.934C55.901 441.626 12.8343 450.701 0.500977 451.028C10.3343 452.007 44.501 464.739 67.501 451.028C106.001 428.076 120.001 382.96 155.501 272.287C191.001 161.615 208.001 92.5675 199.501 68.0825Z"
                ]
                []

        point x y =
            Svg.ellipse
                [ fill "#20FE27"
                , css [ position x y ]
                , cx "63"
                , cy "60"
                , rx "63"
                , ry "60"
                ]
                []

        pointStroke =
            strokePath "M56.4337 129.415C66.5034 129.415 68.967 130.731 78.8108 129.415C98.9502 126.722 132.238 111.576 135.089 85.9914C137.351 65.6903 136.291 45.7354 121.481 28.9978C111.161 17.3335 90.7733 10 75.3164 10C55.9985 10 43.3884 14.6998 29.5546 28.4953C17.7516 40.2656 10 53.6855 10 70.7127C10 89.4249 23.1644 108.498 38.0216 119.363C44.4342 124.053 49.1611 127.28 56.4337 129.415Z" []

        pointShadow x y =
            Svg.path
                [ fill "black"
                , css [ position x y ]
                , d "M102.501 18.5445C86.317 6.86047 53.8314 -1.22523 32.8108 0.963756C42.67 6.43623 65.5003 19.9459 78.001 31.7664C86.3489 39.6601 93.0904 57.9229 84.1531 92.9789C74.6659 130.192 27.5323 130.004 0.000976562 128.727C15.0688 146.786 54.1609 145.711 68.001 144.397C85.3011 142.756 115.405 127.378 126.008 97.2792C136.611 67.1807 127.711 36.7451 102.501 18.5445Z"
                ]
                []

        backgroundFill =
            fill "#7ECD9A"

        detailedBackground x y =
            Svg.path
                [ backgroundFill
                , css [ position x y ]
                , d "M14.0002 399.5C39.4438 436.728 84.0002 432.5 107 423.5C63.0002 459.333 -28.341 619.869 66.5 691C130.5 739 261.5 691 261.5 691C261.5 691 333.336 810 484 657C737.569 399.5 713.5 294.5 662 255.5C607.602 214.305 532 243.5 532 243.5C532 243.5 654.5 118.5 597.5 52.4999C513.296 -45.0001 369.5 8.99991 214.5 105.5C45.9324 210.447 -33.5 330 14.0002 399.5Z"
                ]
                []

        detailed =
            [ detailedBackground 65.41 33.97
            , core 320.81 45.47
            , coreStroke 322.31 48.64
            , coreShadow 392.5 58.5
            , point 337 581
            , pointStroke 337 581
            , pointShadow 371 572.18
            ]
    in
    { simple =
        init "0 0 800 800"
            [ -- Background
              Svg.path
                [ backgroundFill
                , css [ position 180 0 ]
                , d "M26.4108 71.9986C-47.5892 167.196 56.4133 467.075 65.9121 535.744C75.4109 604.414 22.9128 630.257 50.9121 711.547C85.0411 810.633 260.448 837.817 308.912 734.073C343.912 659.149 308.912 614.096 308.912 580.797C308.912 495.15 517.412 206.176 406.912 71.9986C349.338 2.08793 118.911 -46.9985 26.4108 71.9986Z"
                ]
                []
            , core 273.81 82.74
            , coreStroke 275.31 85.84
            , coreShadow 345.5 95.5
            , point 290 607.24
            , pointStroke 290 607.24
            , pointShadow 324 598.61
            ]
    , detailed =
        init "0 0 800 800"
            detailed
    , expressive =
        init "0 0 800 800"
            (detailed
                ++ [ -- Accent lines
                     strokePath "M184 225.5L10 144.5" [] 87 45
                   , strokePath "M62 10V92.5L142 74.5L156 164.5H183.5" [] 87 45
                   ]
            )
    , animated =
        init "0 0 800 800"
            [ let
                backgroundTranslate =
                    translate2 65.41 33.97
              in
              Svg.g
                [ css
                    [ Css.transform backgroundTranslate
                    , Css.property "transform-origin" "center"
                    , withAnimation
                        { delayMS = 700
                        , durationMS = 350
                        , keyframes =
                            [ ( 0, [ Css.Animations.transform [ Css.scale 0, backgroundTranslate ] ] )
                            , ( 100, [ Css.Animations.transform [ Css.scale 1, backgroundTranslate ] ] )
                            ]
                        }
                    ]
                ]
                [ detailedBackground 0 0
                ]
            , let
                ( pointX, pointY ) =
                    ( 290, 607.24 )
              in
              Svg.g
                [ css
                    [ Css.transform (translate2 pointX pointY)
                    , withAnimation
                        { delayMS = 0
                        , durationMS = 1000
                        , keyframes =
                            [ ( 0, [ Css.Animations.transform [ translate2 pointX pointY, Css.scaleY 0.5 ] ] )
                            , ( 36, [ Css.Animations.transform [ translate2 pointX 0, Css.scaleY 1 ] ] )
                            , ( 60, [ Css.Animations.transform [ translate2 pointX pointY, Css.scaleY 0.5 ] ] )
                            , ( 68, [ Css.Animations.transform [ translate2 pointX pointY, Css.scaleY 0.5 ] ] )
                            , ( 80, [ Css.Animations.transform [ translate2 pointX (pointY * 0.75) ] ] )
                            , ( 100, [ Css.Animations.transform [ translate2 pointX pointY ] ] )
                            ]
                        }
                    ]
                ]
                [ point 0 0
                , pointStroke 0 0
                , pointShadow (324 - pointX) (598.61 - pointY)
                ]
            , let
                ( coreLeft, coreTop ) =
                    ( 273.81, 82.74 )
              in
              Svg.g
                [ css
                    [ Css.transforms [ Css.translate2 (Css.px coreLeft) (Css.px coreTop) ]
                    , Css.property "transform-origin" "top center"
                    , withAnimation
                        { delayMS = 350
                        , durationMS = 620
                        , keyframes =
                            [ ( 0, [ Css.Animations.transform [ Css.translate2 (Css.px coreLeft) (Css.px -800), Css.scaleY 1.3 ] ] )
                            , ( 56, [ Css.Animations.transform [ Css.translate2 (Css.px coreLeft) (Css.px coreTop) ] ] )
                            , ( 68, [ Css.Animations.transform [ Css.translate2 (Css.px coreLeft) (Css.px coreTop), Css.scaleY 0.7 ] ] )
                            , ( 100, [ Css.Animations.transform [ Css.translate2 (Css.px coreLeft) (Css.px coreTop) ] ] )
                            ]
                        }
                    ]
                ]
                [ core (273.81 - coreLeft) (82.74 - coreTop)
                , coreStroke (275.31 - coreLeft) (85.84 - coreTop)
                , coreShadow (345.5 - coreLeft) (95.5 - coreTop)
                ]
            ]
    }


{-| -}
lightBulb : StickerIcon
lightBulb =
    let
        core x y =
            Svg.path
                [ fill "#FFE561"
                , strokeWidth "0"
                , d "M288 597.5C283.2 614.7 208.667 612 172 608.5C101.5 606.262 114 583.5 114 543C114 502.5 117.5 461.5 87.5001 408.5C57.5001 355.5 -10.9999 291 1.50012 178.5C14.0001 66.0001 148.5 5.00012 230 0.500121C311.5 -3.99988 383 88.0001 427 166C471 244 344 385 309.5 446C275 507 294 576 288 597.5Z"
                , css [ position x y ]
                ]
                []

        coreStroke =
            strokePath "M122.147 507.217C127.578 537.091 106.12 606.952 154.356 607.917C171.714 611.073 183.092 612.607 203.733 612.607C221.448 610.076 234.092 612.607 253.592 607.917C262.407 607.917 281.532 604.611 288.151 598.36C297.975 589.081 294.522 572.377 294.522 560.133V495.625C294.522 474.57 303.648 455.675 313.636 438.196C324.567 419.066 336.538 404.663 348.092 386C353.41 377.408 359.405 368.006 365.592 360C370.714 353.371 375.037 343.172 380.592 337C390.017 326.527 393.685 312.116 399.092 299.5C403.667 288.824 409.02 274.918 410.442 263.696C411.329 256.701 410.767 248.011 413.628 241.574C415.415 237.553 413.982 230.026 413.982 225.646V200.339C413.982 179.831 408.44 155.816 400.443 137.158C387.171 106.191 368.561 78.7347 341.51 57.695C336.261 53.6128 332.392 47.5869 326.378 44.4218C317.447 39.7213 308.429 36.2844 299.478 31.1485C286.788 23.8677 273.374 21.6963 259.835 17.3443C247.751 13.4602 231.591 10.6191 218.864 10.6191C201.141 10.6191 186.086 16.0098 168.691 18.4946C153.095 20.7227 136.914 30.5777 122.147 35.5002C109.334 39.771 97.5916 53.5002 85.8662 62.8274C73.9014 80.1099 55.9036 92.6833 46.0464 111.85C41.6114 120.474 34.6936 127.808 30.1184 136.45C23.5598 148.838 17.9579 162.616 14.5444 176.27C10.1788 193.732 11.0049 214.024 11.0049 232.017C11.0049 248.477 15.3399 264.751 17.4646 280.686C18.7855 290.593 25.6372 298.288 28.5256 307.675C32.3867 320.224 42.276 336.557 51.6211 345.902C56.2581 350.539 61.1373 358.573 64.3635 364.308C69.8502 374.062 76.8867 382.895 81.8842 392.89C90.4779 410.077 99.5819 426.577 107.811 443.859C116.779 462.691 118.457 486.923 122.147 507.217Z" []

        shadow x y =
            Svg.path
                [ fill "black"
                , css [ position x y ]
                , d "M112 0.5C201.5 0.5 262.701 21.882 312 89C353.5 145.5 348.083 222.5 327 275C301.5 338.5 250.5 407 227.5 443C220.879 453.363 206.137 479.34 201.5 503.5V581C201.5 600 204 624.5 164.5 627.5C125 630.5 77.5 636 44.5 627.5C30.9425 624.008 6.33333 608.5 0 599C38.8333 603.333 132 609.4 152 599C177 586 157.5 524.5 177 483C196.5 441.5 239 360.5 258 321C277 281.5 315.5 210 291.5 165.5C267.5 121 261.5 84.5 236 65.5C215.6 50.3 123.167 7.83333 112 0.5Z"
                ]
                []

        filament =
            strokePath "M64.0914 357.5C49.0914 338.5 38.8997 317.346 31.7392 298.933C27.3707 287.699 24.1316 276.317 20.6315 264.816C17.1452 253.361 12.3253 240.399 11.9922 228.408C11.2938 203.266 7.47998 177.935 14.3724 153.122C22.0657 125.426 40.0609 92.9998 70.7043 92.9998C96.3848 92.9998 123.773 101.079 138.091 120C147.929 133 154.272 158.122 143.091 173.5C126.591 189.594 115.004 186.363 101.735 184.594C72.0358 180.634 49.904 150.062 38.968 124.736C31.3348 107.059 27.8603 90.5908 27.8603 71.3133C27.8603 50.157 35.4865 30.4062 40.5548 10.1328"

        thread1 =
            strokePath "M10.6646 10.6001C37.4086 10.6001 63.9923 15.3605 91.2394 15.3605C104.65 15.3605 117.093 12.1869 130.204 12.1869C142.734 12.1869 180.288 10.6001 167.759 10.6001" []

        thread2 =
            strokePath "M10.2515 12.0967C25.1139 12.0967 39.0431 15.2704 53.8888 15.2704C63.3224 15.2704 72.5977 13.6835 82.0107 13.6835C90.5722 13.6835 98.2789 15.6019 106.694 14.0362C118.372 11.8635 133.933 12.0967 145.836 12.0967C150.361 12.0967 154.887 12.0967 159.412 12.0967C163.129 12.0967 176.726 9.20392 175.28 12.0967" []

        backgroundFill =
            fill "#FDB157"

        detailedBackground x y =
            Svg.path
                [ backgroundFill
                , d "M99.9995 597C157.5 676 374.5 644.667 471 606.5C513.166 599.167 644.599 540.722 664 481C689.5 402.5 612.499 410.5 612.499 410.5C612.499 410.5 714.999 315 698.999 242C682.999 169 597.997 173.5 597.997 173.5C597.997 173.5 667.497 14.0002 520.999 1.00022C380.324 -11.483 379.795 124.973 162.5 163C-17.4972 194.5 -21.0108 328.5 25.9983 388C56.416 426.5 112.497 443 112.497 443C112.497 443 42.4995 518 99.9995 597Z"
                , css [ position x y ]
                ]
                []

        detailed =
            [ detailedBackground 49.49 93.2
            , core 185.99 150.34
            , coreStroke 191.35 156.12
            , shadow 304 149.5
            , filament [] 336.72 306.63
            , thread1 306.66 657.6
            , thread2 308.25 702.81
            ]
    in
    { simple =
        init "0 0 800 800"
            [ -- Background
              Svg.path
                [ backgroundFill
                , d "M308.119 781H309.002C328.502 781 391.766 780.152 428.502 753.5C479.502 716.5 467.502 648 479.502 574.5C491.502 501 580.502 479.5 602.002 298.5C623.502 117.5 498.502 27.0002 348.502 4.00016C198.502 -18.9998 62.2146 80.2676 12.4997 213.5C-37.5015 347.5 90.4998 512 106 559.5C121.5 607 97.0018 676 148.002 734.5C188.545 781.005 261.573 781.002 308.119 781Z"
                , css [ position 97.98 9.68 ]
                ]
                []
            , core 176.58 99.84
            , coreStroke 181.94 105.62
            , shadow 294.59 105.62
            , filament [] 327.31 256.13
            , thread1 297.26 607.1
            , thread2 298.84 652.31
            ]
    , detailed =
        init "0 0 800 800"
            detailed
    , expressive =
        init "0 0 800 800"
            (detailed
                ++ [ strokePath "M108 56L10 10" [] 112 165
                   , strokePath "M89.9488 132.473L10 10" [] 158.78 30
                   , strokePath "M10.1481 111.617L21 10" [] 315.87 19
                   ]
            )
    , animated =
        init "0 0 800 800"
            [ -- Background animation
              let
                backgroundTranslate =
                    translate2 49.49 93.2
              in
              Svg.g
                [ css
                    [ Css.transform backgroundTranslate
                    , Css.property "transform-origin" "center"
                    , withAnimation
                        { delayMS = 630
                        , durationMS = 550
                        , keyframes =
                            [ ( 0, [ Css.Animations.transform [ Css.scale 0, backgroundTranslate ] ] )
                            , ( 100, [ Css.Animations.transform [ Css.scale 1, backgroundTranslate ] ] )
                            ]
                        }
                    ]
                ]
                [ detailedBackground 0 0
                ]
            , -- Bulb animation
              let
                ( coreX, coreY ) =
                    ( 185.99, 150.34 )
              in
              Svg.g
                [ css
                    [ Css.transform (translate2 coreX coreY)
                    , Css.property "transform-origin" "bottom center"
                    , withAnimation
                        { delayMS = 0
                        , durationMS = 670
                        , keyframes =
                            [ ( 0, [ Css.Animations.transform [ translate2 coreX coreY, Css.scaleY 0.1 ] ] )
                            , ( 82, [ Css.Animations.transform [ translate2 coreX coreY, Css.scaleY 1.15 ] ] )
                            , ( 100, [ Css.Animations.transform [ translate2 coreX coreY, Css.scaleY 1 ] ] )
                            ]
                        }
                    ]
                ]
                [ core 0 0
                , coreStroke (191.35 - coreX) (156.12 - coreY)
                , shadow (304 - coreX) (149.5 - coreY)
                , thread1 (306.66 - coreX) (657.6 - coreY)
                , thread2 (308.25 - coreX) (702.81 - coreY)
                ]
            , -- Filament animation
              filament
                [ -- This is a fun trick to animate the filament line being drawn
                  -- First we set the stroke-dasharray to the length of the entire segment
                  -- Then we can aniamtion the stroke-dashoffset to progressively show more and more of it
                  Css.property "stroke-dasharray" "690"
                , withAnimation
                    { delayMS = 630
                    , durationMS = 550
                    , keyframes =
                        [ ( 0, [ Css.Animations.custom "stroke-dashoffset" "690" ] )
                        , ( 100, [ Css.Animations.custom "stroke-dashoffset" "0" ] )
                        ]
                    }
                ]
                327.31
                256.13
            ]
    }


{-| -}
questionMark : StickerIcon
questionMark =
    let
        core x y =
            Svg.path
                [ fill "#A30FFC"
                , css [ position x y ]
                , d "M1.00024 159C-3.79976 208.2 37.9999 227.667 61.9999 234C67.8332 234 84.9999 231.7 107 222.5C134.5 211 158.5 148.5 206.5 148.5C254.5 148.5 247.5 194.5 238.5 222.5C229.5 250.5 150 294 139.5 374.5C132.723 426.453 162.5 473.5 211 472C259.5 470.5 244 434.575 244 374.5C244 334 281.032 307.141 348.5 222.5C403.5 153.5 371.5 95.0002 348.5 55.5002C325.5 16.0002 233 -13.4998 139.5 7.00017C45.9999 27.5002 7.00024 97.5002 1.00024 159Z"
                ]
                []

        coreStroke =
            strokePath "M226.999 479.49C220.5 480.971 216.842 480.971 211.369 480.971C204.663 480.971 196.5 479.49 191.5 478.092C179.354 474.694 169.452 463.455 162.5 454C145.5 426 137.687 394.619 150.329 359.221C157.194 339.999 169.459 323.372 181.754 307.724C189.326 298.087 197.433 288.753 205.939 279.919C212.58 273.023 217.716 265.428 224.366 258.531C237.387 245.027 250.183 228.502 256.614 210.818C261.072 198.557 256.078 175.708 242.464 168.5C228.387 161.047 218.875 160.487 202.32 162.694C189.759 164.369 178.631 174.741 168.921 181.861C160.312 188.175 153.586 194.728 146.463 201.851C141.28 207.034 138.145 214.447 132.89 219.703C123.745 228.847 104.693 246.493 91.0001 247C65.3419 247.95 33.5012 234.569 21.0005 210.818C1.38507 173.549 12.3767 130.386 32 95.5002C42.929 76.0709 65.9242 61.3926 82.709 48.924C89.9826 43.5207 98.0606 39.3606 105.578 34.5279C120.548 24.9044 137.791 21.9882 154.525 16.759C171.927 11.3207 190.213 10.0957 208.407 10.0957C219.933 10.0957 231.087 10.4106 242.464 11.4942C250.893 12.2969 258.835 14.773 266.979 15.9364C285.096 18.5245 305.949 28.7114 320.614 39.7105C335.069 50.5517 347.906 61.8695 359.525 76.0709C386.519 109.064 392.6 153.296 382.476 193.79C380.506 201.671 375.223 210.964 370.301 217.399C364.896 224.468 360.787 232.493 355.823 239.939C345.988 254.692 333.62 270.698 321.026 283.292C311.42 292.898 300.045 305.148 293.303 316.609C289.994 322.234 285.785 327.022 282.527 332.815C276.901 342.815 268.742 353.923 264 364.5C255.521 394 251.418 401.096 251.5 428C251.582 454.904 246.688 470.498 231.5 478.092" []

        coreShadow x y =
            Svg.path
                [ fill "black"
                , css [ position x y ]
                , d "M124.5 19.9999C76.5 -3.50012 64.8333 0.833217 55 1.99988C65.1667 9.49988 111.9 34.5999 135.5 46.9999C165 62.4999 175.5 116 180 157.5C184.5 199 112 272.5 87.5 297C63 321.5 41.5 382 40.5 420.5C39.7 451.3 13 468 0.5 478C40.3759 478 55.0091 464.782 67.3845 453.604L67.5 453.5C83 439.5 75.5 416 87.5 358.5C99.5 301 206 279 211.5 147.5C214.3 80.5584 157.257 36.0374 124.5 19.9999Z"
                ]
                []

        point x y =
            Svg.ellipse
                [ fill "#A30FFC"
                , css [ position x y ]
                , cx "60.5"
                , cy "55.5"
                , rx "60.5"
                , ry "55.5"
                ]
                []

        pointStroke =
            strokePath "M45.4918 121.373C36.9713 120.426 27.9136 109.681 22.4949 104.062C17.0681 98.4344 12.4969 87.183 11.1228 79.549C8.39273 64.3819 11.273 50.0748 18.1988 36.5878C22.6641 27.892 31.0016 25.3489 38.0367 19.5928C50.8765 9.08758 71.9871 9.18715 87.5053 11.1269C95.6356 12.1432 106.08 20.4776 111.45 25.8475C116.857 31.2551 122.969 35.1221 125.981 42.6529C129.351 51.0771 130.782 56.7989 130.782 65.6497C130.782 71.81 129.874 77.6525 129.645 83.5924C129.482 87.8367 122.742 97.1278 119.645 104.062C114.173 110.142 111.531 112.59 104.121 116.824C95.229 121.905 88.5595 127.332 78.1447 127.332C66.2499 131.016 54.8604 124.584 50.6447 123.647" []

        pointShadow x y =
            Svg.path
                [ fill "black"
                , css [ position x y ]
                , d "M99.5 19C84.3 7.8 59.1667 0.333333 48.5 0C63 14.8333 99.5 19 92 89C88.5483 121.216 28.3333 125.167 0 124C7.83333 129 18 135.9 48.5 142C79 148.1 114 118.5 120 89C126 59.5 118.5 33 99.5 19Z"
                ]
                []

        backgroundFill =
            fill "#EED1FF"

        detailedBackground x y =
            Svg.path
                [ backgroundFill
                , css [ position x y ]
                , d "M399.501 689.5C468.301 700.7 480.501 637.833 478.001 605C520.501 623 611.001 627 631.001 538.5C651.001 450 576.668 428.167 554.501 418C563.834 412.667 616.002 400 631.001 349.5C646 299 638 256 604 220C570 184 441 182.5 364 87C290.098 -4.65719 196.5 -30.5 142.501 46C102.499 102.67 131.834 156.5 158.501 182.5C114 182.5 21.5001 184.5 3.50016 273C-14.4998 361.5 57.8344 396.333 100.001 418C73.8344 428.667 24.801 465.3 38.001 526.5C54.501 603 313.501 675.5 399.501 689.5Z"
                ]
                []

        details =
            [ detailedBackground 81.68 45.98
            , core 201.62 124.1
            , coreStroke 202.68 124.1
            , coreShadow 404.5 125.92
            , point 338.46 649.28
            , pointStroke 338.46 649.28
            , pointShadow 366 641
            ]
    in
    { simple =
        init "0 0 800 800"
            [ -- Background
              Svg.path
                [ backgroundFill
                , css [ position 117 7 ]
                , d "M1.55518 263.832C8.35518 338.232 90.7216 380.165 131.055 393.332C158.555 422.331 113.555 474.332 131.055 529.332C143.218 567.559 162.621 573.165 158.555 597.832C143.555 688.832 161.125 785.832 302.055 785.832C365.431 785.832 406.063 739.527 414.555 690.832C426.414 622.832 399.372 590.643 414.555 577.332C451.055 545.332 454.555 492.331 462.555 433.331C470.555 374.332 583.555 332.832 563.055 187.831C549.729 93.5717 435.055 -28.1683 224.055 5.8317C13.0549 39.8317 -6.94482 170.832 1.55518 263.832Z"
                ]
                []
            , core 201.26 81.13
            , coreStroke 202.33 79.93
            , coreShadow 404.14 81.75
            , point 337.64 604.8
            , pointStroke 337.64 604.8
            , pointShadow 365.64 596.83
            ]
    , detailed =
        init "0 0 800 800"
            details
    , expressive =
        init "0 0 800 800"
            (details
                ++ [ strokePath
                        "M18.0001 137.5C34.0001 140 65.9997 121.623 74.4999 91.6229C86.3779 49.7014 68.5002 10.5 39.5001 10.5C-0.996813 10.5 4.99996 56 34 75.5C63 95 115.5 112 155 36.5"
                        []
                        573.69
                        16.5
                   ]
            )
    , animated =
        init "0 0 800 800"
            [ let
                backgroundTranslate =
                    translate2 81.68 45.98
              in
              Svg.g
                [ css
                    [ Css.transform backgroundTranslate
                    , Css.property "transform-origin" "center"
                    , withAnimation
                        { delayMS = 310
                        , durationMS = 470
                        , keyframes =
                            [ ( 0, [ Css.Animations.transform [ Css.scale 0, backgroundTranslate ] ] )
                            , ( 100, [ Css.Animations.transform [ Css.scale 1, backgroundTranslate ] ] )
                            ]
                        }
                    ]
                ]
                [ detailedBackground 0 0
                ]
            , let
                ( markX, markY ) =
                    ( 201.62, 124.1 )

                markTranslate =
                    translate2 markX markY
              in
              Svg.g
                [ css
                    [ Css.transform markTranslate
                    , Css.property "transform-origin" "center center"
                    , withAnimation
                        { delayMS = 0
                        , durationMS = 470
                        , keyframes =
                            [ ( 0, [ Css.Animations.transform [ Css.scale 0, Css.rotate (Css.deg -135), markTranslate ] ] )
                            , ( 33, [ Css.Animations.transform [ Css.scale 0.9, markTranslate ] ] )
                            , ( 50, [ Css.Animations.transform [ Css.scale 1, Css.rotate (Css.deg 35), markTranslate ] ] )
                            , ( 75, [ Css.Animations.transform [ Css.scale 0.9, Css.rotate (Css.deg -20), markTranslate ] ] )
                            , ( 100, [ Css.Animations.transform [ Css.scale 1, markTranslate ] ] )
                            ]
                        }
                    ]
                ]
                [ core 0 0
                , coreStroke (202.68 - markX) (124.1 - markY)
                , coreShadow (404.5 - markX) (125.92 - markY)
                , point (338.46 - markX) (649.28 - markY)
                , pointStroke (338.46 - markX) (649.28 - markY)
                , pointShadow (366 - markX) (641 - markY)
                ]
            ]
    }


{-| -}
heart : StickerIcon
heart =
    let
        core x y =
            Svg.path
                [ fill "#E42F01"
                , css [ position x y ]
                , d "M367.138 10.9893C313.138 3.01679 262.305 61.8226 251.138 89.4893C227.305 53.4893 162.138 -14.9832 92.1381 3.01679C4.63806 25.5168 -15.3619 104.017 12.6381 216.517C35.0381 306.517 163.472 418.183 229.138 448.517C331.138 402.017 469.138 313.517 502.138 161.017C527.74 42.7076 448.875 23.0569 367.138 10.9893Z"
                ]
                []

        leftStroke =
            strokePath "M256.947 453.952C239.769 453.952 231.471 452.747 217.158 443.035C198.395 430.303 180.947 417.356 162.812 403.488C130.23 378.572 107.182 346.505 79.595 316.875C73.6618 310.502 67.8625 302.466 61.6381 293.017C58.3612 285.098 54.1381 281.517 49.5108 270.535C44.067 257.615 31.0213 238.281 25.9771 225.166C14.31 194.832 6.57333 155.93 12.5119 123.268C18.8123 88.6156 30.5306 62.6248 55.6381 37.5173C79.595 17.0173 115.666 6.92319 144.859 10.8155C192.09 17.1131 238.76 55.2606 259.138 96.0173" []

        -- Vector 35
        rightStroke =
            strokePath "M17.6758 89.3346C27 77.4998 32.6799 69.8767 38.4194 60.9486C42.6095 54.4306 49.1792 48.8014 54.7959 43.4803C66.573 32.323 79 28.9998 96.2831 19.4614C106.5 13.8228 126.666 10.7272 137.285 10.7272C150.389 10.7272 165.151 8.49248 185 15.9998C209.625 25.3134 240.795 42.435 253.498 71.0171C264.306 95.3354 270 129.5 264.901 159.208C262.276 174.5 260.263 179.922 257.38 189.292C253.235 202.762 249.89 221.084 242.58 232.963C237.665 240.95 235.467 250.795 230.449 258.68C223.297 269.919 214.255 281.068 204.975 290.705C191.14 305.072 177.75 321.541 165.065 337.045C154.049 350.508 137.285 365 121.272 377.561C110.566 381.455 88.6641 401.327 71 413.5C53.3359 425.672 35 442 10.5 448" []

        -- Vector 37
        rightShadow x y =
            Svg.path
                [ fill "black"
                , css [ position x y ]
                , d "M299.138 73.0176C252.638 9.51762 192.638 -2.31572 176.138 1.51762C198.971 11.6843 250.488 52.2584 259.138 73.0176C276.638 115.018 286.638 111.518 248.138 226.518C217.338 318.518 69.3047 414.851 0.638062 450.018L62.6381 464.518C98.9714 446.518 185.338 391.918 240.138 317.518C308.638 224.518 353.243 146.902 299.138 73.0176Z"
                ]
                []

        topShadow x y =
            Svg.path
                [ fill "black"
                , css [ position x y ]
                , d "M95 11.9999C73 1.19991 18.8333 -1.00008 0 0.499923C15.3333 9.49992 59.5 25.9999 81 41.9999C98.2209 54.8155 122.667 79.9999 131.5 91.9999L152.5 62.4999C142.5 50.1666 117 22.7999 95 11.9999Z"
                ]
                []

        backgroundFill =
            fill "#FFB4A5"

        detailedBackground x y =
            -- Background
            Svg.path
                [ backgroundFill
                , css [ position x y ]
                , d "M721.5 267C721.5 193.809 648.333 174.333 635.5 176.5C644.833 154.5 664.5 96.5 600.5 54.5C536.5 12.5 459.167 51.1667 437 74C459.667 44.6667 444 0 382.5 0C259.202 0 0.000166476 272 0 367C-0.000101558 424.954 47.5001 457.333 73.5002 459.5C53.5002 483.667 28.8005 562.565 112.5 636C165.499 682.5 265.833 654.667 302.5 620C350 681 453.862 653.5 566 526.5C690.5 385.5 721.5 357 721.5 267Z"
                ]
                []

        details =
            [ detailedBackground 48 70
            , core 135.2 188.54
            , leftStroke 131.83 190.48
            , rightStroke 375.5 197.42
            , topShadow 250 183.08
            , rightShadow 361 193.35
            ]
    in
    { simple =
        init "0 0 800 800"
            [ -- Background
              Svg.path
                [ backgroundFill
                , css [ position 23 72 ]
                , d "M629.548 29.0171C537.548 -11.3829 427.048 24.3505 379.548 58.0172C343.049 29.0171 258.549 -18.483 161.548 7.5171C12.8225 47.3814 -45.9511 204.517 39.5483 379.517C121.715 547.695 333.096 654.534 372.048 656.017C411 657.5 564.049 573.517 693.549 404.017C806.539 256.127 744.548 79.5171 629.548 29.0171Z"
                ]
                []
            , core 129.83 178.06
            , leftStroke 126.46 179.99
            , rightStroke 370.14 186.93
            , topShadow 244.64 172.6
            , rightShadow 355.64 182.87
            ]
    , detailed =
        init "0 0 800 800"
            details
    , expressive =
        init "0 0 800 800"
            [ -- Background
              Svg.path
                [ backgroundFill
                , css [ position 48 70 ]
                , d "M721.5 267C721.5 193.809 648.333 174.333 635.5 176.5C644.833 154.5 664.5 96.5 600.5 54.5C536.5 12.5 459.167 51.1667 437 74C459.667 44.6667 444 0 382.5 0C259.202 0 0.000166476 272 0 367C-0.000101558 424.954 47.5001 457.333 73.5002 459.5C53.5002 483.667 28.8005 562.565 112.5 636C165.499 682.5 265.833 654.667 302.5 620C350 681 453.862 653.5 566 526.5C690.5 385.5 721.5 357 721.5 267Z"
                ]
                []
            , core 135.2 188.54
            , leftStroke 131.83 190.48
            , rightStroke 375.5 197.42
            , topShadow 250 183.08
            , rightShadow 361 193.35
            , strokePath
                "M10 116C10 104.266 18.9596 74.5 41.3217 49.8967C68.6964 19.7785 89.43 15.476 110 10"
                []
                31
                107
            , strokePath
                "M12.4999 169.5C6.82224 144.076 14.1064 90.8857 50.0001 56.4998C85.8938 22.1139 131.998 8.25731 150 10.5047"
                []
                71.77
                141.27
            ]
    , animated =
        init "0 0 800 800"
            [ let
                backgroundTranslate =
                    translate2 48 70
              in
              Svg.g
                [ css
                    [ Css.transform backgroundTranslate
                    , Css.property "transform-origin" "center"
                    , withAnimation
                        { delayMS = 1490
                        , durationMS = 350
                        , keyframes =
                            [ ( 0, [ Css.Animations.transform [ Css.scale 0, backgroundTranslate ] ] )
                            , ( 89, [ Css.Animations.transform [ Css.scale 1.1, backgroundTranslate ] ] )
                            , ( 100, [ Css.Animations.transform [ Css.scale 1, backgroundTranslate ] ] )
                            ]
                        }
                    ]
                ]
                [ detailedBackground 0 0
                ]
            , -- Heart animation
              let
                ( coreX, coreY ) =
                    ( 135.2, 188.54 )

                coreTranslate =
                    translate2 coreX coreY
              in
              Svg.g
                [ css
                    [ Css.transform coreTranslate
                    , Css.property "transform-origin" "center"
                    , withAnimation
                        { delayMS = 0
                        , durationMS = 1340
                        , keyframes =
                            [ ( 0, [ Css.Animations.transform [ Css.scale 0, coreTranslate ] ] )
                            , ( 32, [ Css.Animations.transform [ Css.scale 1.4, coreTranslate ] ] )
                            , ( 38, [ Css.Animations.transform [ Css.scale 0.9, coreTranslate ] ] )
                            , ( 44, [ Css.Animations.transform [ Css.scale 1, coreTranslate ] ] )
                            , ( 67, [ Css.Animations.transform [ Css.scale 1, coreTranslate ] ] )
                            , ( 73, [ Css.Animations.transform [ Css.scale 0.8, coreTranslate ] ] )
                            , ( 88, [ Css.Animations.transform [ Css.scale 1.4, coreTranslate ] ] )
                            , ( 94, [ Css.Animations.transform [ Css.scale 0.9, coreTranslate ] ] )
                            , ( 100, [ Css.Animations.transform [ Css.scale 1, coreTranslate ] ] )
                            ]
                        }
                    ]
                ]
                [ core 0 0
                , leftStroke (131.83 - coreX) (190.48 - coreY)
                , rightStroke (375.5 - coreX) (197.42 - coreY)
                , topShadow (250 - coreX) (183.08 - coreY)
                , rightShadow (361 - coreX) (193.35 - coreY)
                ]
            ]
    }


{-| -}
star : StickerIcon
star =
    let
        core x y =
            Svg.path
                [ fill "#1A80FE"
                , css [ position x y ]
                , d "M278.42 0.790039L199.92 182.29L8.41974 173.79L0.919739 205.29L140.42 352.29L87.4197 543.29L103.42 555.29L270.42 453.79L446.92 571.29L472.42 547.29L424.42 369.29L567.92 224.79L557.42 197.29H373.42L305.92 0.790039H278.42Z"
                ]
                []

        topStroke =
            strokePath "M10.6492 196.914C15.4776 194.5 18.3438 180.17 20.0611 174.803C23.2306 164.899 25.5192 153.9 30.8175 144.775C35.0986 137.402 36.4144 128.651 40.2293 121.021C43.5501 114.38 47.8607 105.809 49.6411 98.687C54.1536 80.6372 65.3763 59.8156 74.0671 43.2617C77.6244 36.4859 82.4463 29.9535 86.6162 23.467C92.4409 14.4064 99.9197 7.79065 110.92 11.7906C119.093 14.7627 119.823 17.6943 123.42 21.2906C127.618 25.4886 129.92 32.7906 133.003 39.6016C137.616 49.7906 140.326 57.3897 142.714 65.4468C144.45 71.3058 146.431 76.9294 148.465 82.6271C159.593 113.785 164.656 146.81 175.207 178.464C177.194 184.423 178.42 189.291 180.735 197.586C181.645 199.224 184.604 208.291 183.92 208.291" []

        leftStroke =
            strokePath "M154.177 182.949C140.987 171.062 128.011 160.159 114.513 148.738C104.986 140.677 94.9961 133.18 86.1276 124.312C73.36 111.544 58.6503 99.9176 47.6586 85.5438C42.8054 79.1974 39.4197 77.7909 31.8228 67.9153C24.2259 58.0397 19.7026 54.4115 15.9197 47.2909C13.197 42.1658 9.00582 30.9902 10.9199 24.2909C13.1623 16.4424 20.0857 10.8467 27.1169 10.8467C44.6878 10.8467 62.5589 10.8467 80.5253 10.8467C101.509 10.8467 121.599 14.8803 142.599 14.8803C160.419 14.8803 193.086 18.0939 208.419 18.7909" []

        bottomLeftStroke =
            strokePath "M196.42 115.79C186.429 123.913 178.362 127.679 171.605 132.647C163.743 138.426 155.725 144.148 148.523 150.724C142.316 156.391 134.325 160.698 127.683 165.962C120.47 171.677 111.477 176.25 103.78 181.499C98.537 185.073 93.1396 189.177 87.9197 192.79C79.272 198.777 71.9197 201.79 61.7999 207.643C53.818 209.756 46.1445 214.102 38.0462 216.532C34.0068 217.744 30.0435 217.559 25.4197 216.532C20.796 215.504 15.0273 211.192 13.5455 207.792C9.62315 198.794 10.1932 195.163 11.7528 186.429C13.5157 176.557 14.9234 165.862 18.102 156.326C23.8937 138.951 27.8271 120.978 34.3113 103.814C46.0121 72.841 56.7518 41.5718 67.1781 10.293" []

        bottomRightStroke =
            strokePath "M10.9432 91.8565C25.4784 106.392 46.0611 117.277 62.4095 128.832C100.044 155.43 132.729 185.375 174.92 204.126C178.256 205.609 187.555 205.397 191.113 204.126C200.305 200.843 207.038 193.136 209.638 183.062C211.968 174.032 207.403 164.459 205.828 155.797C200.977 129.117 188.683 102.196 180.356 76.29C176.425 64.0587 173.975 53.4566 169.92 41.29C167.981 35.4725 162.92 19.3629 162.92 10.79" []

        rightStroke =
            strokePath "M10.0955 10.3594C28.0567 10.3594 45.0448 10.3594 63.4197 10.3594C81.6521 10.3594 100.802 10.3594 119.004 10.3594C133.346 10.3594 147.688 10.3594 162.029 10.3594C170.165 10.3594 183.249 10.771 190.265 14.0942C193.434 15.5953 196.037 17.07 199.42 20.7908C202.802 24.5116 202.607 30.2143 202.366 34.5612C201.67 47.0969 186.703 60.9801 178.164 69.5195C163.455 84.229 144.961 97.6793 132.92 114.791C127.508 122.481 118.078 128.633 111.42 135.291C105.915 140.796 102.301 147.088 96.1466 151.836C82.8747 162.074 79.084 171.127 67.4197 182.791" []

        topShadow x y =
            Svg.path
                [ fill "black"
                , css [ position x y ]
                , d "M62 33.5C38.4 9.50001 11.1667 -0.833322 0 1.00001L32 52L78.5 209H129.5C116.833 160.5 85.6 57.5 62 33.5Z"
                ]
                []

        bottomLeftShadow x y =
            Svg.path
                [ fill "black"
                , css [ position x y ]
                , d "M50.9182 125.79C8.51823 123.39 -1.74693 99.29 0.419739 91.29L38.9177 98.79L190.918 0.790039L237.418 32.29C207.918 55.4567 140.478 112.651 111.918 121.79C99.4177 125.79 103.918 128.79 50.9182 125.79Z"
                ]
                []

        rightShadow x y =
            Svg.path
                [ fill "black"
                , css [ position x y ]
                , d "M186 73C210.5 37 148 -1.52588e-05 131.5 0L143 39L0 186C18.5 230.667 55.5 326.3 55.5 351.5C55.5 376.7 33.8333 389.5 26.5 390C37.5 391 70.8939 390.14 83.5 384.5C102.5 376 108 362.5 108 351.5C108 342.7 75.1667 241.5 59.5 190C105.167 150 176.284 87.2772 186 73Z"
                ]
                []

        backgroundFill =
            fill "#97E7FF"

        detailedBackground x y =
            -- Background
            Svg.path
                [ backgroundFill
                , css [ position x y ]
                , d "M222.001 9.99989C163.058 42.6335 158.501 92.4999 168.001 114.5C139.835 92.9999 63.8643 65.2815 19.0004 130.5C-35 209 45.8349 271.333 83.5015 317.5C70.0016 319.5 42.2017 338.4 39.0017 398C35.0017 472.5 363.002 673 437.002 681C496.202 687.4 515.335 666.333 517.502 655C553.668 671.167 626.002 703.5 684.502 643C731.302 594.6 665.002 507.5 626.002 470C678.335 486.833 778.502 492.7 760.502 381.5C738.002 242.5 338.501 -54.5001 222.001 9.99989Z"
                ]
                []

        details =
            [ detailedBackground 0.96 49.96
            , core 97 64.5
            , topStroke 294.73 59.31
            , leftStroke 94.52 237.56
            , bottomLeftStroke 181.65 411
            , bottomRightStroke 370.02 437.5
            , rightStroke 468.18 259.07
            , topShadow 390 49.79
            , bottomLeftShadow 174.23 515.5
            , rightShadow 515 252
            ]
    in
    { simple =
        init "0 0 800 800"
            [ -- Background
              Svg.path
                [ backgroundFill
                , css [ position 16 32.31 ]
                , d "M325.33 19.7903C282.13 48.9903 277.333 101.79 236.833 152.79C196.333 203.79 120.833 176.79 71.333 189.79C21.8333 202.79 -20.6996 261.225 10.8326 327.79C46.8339 403.79 112.83 426.29 125.83 473.29C138.83 520.29 88.8303 561.29 98.3303 662.29C102.403 705.588 148.833 736.29 195.833 736.29C242.833 736.29 341.833 681.79 385.833 681.79C429.833 681.79 503.248 736.29 563.5 736.29C639.503 736.29 677 690.541 677 642C677 581.5 659 546 659 499.5C659 453 713 417.5 744 384C775 350.5 775.5 288 744 251.5C712.5 215 625 223.5 578.5 202.5C532 181.5 517 84.9999 486 43.4999C455 1.99988 379.33 -16.7097 325.33 19.7903Z"
                ]
                []
            , core 103.92 122.79
            , topStroke 301.65 117.6
            , leftStroke 101.44 295.85
            , bottomLeftStroke 188.57 469.29
            , bottomRightStroke 376.94 495.79
            , rightStroke 475.1 317.36
            , topShadow 396.92 108.08
            , bottomLeftShadow 181.15 573.79
            , rightShadow 521.92 310.29
            ]
    , detailed =
        init "0 0 800 800"
            details
    , expressive =
        init "0 0 800 800"
            (details
                ++ [ strokePath
                        "M10 10C35.3173 12.9813 108.219 30.9002 142.562 80.6089C176.906 130.318 177 176.637 177 211"
                        []
                        621
                        437
                   , strokePath
                        "M10 10C40.9357 16.2984 109.403 57.2381 117.411 123.371C126.838 201.215 91.6577 241.624 81.6075 258"
                        []
                        620.04
                        483
                   ]
            )
    , animated =
        init "0 0 800 800"
            [ let
                backgroundTranslate =
                    translate2 0.96 49.96
              in
              Svg.g
                [ css
                    [ Css.transform backgroundTranslate
                    , Css.property "transform-origin" "center"
                    , withAnimation
                        { delayMS = 710
                        , durationMS = 250
                        , keyframes =
                            [ ( 0, [ Css.Animations.transform [ Css.scale 0, backgroundTranslate ] ] )
                            , ( 100, [ Css.Animations.transform [ Css.scale 1, backgroundTranslate ] ] )
                            ]
                        }
                    ]
                ]
                [ detailedBackground 0 0
                ]
            , let
                ( coreX, coreY ) =
                    ( 97, 64.5 )

                translateStar =
                    translate2 coreX coreY
              in
              Svg.g
                [ css
                    [ Css.transform translateStar
                    , Css.property "transform-origin" "center"
                    , withAnimation
                        { delayMS = 0
                        , durationMS = 790
                        , keyframes =
                            [ ( 0, [ Css.Animations.transform [ Css.scale 0, Css.rotate (Css.turn -3), translateStar ] ] )
                            , ( 80, [ Css.Animations.transform [ Css.scale 1.1, Css.rotate (Css.turn 0), translateStar ] ] )
                            , ( 90, [ Css.Animations.transform [ Css.scale 1.2, translateStar ] ] )
                            , ( 100, [ Css.Animations.transform [ Css.scale 1, translateStar ] ] )
                            ]
                        }
                    ]
                ]
                [ core 0 0
                , topStroke (294.73 - coreX) (59.31 - coreY)
                , leftStroke (94.52 - coreX) (237.56 - coreY)
                , bottomLeftStroke (181.65 - coreX) (411 - coreY)
                , bottomRightStroke (370.02 - coreX) (437.5 - coreY)
                , rightStroke (468.18 - coreX) (259.07 - coreY)
                , topShadow (390 - coreX) (49.79 - coreY)
                , bottomLeftShadow (174.23 - coreX) (515.5 - coreY)
                , rightShadow (515 - coreX) (252 - coreY)
                ]
            ]
    }
