module Nri.Ui.CharacterIcon.V2 exposing
    ( lindyInstructive, lindySupportive
    , redInstructive, redSupportive
    , salInstructive, salSupportive
    , lindyHeadshot, redHeadshot, salHeadshot
    )

{-| Patch changes:

  - added missing headshots

TODO: There was a bunch of work on <https://github.com/NoRedInk/noredink-ui/pull/1403>
towards reusing code in the SVGs to make them smaller. We should do the same for headshots.

@docs lindyInstructive, lindySupportive
@docs redInstructive, redSupportive
@docs salInstructive, salSupportive
@docs lindyHeadshot, redHeadshot, salHeadshot

-}

import Css
import Nri.Ui.Svg.V1
import Svg.Styled as Svg
import Svg.Styled.Attributes exposing (..)


{-| Use this to create a mask that **excludes** the stroke (this is almost always what we want)
-}
maskStyle : Svg.Attribute msg
maskStyle =
    css
        [ Css.fill (Css.hex "FFFFFF")
        , Css.property "stroke" "#000000"
        ]


renderFullLindy :
    { name : String
    , platformEllipse : Svg.Svg Never
    , tailPath : String
    , bodyPath : String
    , legPaths : List String
    , armPaths : List String
    , wristPaths : List String
    , hoofPaths : List String
    , shoePaths : List String
    , waistPath : String
    , undershirtPath : String
    , shirtPaths : List String
    , headPath : String
    , neckPoints : String
    , earPaths : List String
    , earLinePaths : List String
    , hairlinePath : String
    , eyeEllipses : List (Svg.Svg Never)
    , irisEllipses : List (Svg.Svg Never)
    , glassesPaths : List String
    , muzzlePath : String
    , muzzlePath2 : String
    , scarfPaths : List String
    , scarfLinePath : String
    , browPaths : List String
    , smilePaths : List String
    , armpitPaths : List String
    , pointerPath : String
    , pointerMagicPaths : List String
    , crotchLine1Path : String
    , crotchLine2Path : String
    }
    -> Nri.Ui.Svg.V1.Svg
renderFullLindy config =
    let
        stroke =
            { veryLight = Css.property "stroke-width" "1.46"
            , light = Css.property "stroke-width" "1.82"
            , medium = Css.property "stroke-width" "2.19"
            , heavy = Css.property "stroke-width" "2.55"
            , black = Css.property "stroke" "#000000"
            , dark = Css.property "stroke" "#231F20"
            }

        fill =
            { glasses = Css.fill (Css.hex "E70D4F")
            , body = Css.fill (Css.hex "9F9FAA")
            , head = Css.fill (Css.hex "9F9FA5")
            , pants = Css.fill (Css.hex "1E2840")
            , white = Css.fill (Css.hex "FFFFFF")
            , shirt = Css.fill (Css.hex "#95B7B7")
            , scarf = Css.fill (Css.hex "5578B5")
            , hoof = Css.fill (Css.hex "5B5B5B")
            , shoe = Css.fill (Css.hex "6E81B8")
            , pointer = Css.fill (Css.hex "#06CCBB")
            , brows = Css.fill (Css.hex "5B5B5B")
            , iris = Css.fill (Css.hex "030303")
            , line = Css.fill (Css.hex "231F20")
            }
    in
    Nri.Ui.Svg.V1.init "0 0 480 600"
        [ Svg.defs []
            [ Svg.path [ id (config.name ++ "_body"), css [ stroke.heavy ], d config.bodyPath ] []
            , Svg.g
                [ id (config.name ++ "_legs"), css [ stroke.heavy ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.legPaths)
            , Svg.g
                [ id (config.name ++ "_arms"), css [ stroke.heavy ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.armPaths)
            ]
        , Svg.g
            [ css
                [ Css.color (Css.hex "231F20")
                , Css.property "stroke-miterlimit" "10"
                ]
            ]
            [ Svg.g
                [ css [ fill.body, Css.opacity (Css.num 0.35), Css.property "enable-background" "new" ] ]
                [ config.platformEllipse
                ]
            , Svg.path [ d config.tailPath, css [ fill.body, stroke.dark, stroke.heavy ] ] []
            , Svg.use [ xlinkHref ("#" ++ config.name ++ "_body"), css [ fill.body, stroke.dark ] ] []
            , Svg.use [ xlinkHref ("#" ++ config.name ++ "_legs"), css [ fill.pants, stroke.dark ] ] []
            , Svg.mask [ id (config.name ++ "_legMask") ]
                [ Svg.use [ xlinkHref ("#" ++ config.name ++ "_legs"), maskStyle ] []
                ]
            , Svg.g
                [ css [ fill.shoe, stroke.light, stroke.dark, Css.property "mask" ("url(#" ++ config.name ++ "_legMask" ++ ")") ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.shoePaths)
            , Svg.use [ xlinkHref ("#" ++ config.name ++ "_arms"), css [ fill.shirt, stroke.dark ] ] []
            , Svg.mask [ id (config.name ++ "_bodyMask") ]
                [ Svg.use [ xlinkHref ("#" ++ config.name ++ "_body"), maskStyle ] []
                ]
            , Svg.g
                [ css [ Css.property "mask" ("url(#" ++ config.name ++ "_bodyMask" ++ ")") ] ]
                [ Svg.path [ d config.waistPath, css [ fill.pants ] ] []
                , Svg.path [ d config.undershirtPath, css [ fill.white, stroke.dark, stroke.light ] ] []
                , Svg.g
                    [ css [ fill.shirt, stroke.dark, stroke.medium ] ]
                    (List.map (\path -> Svg.path [ d path ] []) config.shirtPaths)
                ]
            , Svg.path [ d config.headPath, css [ fill.head, stroke.dark, stroke.heavy ] ] []
            , Svg.polygon [ points config.neckPoints, css [ fill.head ] ] []
            , Svg.g
                [ css [ fill.body, stroke.dark, stroke.heavy ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.earPaths)
            , Svg.g
                []
                (List.map (\path -> Svg.path [ d path ] []) config.earLinePaths)
            , Svg.path [ d config.hairlinePath, css [ fill.body, stroke.dark, stroke.heavy ] ] []
            , Svg.g
                [ css [ fill.white, stroke.dark, stroke.light ] ]
                config.eyeEllipses
            , Svg.g
                [ css [ fill.iris ] ]
                config.irisEllipses
            , Svg.g
                [ css [ fill.glasses, stroke.dark, stroke.veryLight ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.glassesPaths)
            , Svg.path [ d config.muzzlePath, css [ fill.body ] ] []
            , Svg.path [ d config.muzzlePath2, css [ fill.line ] ] []
            , Svg.g
                []
                (List.map (\path -> Svg.path [ d path ] []) config.armpitPaths)
            , Svg.mask [ id (config.name ++ "_armMask") ]
                [ Svg.use [ xlinkHref ("#" ++ config.name ++ "_arms"), maskStyle ] []
                ]
            , Svg.g
                [ css [ fill.body, stroke.black, stroke.light, Css.property "mask" ("url(#" ++ config.name ++ "_armMask" ++ ")") ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.wristPaths)
            , Svg.path [ d config.pointerPath, css [ fill.pointer, stroke.black, stroke.light ] ] []
            , Svg.g
                [ css [ fill.line ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.pointerMagicPaths)
            , Svg.g
                [ css [ fill.hoof, stroke.dark, stroke.heavy ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.hoofPaths)
            , Svg.g
                [ css [ fill.scarf, stroke.dark, stroke.heavy ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.scarfPaths)
            , Svg.path [ d config.scarfLinePath, css [ fill.line ] ] []
            , Svg.g
                [ css [ fill.brows, stroke.dark, stroke.heavy ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.browPaths)
            , Svg.g
                [ css [ fill.iris ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.smilePaths)
            , Svg.path [ d config.crotchLine1Path, css [ fill.pants ] ] []
            , Svg.path [ d config.crotchLine2Path, css [ fill.line ] ] []
            ]
        ]


{-| Full height Lindy to be shown when presenting a question.
-}
lindyInstructive : Nri.Ui.Svg.V1.Svg
lindyInstructive =
    renderFullLindy
        { name = "lindyInstructive"
        , platformEllipse = Svg.ellipse [ cx "229.6", cy "573.6", rx "90.1", ry "16.9" ] []
        , tailPath = "M277.3,463.7c0,0-1.9,36.9,18.5,34.9c15.7-1.5,14.1-13.7,14.1-13.7s18,11.3,27.8,2.5c16.9-15.2-6.5-30.9-6.5-30.9s9.8,2.2,9.6-8.6c-0.2-10.7-27.5-10.2-27.5-10.2L277.3,463.7z"
        , bodyPath = "M226.9,483.6c-10.7-0.3-86.7,2.4-98.3-47.2c-7.8-33,10.7-52.4,28.7-80.6c16.4-25.6,26.4-46.2,28.8-55.6c3.1-12.2,11.7-121.5,11.7-121.5s-1.1-24.9,26.2-26.2c26.2-1.3,32.1,18.5,33,31.1c1,12.6,2,98,13.7,123.2s15.9,32.6,30.5,53c14.6,20.4,27.3,31.6,26.7,59.8C326.7,482.3,242.4,484.1,226.9,483.6L226.9,483.6z"
        , legPaths =
            [ "M183.4,463.7c0,0,6.5,41.6,6.5,58.1c0,7.7,0.8,19.6-3.7,28.1c-5.1,9.7-15.6,15.8-16.7,20.5c-1.9,8.7,26.2,1,36.9-7.8c4.6-3.8,6.9-21.2,7.9-39.9c1.4-24.7,0.9-55.6,0.9-55.6L183.4,463.7L183.4,463.7z"
            , "M272.9,463.7c0,0-6.5,41.6-6.5,58.1c0,7.7-0.8,19.6,3.7,28.1c5.1,9.7,15.6,15.8,16.7,20.5c1.9,8.7-26.2,1-36.9-7.8c-4.6-3.8-6.9-21.2-7.9-39.9C240.5,498,241,467,241,467L272.9,463.7L272.9,463.7z"
            ]
        , armPaths =
            [ "M170.5,326.5c-16.2-0.4-40.1,16.6-48.1,12.1c-11-6.1-33-44.2-39.6-49.6c-7.8-6.4-5.8,16.2,0.3,36.2c6.2,20.4,15.6,34.4,24.7,38.9s34.2,8,47.2,0.4C179.4,350.2,186.7,326.9,170.5,326.5L170.5,326.5z"
            , "M291.5,325.6c15.3,5.3,23.6,15.9,32.6,14.5c12.4-1.9,46.3-29.9,54.5-32.7c9.5-3.3-0.2,17.2-12.9,33.8c-12.9,16.9-26.6,26.8-36.8,27.8c-10.4,1.1-33.1-2.1-42.5-13.7c-17.8-21.9-9.8-34.8,5.5-29.5L291.5,325.6z"
            ]
        , wristPaths =
            [ "M70.8,320c0,0,10,7.2,23.1,4.2c13.1-3,17.9-20.7,17.9-20.7L88.3,264l-48.3,13.9L70.8,320L70.8,320z"
            , "M379,340.6c0,0-11.8,3.2-23.1-4.1s-9.6-25.7-9.6-25.7l35.8-28.8l40.4,29.9L379,340.6z"
            ]
        , hoofPaths =
            [ "M91.8,283.6l13.2,9.1c0,0-14.7,8.2-25.6,4.4c-11.6-4-14.1-11.7-11.9-18.4s12.3-13.3,23-7.3c10.1,5.6,15.6,15.7,15.6,15.7L91.8,283.6L91.8,283.6z"
            , "M399.6,304.4c0,0,2.1-1.5,4.5-4.4c2.5-3,4.1-6.5,3.9-6.4c-3.4,0.8-12.4,5.3-21.6,8c-12.5,3.7-15.7,7.3-13.3,14.4c2.4,7.1,11.7,9.3,22,5.6c9.6-3.5,16.8-14.1,18.2-17.5c1.5-3.6-0.1-3.3-0.6-3.2L399.6,304.4z"
            ]
        , shoePaths =
            [ "M175.1,548.7c0,0,6.3,9.7,19.7,10.9c13.4,1.2,21.8,0.9,21.8,0.9s-5,26.1-22.1,27.7c-17.1,1.5-55.8,2.1-51.3-12.5C147.7,561,175.1,548.7,175.1,548.7z"
            , "M281.2,548.7c0,0-6.3,9.7-19.7,10.9s-21.8,0.9-21.8,0.9s5,26.1,22.1,27.7c17.1,1.5,55.8,2.1,51.3-12.5S281.2,548.7,281.2,548.7L281.2,548.7z"
            ]
        , pointerPath = "M58.7,192l26.5,103.3l8.1-0.5L61.4,190.7c0,0-0.2-0.2-0.5-0.4C58.6,189.6,58.7,192,58.7,192z"
        , waistPath = "M122.6,398.1c0,0,41.6,32.3,98.5,33.2c67,1.1,111.8-16.6,111.8-16.6s-4.1,55.4-40.1,62.5c-41.2,8-85.5,26.6-146.4-4.3C114.6,456.6,122.6,398.1,122.6,398.1L122.6,398.1z"
        , undershirtPath = "M161.3,328.3c0,0,40.8,25.5,66,24.4s77.3-28.8,77.3-28.8l42.6,90.8c0,0-65.8,20.5-124,20.5c-49.8,0-112.3-33.2-112.3-33.2s52.6-82.5,51.5-78"
        , shirtPaths =
            [ "M167,321.3c0,0,7.5,7.5,23.4,15.1c7.5,3.6,25,8.9,25,8.9l-28.6,98.5c0,0-28.3-0.7-45.6-8.9c-15.1-7.1-26.6-17.1-26.6-17.1L167,321.3z"
            , "M264.7,443.6l-23.8-98.8c0,0,8,0.8,28.9-11.5s26.8-32.1,26.8-32.1S340,393.8,340,403.8s-13.8,24.9-33.8,32.6C286.3,444.1,264.7,443.6,264.7,443.6L264.7,443.6z"
            ]
        , headPath = "M248.1,184.1h-45c-10.8,0-19.5-8.7-19.5-19.5l-2.2-41.8c0-10.8,8.7-19.5,19.5-19.5H251c10.8,0,19.5,8.7,19.5,19.5l-2.9,41.8C267.6,175.3,258.9,184.1,248.1,184.1z"
        , neckPoints = "198.2,177.6 198.2,210.7 252,213.9 249.5,170 "
        , earPaths =
            [ "M238,96.7c0,0-1.6-25-9.2-44S205.4,18.5,195,25.4c-13.6,9,16.2,29.5,20.4,40.9s5.7,30.4,5.7,30.4L238,96.7L238,96.7z"
            , "M256.8,100.8c0,0,2-25,9.6-44s23.8-34.8,33.8-27.3c14,10.5-16.2,29.5-20.4,40.9s-6.1,30.4-6.1,30.4S256.8,100.8,256.8,100.8z"
            ]
        , earLinePaths =
            [ "M206.5,37c8.6,7.5,14.9,17.6,18.6,28.5c3.5,10.9,5,22.3,5.9,33.7c-2.3-11.1-4.1-22.3-7.6-33.1C219.8,55.4,214,45.4,206.5,37L206.5,37L206.5,37z"
            , "M288.7,41.1c-7.5,8.4-13.3,18.4-16.9,29.1c-3.5,10.8-5.3,22-7.6,33.1c0.9-11.4,2.4-22.8,5.9-33.7C273.8,58.7,280.1,48.6,288.7,41.1L288.7,41.1L288.7,41.1z"
            ]
        , hairlinePath = "M183.7,115.5c0,0-20.3,19.5-27.5,7.1c-9.2-15.9,10.8-20.3,10.8-20.3s-25.9-9.1-18-23.1c19.1-33.8,48.8,1.8,48.8,1.8s-11.7-32.3,17.6-35.6c37.9-4.3,41.9,44.9,41.9,44.9s25.9-11.9,30.7,3.9c6.7,21.8-21.4,23.5-21.4,23.5"
        , eyeEllipses =
            [ Svg.ellipse [ transform "matrix(0.9931 -0.1175 0.1175 0.9931 -14.1144 24.4552)", cx "200.3", cy "131.9", rx "8.6", ry "9.3" ] []
            , Svg.ellipse [ transform "matrix(0.1175 -0.9931 0.9931 0.1175 76.9799 349.0489)", cx "234.9", cy "131.2", rx "9.9", ry "9.2" ] []
            ]
        , irisEllipses =
            [ Svg.ellipse [ transform "matrix(0.9931 -0.1175 0.1175 0.9931 -14.1359 24.7126)", cx "202.5", cy "132.2", rx "4.2", ry "4.8" ] []
            , Svg.ellipse [ transform "matrix(0.1175 -0.9931 0.9931 0.1175 74.8672 346.4753)", cx "232.4", cy "131.1", rx "5.2", ry "4.5" ] []
            ]
        , glassesPaths =
            [ "M277.1,130c3.7,0.7,6.4,3.3,5.3,7.6c-0.5,2.1-4.4,2.5-4.4-1.8c0-1.6-2.6-1.3-2.6-1.3l-21.8-0.4l0.2-4.9C253.8,129.3,273.8,129.3,277.1,130L277.1,130z"
            , "M255.3,133.8c-0.1,0.7,0.1-2.5,0.2-4.4c0-0.4,0-0.9,0.1-1.4c0.2-4.4,0.5-9.4-3-12.7c-0.8-0.7-2.8-1.5-4.3-2l-0.3-0.1l-25.6-0.2l-0.3,0.1c-1.4,0.6-3.3,1.4-4,2.3c-2.4,2.9-2.4,8.6-2.2,13.5l-4.1,0c0-0.2,0-0.4,0-0.6c0.2-4.6,0.4-9.4-2.7-12.6c-0.7-0.8-2.6-1.5-4-2l-0.3-0.1l-23.4-0.2l-0.4,0.2c-1.1,0.5-3,1.4-3.7,2.3c-2.3,3.1-2.1,9.4-2,14.5c0,1,0.1,1.9,0.1,2.7c0,11.4,6.9,18.5,17.9,18.5s18-7.8,18.4-19l4.2,0c0,0.1,0,0.1,0,0.2c0,11.4,7.5,18.4,19.5,18.4c11.2,0,19.2-7.4,20-18.2 M208.1,128.3c-0.1,1.1-0.1,2.3-0.1,3.4c0,4.7-1.4,8.8-4.1,11.8c-2.7,3-6.4,4.6-10.7,4.6c-9.1,0-14.3-5.4-14.3-14.8c0-0.8,0-1.8-0.1-2.8c-0.1-3.8-0.3-10.1,1.2-12.2c0.2-0.2,1.1-0.7,1.9-1.1l22.1,0.2c1,0.4,2.1,0.8,2.4,1.1C208.4,120.6,208.3,124.3,208.1,128.3L208.1,128.3z M247.3,143.1c-3,3-7.1,4.6-11.9,4.6c-10.1,0-15.9-5.4-15.9-14.8c0-0.8,0-1.8-0.1-2.8c-0.1-3.8-0.3-10,1.4-12.1c0.3-0.2,1.3-0.7,2.2-1.1l24.4,0.2c1.2,0.4,2.4,0.9,2.7,1.1c2.2,2.1,2,5.9,1.8,9.8c-0.1,1.1-0.1,2.3-0.1,3.4v0.1C251.8,136,250.2,140.1,247.3,143.1L247.3,143.1z"
            ]
        , muzzlePath = "M215.4,145.4c0,0-42.8-4.2-46,8.4c-2.1,8.6,0.4,19.3,1.1,25.4c0.3,2.7,2.7,4.8,5.6,4.8h26.9"
        , muzzlePath2 = "M215.4,144.5c-4.8-0.5-9.7-0.7-14.5-0.7c-8,0-16.7,0.1-24.4,2.9c-2.8,1-5.7,2.5-7.2,5.1c-1.2,2-1.4,4.6-1.5,6.8c-0.3,3.6,0,7.1,0.4,10.7c0.4,3,0.9,6.1,1.3,9.1c0.4,2.8,1.5,5.1,4.3,6.2c2.2,0.8,5.1,0.4,7.4,0.4c7.1,0,14.2,0.1,21.4,0c0.2,0,0.3,0,0.5,0c1.2,0,1.2-1.8,0-1.8H185c-2.8,0-5.6,0-8.3,0c-1.7,0-3.3-0.5-4.3-1.9c-0.8-1.1-0.9-2.4-1.1-3.7c-0.2-1.4-0.4-2.9-0.7-4.4c-0.5-3.3-0.9-6.7-1.1-10.1c-0.1-3.1-0.1-6.5,0.9-9.5c0.9-2.5,3.4-3.9,5.7-4.9c7.1-2.9,15.4-3.1,22.9-3.1c5.5,0,11,0.1,16.4,0.7C216.6,146.5,216.6,144.7,215.4,144.5L215.4,144.5L215.4,144.5z"
        , scarfPaths =
            [ "M124.6,297.9c0,0,13.3-17.1,29.6-20.3c16.3-3.2,29.6,2,29.6,2s-9.9,17.1-29.6,21C143.4,302.9,124.5,297.9,124.6,297.9L124.6,297.9z"
            , "M183.1,280.9c0,0-17.1-6-28.6,9.9C143,306.7,143,325.4,143,325.4s20.4-3.6,29-11.9C190.6,295.2,183.1,280.9,183.1,280.9z"
            , "M183.7,279.6c0,0,11.7,7.3,44.3,5.7c23.9-1.2,39.3-12.1,39.3-12.1s6,3.6,6.8,13.1s-1.7,13.9-1.7,13.9s-28.4,10.6-51.7,5.9c-31-6.3-36.9-13.4-36.9-13.4s-3.6-4.4-2.8-7.9S183.7,279.6,183.7,279.6z"
            ]
        , scarfLinePath = "M182.6,286.3c20.4,8.4,42.9,8.7,64.6,7.1C226.3,298.7,201.4,298,182.6,286.3L182.6,286.3z"
        , browPaths =
            [ "M185.4,107.3c0,0,0.5-7.1,6.7-11.1s13.2-1,13.2,3.1s-6.6,3-10,4S185.4,107.3,185.4,107.3L185.4,107.3z"
            , "M244.9,107.3c0,0-0.5-7.1-6.7-11.1s-13.2-1-13.2,3.1s6.6,3,10,4S244.9,107.3,244.9,107.3L244.9,107.3z"
            ]
        , smilePaths =
            [ "M172.2,167.6c0.4,0.7,1.3,1.1,2,1.4c2.4,0.8,4.9,0.9,7.5,1c6.7,0.1,13.4-1.3,19.2-4.7c2.9-1.7,5.6-3.7,8-6.3c-4.9,9.8-16.9,14.3-27.3,12.9C178.5,171.5,173.5,170.8,172.2,167.6L172.2,167.6L172.2,167.6z"
            , "M179,162.8c0,2.1,0,4.3,0,6.4v0.9c0,1.2,1.8,1.2,1.8,0v-7.4C180.9,161.7,179,161.7,179,162.8L179,162.8z"
            , "M171.8,155.1c1.9,3.4,4.7,6.4,8.5,7c3.8,0.2,7.3-1.8,10.8-3.3c2.4-1.2,4.7-2.4,7.1-3.7c-1.9,1.9-4.1,3.5-6.4,4.9c-3.5,2.1-7.4,4.3-11.8,3.8C175.8,163,172.6,159,171.8,155.1L171.8,155.1L171.8,155.1z"
            ]
        , armpitPaths =
            [ "M127.6,411.6c3.3-25,17.2-43.9,36.2-59.7C145.7,371.9,135.1,385.1,127.6,411.6L127.6,411.6z"
            , "M327.9,406.1c-7.6-21.3-21.4-39.5-37.2-55.5C308.6,364.3,323.2,383.8,327.9,406.1L327.9,406.1z"
            ]
        , pointerMagicPaths =
            [ "M56.5,176.6c-1.9-5.6-3.8-11.2-5.7-16.7c-0.3-0.8-0.5-1.6-0.8-2.4c-0.6-1.7-3.2-0.9-2.6,0.7c1.9,5.6,3.8,11.2,5.7,16.7c0.3,0.8,0.5,1.6,0.8,2.4C54.4,179,57,178.3,56.5,176.6L56.5,176.6z"
            , "M49.4,181c-3.2-2.2-6.3-4.3-9.5-6.5c-0.5-0.3-0.9-0.6-1.3-0.9c-0.6-0.4-1.5-0.1-1.9,0.5c-0.4,0.7-0.1,1.4,0.5,1.9c3.2,2.2,6.3,4.3,9.5,6.5c0.5,0.3,0.9,0.6,1.3,0.9c0.6,0.4,1.5,0.1,1.9-0.5C50.3,182.2,50,181.4,49.4,181L49.4,181z"
            , "M65.3,177.3c1-2.8,2-5.6,3-8.3c0.2-0.4,0.3-0.8,0.4-1.2c0.2-0.7-0.3-1.5-1-1.7c-0.8-0.2-1.4,0.3-1.7,1c-1,2.8-2,5.6-3,8.3c-0.2,0.4-0.3,0.8-0.4,1.2c-0.2,0.7,0.3,1.5,1,1.7C64.3,178.5,65,178,65.3,177.3L65.3,177.3z"
            ]
        , crotchLine1Path = "M208.8,482.5c0,0,9.3,1,19.8,1.4s20.4-1.4,20.4-1.4"
        , crotchLine2Path = "M208.8,483.4c11.8,1.2,23.7,2.1,35.5,0.7c1.7-0.2,3.3-0.4,5-0.7c1.1-0.2,0.7-2-0.5-1.8c-13.1,2.4-26.8,1.3-39.9,0C207.7,481.5,207.7,483.3,208.8,483.4L208.8,483.4L208.8,483.4z"
        }


{-| Full height Lindy to be shown after getting an answer wrong.
-}
lindySupportive : Nri.Ui.Svg.V1.Svg
lindySupportive =
    renderFullLindy
        { name = "lindySupportive"
        , platformEllipse = Svg.ellipse [ cx "229.4", cy "573.5", rx "90.1", ry "17" ] []
        , tailPath = "M193.6,485.3c0,0,1.7,37-16.7,34.9c-14.1-1.5-12.7-13.7-12.7-13.7s-16.3,11.4-25.1,2.5c-15.2-15.2,5.9-31,5.9-31s-8.8,2.2-8.6-8.6c0.2-10.7,24.8-10.2,24.8-10.2L193.6,485.3L193.6,485.3z"
        , bodyPath = "M226.8,483.5c-10.7-0.3-86.7,2.4-98.4-47.2c-7.8-33,8.6-53.9,28.7-80.6c18.7-24.9,26-43.6,28.4-53c3.1-12.3,12.1-124.2,12.1-124.2s-1.1-24.9,26.2-26.2c26.2-1.3,25.1,17,26.1,29.6s9,99.5,20.7,124.8c11.7,25.3,15.9,32.6,30.5,53s27.3,31.6,26.7,59.9C326.6,482.1,242.3,484,226.8,483.5L226.8,483.5z"
        , legPaths =
            [ "M272.8,463.5c0,0-6.5,41.6-6.5,58.1c0,7.7-0.8,19.6,3.7,28.1c5.1,9.7,15.6,15.8,16.7,20.5c1.9,8.8-26.2,1-36.9-7.8c-4.6-3.8-6.9-21.2-7.9-39.9c-1.4-24.7-0.9-55.6-0.9-55.6L272.8,463.5L272.8,463.5z"
            , "M183.3,463.5c0,0,6.5,41.6,6.5,58.1c0,7.7,0.8,19.6-3.7,28.1c-5.1,9.7-15.6,15.8-16.7,20.5c-1.9,8.8,26.2,1,36.9-7.8c4.6-3.8,6.9-21.2,7.9-39.9c1.4-24.7,0.9-55.6,0.9-55.6L183.3,463.5L183.3,463.5z"
            ]
        , armPaths =
            [ "M289.9,354.3c15,1.5,26.8-6.8,40.6-1.4c13.9,5.5-14,39.9-8.5,45.7c6.4,6.8,15.3-1.5,25.7-16c10.3-14.4,14.2-39.7,7.4-50c-5.3-8.1-33.2-20.7-65.8-11.6C263.7,328.1,275,352.8,289.9,354.3L289.9,354.3z"
            , "M170.3,328.7c-16.5,4.8-47.2-7.3-56.8-21.9C97.9,283.3,102,238.3,94,235.2c-11.3-4.3-12.4,17-14.1,37.8c-2.4,29.4,4.8,49.5,12.3,58.1c24,27.5,60.9,41.1,75.8,39.5C196,367.6,180.7,325.7,170.3,328.7L170.3,328.7z"
            ]
        , wristPaths =
            [ "M362.4,385.4c0,0,0.1-12.3-9.9-21.3c-10-9-27.3-2.6-27.3-2.6l-18.6,42l39.2,31.4L362.4,385.4L362.4,385.4z"
            , "M71.9,290.6c0,0,9.7,7.5,22.9,4.9c13.2-2.6,18.5-20.2,18.5-20.2l-22.3-40.2l-48.7,12.4L71.9,290.6L71.9,290.6z"
            ]
        , hoofPaths =
            [ "M353.4,410.4c3.8,0.4,7.7,0,7.5-0.1c-2.4-1.9-11-6-18.1-10.8c-9.7-6.6-14.4-7.3-19.2-2.9c-4.8,4.4-1.8,11.4,6.8,16.5c8,4.8,20.8,5.2,24.4,4.8c3.9-0.5,2.7-1.4,2.4-1.7l-9.9-7C347.3,409.2,349.6,410,353.4,410.4z"
            , "M107.5,240.9l12.6,9.9c0,0-15.2,7.3-25.9,2.9c-11.3-4.7-13.3-12.5-10.8-19.1s13.1-12.5,23.4-5.9c9.8,6.2,14.7,16.6,14.7,16.6L107.5,240.9z"
            ]
        , shoePaths =
            [ "M281.1,548.6c0,0-6.3,9.7-19.7,10.9c-13.4,1.2-21.8,0.9-21.8,0.9s5,26.2,22.1,27.7s55.9,2.1,51.4-12.5S281.1,548.6,281.1,548.6z"
            , "M175,548.6c0,0,6.3,9.7,19.7,10.9c13.4,1.2,21.8,0.9,21.8,0.9s-5,26.2-22.1,27.7s-55.9,2.1-51.3-12.5C147.6,561,175,548.6,175,548.6L175,548.6z"
            ]
        , pointerPath = "M160.5,127.6L99,238.1l5.9,5.5l58.4-115c0,0,0-0.2-0.1-0.6C162.9,126.7,161.2,126.5,160.5,127.6z"
        , waistPath = "M122.4,397.9c0,0,41.6,32.3,98.5,33.2c67,1.1,111.8-16.6,111.8-16.6s-4.1,55.4-40.1,62.5c-41.2,8-85.5,26.7-146.4-4.3C114.4,456.5,122.4,397.9,122.4,397.9L122.4,397.9z"
        , undershirtPath = "M164,328.2c0,0,39,31.2,78.8,25.3c40.8-6,64.5-29.8,64.5-29.8l37.1,65.3c0,0-46.2,47-104.3,48.3c-59.7,1.3-125.2-25.9-125.2-25.9s51.2-92.1,50.1-87.7"
        , shirtPaths =
            [ "M286.8,437.8l-32.3-97.1c0,0,15.3-0.9,26.2-15.9c14-19.3,25.2-33.7,25.2-33.7s51,88.6,51.8,98.5c0.8,9.9-14.1,22.1-30.9,35.4C306.6,441,286.8,437.8,286.8,437.8L286.8,437.8z"
            , "M171.7,326.4c0,0,6.3,6.1,27.2,12.4c9.1,2.7,24.4,5.3,24.4,5.3l-16.8,101.3c0,0-41.8-3.4-62-11.5c-17.6-7.1-31-17.1-31-17.1L171.7,326.4L171.7,326.4z"
            ]
        , headPath = "M207.7,188.1l41.1-18.4c9.8-4.4,14.3-16,9.9-25.8l-15.1-39c-4.4-9.8-16-14.3-25.8-9.9l-45.8,20.5c-9.9,4.4-14.3,16-9.9,25.8l19.7,36.9C186.3,188.1,197.9,192.5,207.7,188.1L207.7,188.1z"
        , neckPoints = "248.2,165.1 247.1,200.1 212.9,215.2 200.1,173.1 "
        , earPaths =
            [ "M199.6,93.9c0,0-5.8-24.4-16.5-41.8c-10.7-17.4-28.9-29.8-38-21.2c-11.9,11.2,21,26.3,27.1,36.8s10.8,29,10.8,29L199.6,93.9L199.6,93.9z"
            , "M171.9,105.3c0,0-19.2-16.2-38-24.2s-41.5-7.7-43.2,4.7c-2.4,17.4,32.4,9.3,43.4,14.3s25.9,17.2,25.9,17.2L171.9,105.3L171.9,105.3z"
            ]
        , earLinePaths =
            [ "M158.4,40.4c9.8,6,17.7,14.9,23.2,24.9c5.3,10.2,8.7,21.2,11.5,32.2c-4.1-10.6-7.8-21.3-13.1-31.4C174.6,56.2,167.2,47.4,158.4,40.4C158.4,40.4,158.4,40.4,158.4,40.4z"
            , "M107,85.8c11.4-0.8,23.1,1.8,33.4,6.8c10.2,5.3,19.3,12.2,28.1,19.6c-9.5-6.2-18.8-12.8-28.9-18C129.4,89.3,118.3,86.5,107,85.8L107,85.8L107,85.8z"
            ]
        , hairlinePath = "M238.6,99.2c0,0,26.5,9.6,28-4.8c1.9-18.3-18.1-14.1-18.1-14.1s12.9-19.8,3.6-28.1c-16.6-14.8-41.1,16.8-41.1,16.8s-3.4-25.9-28.7-18.1c-36.5,11.1-21.2,55.4-21.2,55.4s-25.7-1.2-25.6,15.5c0.1,22.8,27.1,15.3,27.1,15.3"
        , eyeEllipses =
            [ Svg.ellipse [ transform "matrix(0.9545 -0.2982 0.2982 0.9545 -25.5814 74.126)", cx "230.1", cy "120.9", rx "8.6", ry "9.3" ] []
            , Svg.ellipse [ transform "matrix(0.8585 -0.5128 0.5128 0.8585 -40.8685 120.6642)", cx "198.2", cy "134.4", rx "9.2", ry "9.9" ] []
            ]
        , irisEllipses =
            [ Svg.ellipse [ transform "matrix(0.9389 -0.3443 0.3443 0.9389 -34.1473 77.1381)", class "st22", cx "200.1", cy "134.7", rx "4.5", ry "5.2" ] []
            , Svg.ellipse [ transform "matrix(0.9389 -0.3443 0.3443 0.9389 -28.0369 86.2608)", class "st22", cx "228.9", cy "122.1", rx "4.2", ry "4.9" ] []
            ]
        , glassesPaths =
            [ "M161.3,149.4c-2.7,2-3.9,5.3-1.2,8.9c1.3,1.8,4.6,0.6,2.9-3.2c-0.7-1.5,1.6-2.2,1.6-2.2l17.6-8.3l-2.2-4.4C180.1,140.3,163.8,147.6,161.3,149.4L161.3,149.4z"
            , "M180.4,145.4c5.1,9.6,15.5,13.1,25.7,8.5c11-4.9,14.9-14.4,10.3-24.8c0-0.1-0.1-0.1-0.1-0.2l3.9-1.7c5,10.1,15,14.2,24.6,9.9s13.5-13.8,8.8-24.2c-0.3-0.7-0.7-1.6-1.1-2.5c-1.9-4.7-4.4-10.6-7.8-12.5c-1.1-0.6-3.2-0.6-4.4-0.6h-0.4l-21.3,9.8l-0.2,0.2c-1.1,1-2.5,2.5-2.8,3.5c-1.5,4.3,0.6,8.5,2.7,12.6c0.1,0.2,0.2,0.3,0.3,0.5l-3.7,1.6c-1.9-4.5-4.2-9.7-7.6-11.4c-1.1-0.5-3.1-0.5-4.6-0.5l-0.3,0l-23.4,10.7l-0.2,0.2c-1.2,1.1-2.7,2.6-3.1,3.6c-1.8,4.4,0.4,8.9,2.5,12.8c0.2,0.4,0.4,0.8,0.6,1.2c0.8,1.7,2.3,4.5,1.9,3.9 M219.1,112c0.2-0.3,1-1.2,1.7-1.9l20.1-9.2c0.9,0,1.9,0.1,2.2,0.2c2.3,1.3,4.7,7.2,6.1,10.6c0.4,1,0.8,1.9,1.1,2.6c3.9,8.6,1.3,15.6-7,19.4c-4,1.8-8,1.8-11.6,0.2c-3.6-1.6-6.6-4.8-8.5-9.1c-0.5-1-1-2.1-1.5-3.1C219.8,118.1,218.1,114.8,219.1,112L219.1,112z M182.9,142.3L182.9,142.3c-0.5-1.1-1-2.1-1.5-3.1c-1.8-3.5-3.5-6.9-2.4-9.7c0.2-0.3,1.1-1.3,2-2.1l22.2-10.1c1,0,2.1,0,2.5,0.1c2.4,1.2,4.8,7.1,6.2,10.5c0.4,1,0.8,1.9,1.1,2.6c3.9,8.6,0.8,15.9-8.4,20c-4.4,2-8.9,2.2-12.8,0.7C188,149.7,184.8,146.6,182.9,142.3L182.9,142.3z"
            ]
        , muzzlePath = "M221.8,139.5c0,0,37.4-21.4,45.4-11.1c5.4,6.9,7.5,17.8,9.3,23.7c0.8,2.6-0.5,5.5-3.1,6.6l-29.2,14.2"
        , muzzlePath2 = "M222.3,140.2c4.5-2.6,9.2-4.9,14-7c4.1-1.8,8.3-3.5,12.6-4.8c3.7-1.1,7.7-2.1,11.6-1.9c1.4,0.1,2.9,0.3,4.2,0.9c1.8,0.9,2.9,2.8,3.9,4.5c3.2,5.5,4.8,11.8,6.4,17.9c0.3,1.2,0.9,2.5,0.9,3.8c0,1.6-0.9,3.2-2.3,4c-0.6,0.4-1.2,0.6-1.8,0.9l-8.7,4.2c-6.3,3.1-12.7,5.9-18.9,9.2c-0.1,0.1-0.2,0.1-0.4,0.2c-1.1,0.5-0.1,2.1,0.9,1.6c5.8-2.8,11.6-5.6,17.4-8.4c3.3-1.6,6.6-3.2,10-4.8c0.6-0.3,1.2-0.6,1.9-0.9c2.4-1.2,4.1-3.7,3.8-6.4c-0.1-1.3-0.6-2.5-1-3.8c-0.4-1.4-0.8-2.8-1.2-4.2c-0.9-3.2-1.9-6.5-3.2-9.6c-1.7-4-3.9-9-8.5-10.3c-3.6-1-7.5-0.4-11.1,0.4c-4.5,1-8.9,2.6-13.2,4.3c-6.1,2.5-12.3,5.3-17.9,8.7c-0.1,0.1-0.2,0.1-0.3,0.2C220.4,139.3,221.3,140.9,222.3,140.2L222.3,140.2L222.3,140.2z"
        , scarfPaths =
            [ "M125.3,269.1c0,0,20.5-7.2,35.9-1.1s23.8,17.7,23.8,17.7s-17.6,9-36.3,1.7C138.5,283.5,125.3,269.1,125.3,269.1z"
            , "M184.9,287.2c0,0-11.1-14.3-29.4-7.3S127,302.6,127,302.6s19.1,8.2,30.8,5.8C183.4,303.3,184.9,287.2,184.9,287.2z"
            , "M227.9,285.1c24-1.2,39.3-12.1,39.3-12.1s6,3.6,6.8,13.1c0.8,9.5-1.7,13.9-1.7,13.9s-28.4,10.6-51.7,5.9c-31-6.3-36.9-13.4-36.9-13.4s-3.6-4.4-2.8-7.9s2.8-5.2,2.8-5.2S195.3,286.7,227.9,285.1z"
            ]
        , scarfLinePath = "M182.5,286.1c20.5,8.4,42.9,8.7,64.7,7.1C226.2,298.5,201.3,297.9,182.5,286.1L182.5,286.1L182.5,286.1z"
        , browPaths =
            [ "M231.6,90.4c0,0-2.1-6.3-8.5-8.6s-11.9,2.1-11,5.7s6.6,1.3,9.7,1.4C225.1,89.1,231.6,90.4,231.6,90.4L231.6,90.4z"
            , "M174.7,98.1c3-6.8,10.5-8.1,12.7-4.7s-3.9,6.2-6.1,8.8c-2.3,2.7-6.1,8.9-6.1,8.9S171.7,104.9,174.7,98.1z"
            ]
        , smilePaths =
            [ "M267.7,145.7c-1.9,6.6-11.8,9.9-18.1,9.7c-2.4-0.1-4.9-0.5-6.9-2C249.6,154.5,263.2,151.1,267.7,145.7L267.7,145.7L267.7,145.7z"
            , "M258.7,142.8c0.9,2,1.8,3.9,2.6,5.9c0.1,0.3,0.2,0.6,0.4,0.8c0.2,0.4,0.9,0.6,1.2,0.3c0.5-0.3,0.5-0.8,0.3-1.2c-0.9-2-1.8-3.9-2.6-5.9c-0.1-0.3-0.2-0.6-0.4-0.8c-0.2-0.4-0.9-0.6-1.2-0.3C258.6,141.8,258.5,142.3,258.7,142.8L258.7,142.8z"
            , "M264.9,130.4c0.9,4-0.5,8.9-4,11.3c-3.7,2.2-8.2,1.8-12.3,1.3c-2.7-0.4-5.3-1-7.9-1.9c2.7,0.2,5.3,0.4,8,0.5c3.8,0,7.8,0.4,11.2-1.4C263.2,138.2,264.6,134.3,264.9,130.4L264.9,130.4L264.9,130.4z"
            ]
        , armpitPaths =
            [ "M140.9,380.6c1.4-7.9,6.5-15.1,13.3-19.3C149.5,367.7,145.1,373.9,140.9,380.6L140.9,380.6z"
            , "M327.8,405.9c-8.5-23.4-22.8-44.2-40.4-61.8C307.1,359.4,322.2,381.5,327.8,405.9L327.8,405.9z"
            ]
        , pointerMagicPaths =
            [ "M133.5,145.6c-5.8,0.4-11.7,0.9-17.5,1.3l-2.5,0.2c-1.8,0.1-1.8,2.9,0,2.7c5.8-0.4,11.7-0.9,17.5-1.3l2.5-0.2C135.2,148.2,135.3,145.5,133.5,145.6L133.5,145.6z"
            , "M136.1,139.5c-2.5-1.2-5-2.4-7.5-3.5c-0.4-0.2-0.7-0.3-1.1-0.5c-0.7-0.3-1.5-0.2-1.9,0.5c-0.3,0.6-0.2,1.6,0.5,1.9c2.5,1.2,5,2.4,7.5,3.5c0.4,0.2,0.7,0.3,1.1,0.5c0.6,0.3,1.5,0.2,1.9-0.5C136.9,140.8,136.8,139.8,136.1,139.5L136.1,139.5L136.1,139.5z"
            ]
        , crotchLine1Path = "M208.7,482.4c0,0,9.3,1,19.9,1.4c10.6,0.4,20.4-1.4,20.4-1.4"
        , crotchLine2Path = "M208.7,483.3c11.8,1.2,23.7,2.1,35.5,0.7c1.7-0.2,3.3-0.4,5-0.7c1.1-0.2,0.7-2-0.5-1.8c-13.1,2.4-26.8,1.3-40,0C207.5,481.4,207.5,483.2,208.7,483.3L208.7,483.3L208.7,483.3z"
        }


redPalette =
    { stroke =
        { black = Css.property "stroke" "#000000"
        , width = \x -> Css.property "stroke-width" (String.fromFloat x)
        }
    , fill =
        { platform = Css.fill (Css.hex "9f9faa")
        , body = Css.fill (Css.hex "DF633F")
        , bodySecondary = Css.fill (Css.hex "822D19")
        , bodyTertiary = Css.fill (Css.hex "FFE4D1")
        , shirt = Css.fill (Css.hex "F0EFED")
        , pants = Css.fill (Css.hex "895C48")
        , shoes = Css.fill (Css.hex "11222B")
        , white = Css.fill (Css.hex "FFFFFF")
        , iris = Css.fill (Css.hex "0E0A0B")
        , dark = Css.fill (Css.hex "231F20")
        , pointer = Css.fill (Css.hex "06CCBB")
        , none = Css.property "fill" "none"
        , button = Css.fill (Css.hex "F2336C")
        , mouth = Css.fill (Css.hex "8c1111")
        }
    }


renderFullRed :
    { name : String
    , platformEllipse : Svg.Svg Never
    , bodyPaths : List String
    , headPath : String
    , torsoPath : String
    , tailStripePaths : List String
    , earPaths : List String
    , earFoldPaths : List String
    , earLinePaths : List String
    , waistPath : String
    , hipCoverPoints : List String
    , pantsPaths : List String
    , shoePaths : List String
    , rightShirtPath : String
    , rightWristPath : String
    , rightHandPath : String
    , rightCuffPath : String
    , leftArmOutlinePath : String
    , leftWristPath : String
    , leftClipPath : String
    , leftShirtPath : String
    , leftCuffPath : String
    , leftHandPath : String
    , leftHandWristEllipse : Svg.Svg Never
    , leftFingersPath : String
    , leftHandCoverPoints : String
    , shoulderCovers : List (Svg.Svg Never)
    , eyeEllipses : List (Svg.Svg Never)
    , irisEllipses : List (Svg.Svg Never)
    , eyebrowPaths : List String
    , mouthPatchPath : String
    , cheekPatchPaths : List String
    , mouth : Svg.Svg Never
    , nosePath : String
    , noseBridgeLine : Svg.Svg Never
    , pointerPath : String
    , pointerMagicPaths : List String
    , collarPoints : String
    , shirtLine : Svg.Svg Never
    , buttonEllipses : List (Svg.Svg Never)
    , accentPaths : List String
    }
    -> Nri.Ui.Svg.V1.Svg
renderFullRed config =
    let
        mask name =
            Css.property "mask" ("url(#" ++ config.name ++ name ++ ")")

        { stroke, fill } =
            redPalette
    in
    Nri.Ui.Svg.V1.init "0 0 480 600"
        [ Svg.defs
            []
            [ Svg.g
                [ id (config.name ++ "_body"), css [ stroke.width 2.62 ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.bodyPaths)
            , Svg.path [ id (config.name ++ "_head"), d config.headPath, css [ stroke.width 2.62 ] ] []
            , Svg.path [ id (config.name ++ "_torso"), d config.torsoPath, css [ stroke.width 2.72 ] ] []
            , Svg.path [ id (config.name ++ "_shirt"), css [ stroke.width 2.72 ], d config.rightShirtPath ] []
            , Svg.path [ id (config.name ++ "_leftShirt"), d config.leftClipPath ] []
            , Svg.g
                [ id (config.name ++ "_ears"), css [ stroke.width 2.62 ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.earPaths)
            , Svg.g
                [ id (config.name ++ "_pants"), css [ stroke.width 2.62 ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.pantsPaths)
            ]
        , Svg.g
            [ css
                [ Css.color (Css.hex "231F20")
                , Css.property "stroke-miterlimit" "10"
                ]
            ]
            [ Svg.g
                [ css [ fill.platform, Css.opacity (Css.num 0.35) ] ]
                [ config.platformEllipse
                ]
            , Svg.use [ xlinkHref ("#" ++ config.name ++ "_body"), css [ fill.body, stroke.black ] ] []
            , Svg.mask [ id (config.name ++ "_bodyMask") ]
                [ Svg.use [ xlinkHref ("#" ++ config.name ++ "_body"), maskStyle ] []
                ]
            , Svg.g
                [ css [ fill.bodySecondary, stroke.black, stroke.width 2.25, mask "_bodyMask" ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.tailStripePaths)
            , Svg.use [ xlinkHref ("#" ++ config.name ++ "_ears"), css [ fill.bodyTertiary, stroke.black ] ] []
            , Svg.mask [ id (config.name ++ "_earsMask") ]
                [ Svg.use [ xlinkHref ("#" ++ config.name ++ "_ears"), maskStyle ] []
                ]
            , Svg.g
                [ css [ Css.property "mask" ("url(#" ++ config.name ++ "_earsMask" ++ ")") ] ]
                [ Svg.g
                    [ css [ fill.bodySecondary ] ]
                    (List.map (\path -> Svg.path [ d path ] []) config.earFoldPaths)
                , Svg.g
                    []
                    (List.map (\path -> Svg.path [ d path ] []) config.earLinePaths)
                ]

            -- Shirt
            , Svg.use [ xlinkHref ("#" ++ config.name ++ "_shirt"), css [ fill.shirt, stroke.black ] ] []
            , Svg.mask [ id (config.name ++ "_shirtMask") ]
                [ Svg.use [ xlinkHref ("#" ++ config.name ++ "_shirt"), maskStyle ] []
                ]
            , Svg.g
                [ css [ mask "_shirtMask" ] ]
                [ Svg.path [ css [ fill.body, stroke.black, stroke.width 2.62 ], d config.rightWristPath ] []
                , Svg.path [ css [ fill.bodySecondary ], d config.rightHandPath ] []
                , Svg.path [ css [ fill.none, stroke.black, stroke.width 1.87 ], d config.rightCuffPath ] []
                ]
            , Svg.path [ d config.leftWristPath, css [ fill.body ] ] []
            , Svg.clipPath [ id (config.name ++ "_leftShirtClip") ]
                [ Svg.use [ xlinkHref ("#" ++ config.name ++ "_leftShirt") ] [] ]
            , Svg.g
                [ css [ Css.property "clip-path" ("url(#" ++ config.name ++ "_leftShirtClip)") ] ]
                [ Svg.path [ d config.leftShirtPath, css [ fill.shirt, stroke.black, stroke.width 2.72 ] ] []
                , Svg.path [ d config.leftCuffPath, css [ fill.none, stroke.black, stroke.width 1.87 ] ] []
                , Svg.g [ css [ fill.bodySecondary ] ] [ config.leftHandWristEllipse ]
                ]
            , Svg.path [ d config.leftArmOutlinePath, css [ fill.none, stroke.black, stroke.width 3.13 ] ] []
            , Svg.path [ d config.leftHandPath, css [ fill.bodySecondary, stroke.black, stroke.width 2.25 ] ] []
            , Svg.use [ xlinkHref ("#" ++ config.name ++ "_pants"), css [ fill.pants, stroke.black ] ] []
            , Svg.use [ xlinkHref ("#" ++ config.name ++ "_torso"), css [ fill.shirt, stroke.black ] ] []
            , Svg.g
                [ css [ fill.shirt ] ]
                config.shoulderCovers
            , Svg.mask [ id (config.name ++ "_waistMask") ]
                [ Svg.use [ xlinkHref ("#" ++ config.name ++ "_torso"), maskStyle ] []
                ]
            , Svg.path [ css [ fill.pants, stroke.black, stroke.width 8.24, mask "_waistMask" ], d config.waistPath ] []
            , Svg.mask [ id (config.name ++ "_pantsMask") ]
                [ Svg.use [ xlinkHref ("#" ++ config.name ++ "_pants"), maskStyle ] []
                ]
            , Svg.g
                [ css [ fill.shoes, stroke.width 1.63, stroke.black, mask "_pantsMask" ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.shoePaths)
            , Svg.g
                [ css [ fill.pants ] ]
                (List.map (\ps -> Svg.polygon [ points ps ] []) config.hipCoverPoints)
            , Svg.use [ xlinkHref ("#" ++ config.name ++ "_head"), css [ fill.body, stroke.black ] ] []
            , Svg.g
                [ css [ fill.white, stroke.black, stroke.width 2.62 ] ]
                config.eyeEllipses
            , Svg.g
                [ css [ fill.iris ] ]
                config.irisEllipses
            , Svg.g
                [ css [ fill.bodyTertiary, stroke.width 2.62, stroke.black ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.eyebrowPaths)
            , Svg.mask [ id (config.name ++ "_headMask") ]
                [ Svg.use [ xlinkHref ("#" ++ config.name ++ "_head"), maskStyle ] []
                ]
            , Svg.g
                [ css [ mask "_headMask" ] ]
                [ Svg.path [ d config.mouthPatchPath, css [ fill.bodyTertiary, stroke.black, stroke.width 2.59 ] ] []
                , Svg.g
                    [ css [ fill.bodyTertiary, stroke.black, stroke.width 1.87 ] ]
                    (List.map (\path -> Svg.path [ d path ] []) config.cheekPatchPaths)
                ]
            , Svg.path [ d config.nosePath, css [ fill.iris ] ] []
            , Svg.g [ css [ fill.none, stroke.black, stroke.width 1.87 ] ] [ config.noseBridgeLine ]
            , config.mouth
            , Svg.path [ d config.pointerPath, css [ fill.pointer, stroke.black, stroke.width 1.87 ] ] []
            , Svg.g
                [ css [ fill.dark ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.pointerMagicPaths)
            , Svg.path [ d config.leftFingersPath, css [ fill.bodySecondary, stroke.black, stroke.width 2.25 ] ] []
            , Svg.polygon [ points config.leftHandCoverPoints, css [ fill.bodySecondary ] ] []
            , Svg.g
                [ css [ fill.none, stroke.black, stroke.width 1.87 ] ]
                [ Svg.polyline [ points config.collarPoints ] []
                , config.shirtLine
                ]
            , Svg.g
                [ css [ fill.button, stroke.black, stroke.width 1.12 ] ]
                config.buttonEllipses
            , Svg.g
                []
                (List.map (\path -> Svg.path [ d path ] []) config.accentPaths)
            ]
        ]


{-| Full height Red to be shown when presenting a question.
-}
redInstructive : Nri.Ui.Svg.V1.Svg
redInstructive =
    renderFullRed
        { name = "redInstructive"
        , platformEllipse = Svg.ellipse [ cx "221.1", cy "572.1", rx "92.5", ry "17.4" ] []
        , bodyPaths =
            [ "M221.1,433.6c0,0,28.6,23.2,49.8,3c21.4-20.4,20.3-48.4,45.2-59.4c18.8-8.4,29,0,29,0s-10.4,5-15.2,28.1c-4.9,23-0.5,49.4-21.3,65.5c-13.6,10.6-33.3,13.5-54.3,3.6c-22.3-10.5-34.5-34.1-34.5-34.1L221.1,433.6L221.1,433.6z"
            , "M187.7,325.9l-4.3,24.5c0,0-3.6,10.3,12.4,11.9c16,1.5,15.5-9.4,15.5-9.4l-3.9-27.5L187.7,325.9L187.7,325.9z"
            ]
        , headPath = "M110.7,276.1c0,0,9.2-37.7,51.9-58.1c30.4-14.5,66.1-12,98.2,14.5c36.5,30.1,33.3,65,33.3,65s-19.6,28.8-45.9,37.9c-6.1,2.1-18.5,3.7-24.4,3.3c-22.2-1.5-27.6-0.7-45.9-3.1c-5.7-0.8-11.3-1.2-17.7-3.3C120.4,319.5,110.7,276.1,110.7,276.1L110.7,276.1z"
        , torsoPath = "M232.1,401.8c0,29.1,0.7,46.8-33.2,46.8c-33.9,0-33.9-17.6-33.9-46.8c0-18.6,5.1-40.4,14.4-49.8c5.3-5.4,3.7,0.6,19.1,0.6c12.8,0,12.7-6.6,17.7-1.7C225.9,360.2,232.1,382.7,232.1,401.8L232.1,401.8z"
        , tailStripePaths =
            [ "M266.8,427c0,0,15.6-4.1,27.8,6c24.3,20.1,16.5,49.3,16.5,49.3l-44.2,8l-13.6-6.1c0,0,13.4-2.5,11.2-23.8s-22.5-27.1-22.5-27.1L266.8,427L266.8,427z"
            , "M282.3,404c0,0,14.9-4.2,26.7,0s29.5,27.8,29.5,27.8l26.1-59.5l-36.7-21.6l-42.6,40.5L282.3,404L282.3,404z"
            ]
        , earPaths =
            [ "M184.2,214.1c0,0,1-15.1-12-35.4c-11.7-18.4-23.6-22.1-31.4-13.1c-3.8,4.4-15.7,25.5-22.6,52.6c-6.4,25.2-7.2,49.7-7.5,55.9C110.2,286.1,184.2,214.1,184.2,214.1L184.2,214.1z"
            , "M227.7,214.3c0,0,0.9-15.1,16.2-33.7c13.9-16.8,26.1-19,32.8-9.2c3.3,4.8,12.4,27.2,16,54.9c3.3,25.8,1,50.2,0.6,56.4C292.4,294.8,227.7,214.3,227.7,214.3L227.7,214.3z"
            ]
        , earFoldPaths =
            [ "M153.9,235.9c0,0,13.5-25.3,10.8-47.3c-2.6-20.8-25.1-52.6-25.1-52.6l49.1,34.3l3.9,30.1v12.2L153.9,235.9L153.9,235.9z"
            , "M255.1,239.6c0,0-10.3-26.8-4.9-48.3c5.1-20.3,31.4-49.2,31.4-49.2l-53,28l-7.6,29.4l-1.5,12.1L255.1,239.6z"
            ]
        , earLinePaths =
            [ "M153.2,236.6c-0.3-0.3-0.4-1-0.2-1.4l0.1-0.1l0.1-0.2c0.1-0.2,0.4-0.9,0.5-1.1c6.1-13.5,10.9-28.3,10.2-43.3c-0.5-6.7-2.5-13.1-5-19.3c-3.3-8.3-7.6-16.3-12.2-23.9c-2.4-3.8-5-7.5-8-10.8l-3.5-5l5,3.5l49.1,34.3l0.4,0.3l0.1,0.5c0,0.2,3.9,30.1,3.9,30.2v12.2v0.6l-0.5,0.3C181.8,220.5,165,229.9,153.2,236.6L153.2,236.6L153.2,236.6z M154.7,235.1c10.9-7.1,26.4-16.9,37.4-23.5l-0.5,0.9v-12.2c0.1,0.7-3.9-30.1-3.9-29.9l0.4,0.7L139,136.8c0,0,1.5-1.5,1.5-1.5c6.4,12,14.1,23.1,19.6,35.5c3.8,8.2,6.6,17.2,6.1,26.4c-0.6,13.1-4.7,25.9-10.4,37.6l-0.4,0.8l-0.2,0.4l-0.1,0.2l-0.1,0.1c0,0,0,0.1,0,0C155.1,235.9,154.9,235.3,154.7,235.1L154.7,235.1L154.7,235.1z"
            , "M254.5,238.8c-0.3,0.1-0.6,0.8-0.4,1.1l0-0.1c-0.3-0.8-0.8-2.2-1.1-3c-5.1-16-8.2-34.1-2.1-50.3c3.4-8.4,8.4-16,13.7-23.4c5.2-7.4,11.1-14.1,16.3-21.6c0,0,1.3,1.7,1.3,1.7l-53,28l0.5-0.7c0.1,0-7.7,29.8-7.5,29.2l-1.5,12.1l-0.4-1C230.3,218.7,244.5,230.5,254.5,238.8L254.5,238.8L254.5,238.8z M255.7,240.5c-10.9-8.2-26.3-19.5-36.8-27.9l-0.5-0.4l0.1-0.6l1.5-12.1c-0.1-0.1,7.6-29.4,7.6-29.5l0.1-0.5l0.4-0.2l53-28l5.4-2.9l-4.1,4.5c-3.4,2.9-6.5,6.3-9.3,9.8c-5.6,7-10.8,14.4-15.1,22.3c-3.2,5.9-6,11.9-7.3,18.5c-2.6,14.8,0.4,30.1,4.8,44.2l0.3,0.8l0.1,0.4l0.1,0.2l0,0.1C256.3,239.6,256,240.3,255.7,240.5L255.7,240.5L255.7,240.5z"
            ]
        , waistPath = "M158.7,408.4c0,0,16.2,8,38.5,8c23.9,0,40-8,40-8s1.7,23.9,0,35.2s-39,42.6-56.4,31.3S153,422.1,153,422.1L158.7,408.4L158.7,408.4z"
        , hipCoverPoints =
            [ "173.599 449.103 168.214 429.796 227.741 429.712 226.69 450.862"
            ]
        , pantsPaths =
            [ "M226.6,433.3c0,0,17.4,58.5,20,67.3c3.4,11.6,12.4,40.1,17.4,48.2c5.8,9.3,16.9,15.7,17.7,19.7s-6.3,4.2-17.2,1.9c-5.4-1.1-11.8-3-15.2-4c-2.5-0.7-6-3.4-6.8-5.9c-3.7-11.7-15.8-46.2-18.3-53.4c-3-8.7-18.7-58.6-18.7-58.6"
            , "M169.2,433.6c0,0,5.3,60.8,6.1,69.9c1.1,12,5.3,40.5,0.9,49c-4.4,8.5-16.9,10.4-16.2,14.5s15.5,6.3,26.6,2.9c5.2-1.6,9.6-3.5,12.4-5.7c2-1.6,3.5-3.8,3.3-6.4c-0.8-12.2-3.3-48.8-3.6-56.5c-0.4-9.2-1.7-61.2-1.7-61.2"
            ]
        , shoePaths =
            [ "M271.8,544.7c0,0-4.9,5.6-16.7,7.4c-15.5,2.3-34.8-8.1-34.8-8.1l26.5,36.7l38.7-8.4l13-21.7L271.8,544.7L271.8,544.7z"
            , "M169.6,547.1c0,0,3.3,5.8,15.3,6.2c12,0.4,25.2-3.1,25.2-3.1l-4.8,19.9l-38.3,10.3l-21.5-13.3L169.6,547.1L169.6,547.1z"
            ]
        , rightShirtPath = "M236.1,348.6c24,2.8,41.5,19.2,38.2,37.7c-3.3,18.5-32.2,29.8-32.2,29.8l11.8,6.6c0,0-1.1,6.6-11.8,7.1s-15.5-6.9-15.5-6.9l1.8-18.3c0,0,18.2-8.4,22.9-13.1c5.1-5.2,5.6-14.1-3.2-19.2c-8.8-5.1-27.8,0-27.8,0l-11-21.4C209.4,350.9,212.1,345.7,236.1,348.6z"
        , rightWristPath = "M241.3,391.9c0,0,13.9,0.4,19.5,4.6c5.6,4.2,10.3,17,10.3,17l-11.5,16.3l-30.7,8.9l-16-22.3L241.3,391.9    L241.3,391.9z"
        , rightHandPath = "M227.7,401.9c0,0,7,0.8,11.1,3.7c4.1,2.9,5.8,9.6,5.8,9.6l15.9,3.1l-6.5,15.2l-17.8,3.1l-13-5l2-34.3    L227.7,401.9L227.7,401.9z"
        , rightCuffPath = "M276.9,401.9c0,0-4.7-11.7-13.6-14.7c-8.9-3-26.3,6.8-26.3,6.8"
        , leftArmOutlinePath = "M175.5,388.2c0,0-40.6,20.5-68.6-3c-30.6-25.7-29-60.4-26.3-65.9c1.7-3.4,4.5-8.7,9.6,1.9   s5.2,23.9,25.9,42.9c10.5,9.6,26.9,11.4,36.9,8.1c13.4-4.5,19.6-16.9,26.4-20.1s9,3.7,5.2,10.3   C179.8,370.4,175.5,388.2,175.5,388.2L175.5,388.2z"
        , leftWristPath = "M175.5,388.2c0,0-40.6,20.5-68.6-3c-30.6-25.7-29-60.4-26.3-65.9c1.7-3.4,4.5-8.7,9.6,1.9   s5.2,23.9,25.9,42.9c10.5,9.6,26.9,11.4,36.9,8.1c13.4-4.5,19.6-16.9,26.4-20.1s9,3.7,5.2,10.3   C179.8,370.4,175.5,388.2,175.5,388.2L175.5,388.2z"
        , leftClipPath = "M175.5,388.2c0,0-40.6,20.5-68.6-3     c-30.6-25.7-29-60.4-26.3-65.9c1.7-3.4,4.5-8.7,9.6,1.9s5.2,23.9,25.9,42.9c10.5,9.6,26.9,11.4,36.9,8.1     c13.4-4.5,19.6-16.9,26.4-20.1s9,3.7,5.2,10.3C179.8,370.4,175.5,388.2,175.5,388.2L175.5,388.2z"
        , leftShirtPath = "M70.5,360.6c0,0,19.2-1.9,25.9-4.9s12.8-11.9,12.8-11.9l38.3,11.9l32.5-16.9l21,23.5l-14.4,39.7l-73.4,2.9     l-53.8-42.4L70.5,360.6L70.5,360.6z"
        , leftCuffPath = "M83.8,370.2c0,0,10.3-0.5,18.2-5.6c7.8-5.2,12.5-11.2,12.5-11.2"
        , leftHandWristEllipse = Svg.ellipse [ transform "matrix(0.9759 -0.2181 0.2181 0.9759 -69.6343 26.5542)", cx "85.4", cy "328.6", rx "9.2", ry "5.6" ] []
        , leftHandPath = "M82.2,330.9c0,0-7.1,1.8-11.4,1.5c-4.3-0.2-7.9-11.2-7.7-17.5c0.2-6.3,11.4-6.8,11.4-6.8s1.1-2.7,5.1-2   c4,0.7,9.4,1.6,11.3,9c2.1,8.2,2,16.9,2,13"
        , leftFingersPath = "M68.6,327.6c2.7,0.1,4-1.4,4.6-2.2c2.6-3.6,4.5-1.9,5.6-4.4c2.3-4.7-3.9-12.6-3.9-12.6s-1.4-2.2-4.8-2.1   c-2.1,0.1-4.8,2.2-6.6,3.9c-1.5,1.4-2.2,3.4-1.8,5.4c0.2,1.3,0.7,3,1.4,5.2C64.7,325.8,66,327.5,68.6,327.6L68.6,327.6z"
        , leftHandCoverPoints = "66.2,324.8 68.8,329.6 74.5,329.4 77.7,326.9 71.9,322.1  "
        , shoulderCovers =
            [ Svg.polygon [ points "164.3,367.6 179.8,353.2 174.7,386.7 157.6,391.6 " ] []
            , Svg.path [ d "M213.9,350.5c0,0,0.8-1.2,5.1-1.4s7.8,1,7.8,1l6.5,16.8l-16.6,7.3L213.9,350.5L213.9,350.5z" ] []
            ]
        , eyeEllipses =
            [ Svg.ellipse [ transform "matrix(0.9716 -0.2367 0.2367 0.9716 -58.5488 62.9111)", cx "232.8", cy "275.3", rx "13.1", ry "14" ] []
            , Svg.ellipse [ transform "matrix(0.2367 -0.9716 0.9716 0.2367 -128.6066 385.0194)", cx "180.7", cy "274.4", rx "14", ry "13.1" ] []
            ]
        , irisEllipses =
            [ Svg.ellipse [ transform "matrix(0.994 -0.1092 0.1092 0.994 -28.7011 26.645)", cx "228.9", cy "275.3", rx "5.5", ry "6.6" ] []
            , Svg.ellipse [ transform "matrix(0.1092 -0.994 0.994 0.1092 -108.288 427.8788)", cx "184.6", cy "274.4", rx "6.6", ry "5.5" ] []
            ]
        , eyebrowPaths =
            [ "M221.5,235.8c-1.7-0.3-2.4-2.4-1.2-3.7c3.4-3.4,9.4-8.2,14.9-6.4s8.1,8.9,9.3,13.6c0.4,1.8-1.3,3.2-3,2.5c-2.9-1.2-6.8-2.9-9.4-3.7C229.5,237.4,224.8,236.4,221.5,235.8L221.5,235.8z"
            , "M188.1,249c1.7-0.3,2.4-2.4,1.2-3.7c-3.4-3.4-9.4-8.2-14.9-6.4s-8.1,8.9-9.3,13.6c-0.4,1.8,1.3,3.2,3,2.5c2.9-1.2,6.8-2.9,9.4-3.7C180.1,250.6,184.8,249.6,188.1,249L188.1,249z"
            ]
        , mouthPatchPath = "M171.5,338.1c0,0,2.2-25.4,11.8-39.4c8.1-11.9,14.8-14.4,24.8-13.2c9.8,1.1,19.5,16.2,22,35.5s1.9,21.5,1.9,21.5L171.5,338.1L171.5,338.1z"
        , cheekPatchPaths =
            [ "M241,337.1c0,0,7-2.9,11.2-16.6c4.2-13.6,0.6-37.3,15.3-38.8c14.7-1.5,14.6,18.2,13,27.7c-1.6,9.4-7.6,29.4-7.6,29.4L241,337.1L241,337.1z"
            , "M162.3,333c0,0-7-2.9-11.2-16.6c-4.2-13.6-0.6-37.3-15.3-38.8c-14.7-1.5-14.6,18.2-13,27.7c1.7,9.4,7.6,29.4,7.6,29.4L162.3,333L162.3,333z"
            ]
        , mouth =
            Svg.path
                [ css [ redPalette.fill.dark ]
                , d "M189.6,320.9c2.7,1.9,5.8,2.5,8.9,2.6c3.1,0.1,6.2-0.1,9.1-1c6.1-1.9,10.6-6.6,13.5-12.2c-1.2,6.6-6.3,12.6-12.7,14.7C202.2,326.9,193.5,326.6,189.6,320.9L189.6,320.9L189.6,320.9z"
                ]
                []
        , nosePath = "M192.9,306.1c-2.3,0-3.9-2.1-3.2-4.1c1.8-5,5.8-12.6,13.7-11.8s10.7,7.6,11.9,12.1c0.5,2-1.1,3.8-3.3,3.8L192.9,306.1L192.9,306.1z"
        , noseBridgeLine = Svg.line [ x1 "201.9", y1 "303.2", x2 "201", y2 "323.5" ] []
        , pointerPath = "M72.6,323.5l5.1-5.2L48.2,216.1c0,0-0.2-0.2-0.5-0.4c-1.2-0.6-2.5,0.5-2.2,1.8L72.6,323.5z"
        , pointerMagicPaths =
            [ "M44.1,203.4c-1.9-5.7-3.9-11.4-5.8-17.2c-0.3-0.8-0.5-1.6-0.8-2.4c-0.6-1.7-3.3-1-2.7,0.8c1.9,5.7,3.9,11.4,5.8,17.2c0.3,0.8,0.5,1.6,0.8,2.4C42,205.9,44.7,205.1,44.1,203.4L44.1,203.4z"
            , "M36.8,207.9c-3.3-2.2-6.5-4.4-9.8-6.7c-0.5-0.3-0.9-0.6-1.4-0.9c-0.6-0.4-1.6-0.1-1.9,0.5c-0.4,0.7-0.1,1.5,0.5,1.9c3.3,2.2,6.5,4.4,9.8,6.7c0.5,0.3,0.9,0.6,1.4,0.9c0.6,0.4,1.6,0.1,1.9-0.5C37.7,209.2,37.4,208.4,36.8,207.9L36.8,207.9z"
            , "M53.1,204.2c1-2.9,2.1-5.7,3.1-8.6c0.2-0.4,0.3-0.8,0.5-1.2c0.3-0.7-0.3-1.5-1-1.7c-0.8-0.2-1.5,0.3-1.7,1c-1,2.9-2.1,5.7-3.1,8.6c-0.2,0.4-0.3,0.8-0.5,1.2c-0.3,0.7,0.3,1.5,1,1.7C52.2,205.4,52.9,204.9,53.1,204.2L53.1,204.2z"
            ]
        , collarPoints = "212.9,349.1 211.3,379.5 197.2,355.7 183.1,381.1 180.7,350.9 "
        , shirtLine = Svg.line [ x1 "197.2", y1 "416.4", x2 "197.2", y2 "358" ] []
        , buttonEllipses =
            [ Svg.ellipse [ cx "190.5", cy "407.2", rx "1.7", ry "2.5" ] []
            , Svg.ellipse [ cx "190.7", cy "392.6", rx "1.7", ry "2.5" ] []
            , Svg.ellipse [ transform "matrix(0.163 -0.9866 0.9866 0.163 -211.0071 505.6112)", cx "192.5", cy "377.2", rx "2.5", ry "1.7" ] []
            ]
        , accentPaths =
            [ "M230.2,380.4c-3.5-5.1-4.7-11-6.3-16.8C228.3,366.9,230.4,375,230.2,380.4L230.2,380.4z"
            , "M164.4,399.8c-0.6-3.6-0.4-7.1,0.1-10.7c0.5-3.5,1.2-7.1,2.9-10.3C167.8,385.9,167.1,393.1,164.4,399.8L164.4,399.8z"
            , "M186.1,446.7c5,0.8,9.9,1.7,14.9,1.7c5,0,9.9-1.5,15-1.8C206.1,452.1,196.1,452.8,186.1,446.7L186.1,446.7L186.1,446.7z"
            , "M252.8,425.1c-1.6,3.2-6.3,3.3-9.2,1.9c-3.1-1.4-5.2-4.1-6.6-7c2.3,1.8,4.6,4,7.4,4.7C247.3,425.4,249.7,425.9,252.8,425.1  L252.8,425.1L252.8,425.1z"
            , "M245.8,429.5c-3,2.1-7.3,2.4-10.4,0.1c-2.1-1.8-3.4-4.4-3.8-7.1c-0.1-0.9-0.1-1.8,0.2-2.6c0.8,3,2.8,5.7,5.1,7.6  C239.5,429.3,242.7,429.4,245.8,429.5L245.8,429.5L245.8,429.5z"
            , "M76.4,311.9c-4.2-0.6-8.4,1.4-10.8,5C65,311.8,72.8,308.5,76.4,311.9L76.4,311.9z"
            , "M79.7,316.2c-4.2-0.6-8.4,1.4-10.8,5C68.3,316,76.2,312.8,79.7,316.2L79.7,316.2z"
            , "M84.4,314.2c-3.4,2.8-10.2-0.8-9.1-5.2C77.3,312.2,80.5,314.1,84.4,314.2L84.4,314.2z"
            ]
        }


{-| Full height Red to be shown on a wrong answer.
-}
redSupportive : Nri.Ui.Svg.V1.Svg
redSupportive =
    renderFullRed
        { name = "redSupportive"
        , platformEllipse = Svg.ellipse [ cx "220.93", cy "572.49", rx "92.47", ry "17.39" ] []
        , bodyPaths =
            [ "m216.65,433.66s25.67,5.03,39.42,20.04c12.61,13.76,20.86,31.99,38.67,37.4,19.26,5.85,38.31-2.95,38.31-2.95,0,0-26.94,69.26-76.61,31.21-29.04-22.24-34.23-79.86-34.23-79.86l-5.56-5.83Z"
            , "m190.59,326.18l-4.33,24.49s-3.61,10.3,12.4,11.85c16.01,1.55,15.51-9.42,15.51-9.42l-3.9-27.45-19.67.54Z"
            ]
        , headPath = "m116.01,269.71s10.87-37.16,52.72-55.21c29.74-12.83,63.66-8.36,92.69,19.84,33,32.07,27.96,66.71,27.96,66.71,0,0-20.28,27.64-45.84,35.36-5.88,1.78-17.82,2.7-23.47,1.99-21.07-2.67-26.25-2.17-43.5-5.61-5.39-1.08-10.71-1.8-16.71-4.22-37.12-14.99-43.85-58.86-43.85-58.86Z"
        , torsoPath = "m231.98,402.16c0,29.16,3.33,47.52-33.23,46.8-33.89-.66-33.9-17.64-33.9-46.8,0-18.58,5.09-40.35,14.43-49.76,5.31-5.35,3.67.56,19.13.56,12.75,0,12.68-6.56,17.74-1.67,9.58,9.27,15.82,31.78,15.82,50.86Z"
        , tailStripePaths =
            [ "m269.84,456.38s9.2,15.24,6.42,29.69c-5.55,28.9-48.84,19.21-48.84,19.21l-9.36-20.53-1.12-13.86s15.79,12.96,32.23,1.66c16.44-11.29,4.33-33.64,4.33-33.64l16.32,17.47Z"
            , "m283.55,479.67s19.5,13.69,21.01,25.26c2.72,20.82-9.41,33.23-9.41,33.23l64.5-27.37-11.12-38.17-54.87.22-10.11,6.83Z"
            ]
        , earPaths =
            [ "m197.38,217.54s3.12-14.81-5.43-36.62c-7.73-19.73-17.71-24.99-26.04-17.14-4.05,3.81-17.71,23.13-27.87,48.99-9.45,24.06-13.71,48.16-14.89,54.27-2.31,11.89,74.23-49.5,74.23-49.5Z"
            , "m230.02,214.58s2.9-16.4,21.44-34.89c16.77-16.73,29.82-17.77,35.51-6.21,2.77,5.62,9.38,31.14,9.41,61.81.03,28.53-5.59,54.87-6.9,61.6-2.55,13.08-59.47-82.3-59.47-82.3Z"
            ]
        , earFoldPaths =
            [ "m167.22,235.07s15.74-23.27,16.57-45.37c.79-20.91-14.6-55.39-14.6-55.39l38.65,40.47-.97,30.26-1.8,12.07-37.86,17.96Z"
            , "m255.28,245.4s-7.19-30.43,1.29-53.27c8.02-21.6,39.35-50.03,39.35-50.03l-59.1,24.5-11.8,31.19-3.17,13.05,33.43,34.57Z"
            ]
        , earLinePaths =
            [ "m166.41,235.77c-.26-.29-.33-1.01-.02-1.38,0,0,.05-.08.05-.08l.11-.17c.13-.21.51-.82.65-1.05,6.44-10.8,11.86-22.43,14.47-34.77,3.68-17.3-2.06-35.25-7.76-51.56-1.57-4.14-3.35-8.23-5.68-12.01,0,0-2.48-5.61-2.48-5.61,0,0,4.23,4.43,4.23,4.43l38.65,40.47s.31.32.31.32c0,0-.01.45-.01.45-.03.14-.95,30.26-.98,30.39,0,0-1.8,12.07-1.8,12.07,0,0-.08.56-.08.56-11.38,5.55-27.89,12.72-39.64,17.93h0Zm1.62-1.39c10.73-5.51,25.8-13.17,36.59-18.23,0,0-.6.81-.6.81l1.8-12.07c0,.63.94-30.28.96-30.14,0,0,.3.77.3.77,0,0-38.65-40.47-38.65-40.47l1.75-1.17c2.45,8.66,5.94,16.78,8.58,25.29,2.69,8.46,5.1,17.09,5.87,26.07.98,17.03-6.13,33.29-14.68,47.6-.43.67-1.26,2-1.72,2.65,0,0-.06.09-.06.09.23-.28.16-.98-.14-1.2h0Z"
            , "m254.77,244.46c-.31.07-.67.68-.56,1.05.02.08,0,0,0,.02,0,0-.03-.12-.03-.12-.09-.39-.27-1.2-.35-1.6-3.22-16.35-4.12-33.77,1.05-49.82,2.38-7.1,6.56-13.47,10.96-19.42,8.81-12,19.62-22.01,29.37-33.25,0,0,1.13,1.78,1.13,1.78,0,0-59.1,24.5-59.1,24.5,0,0,.59-.61.59-.61.09,0-12.01,31.58-11.76,31.06,0,0-3.17,13.04-3.17,13.04,0,0-.27-1-.27-1,9.51,9.77,22.81,24.2,32.14,34.37h0Zm1.03,1.88c-10.29-10.23-24.88-24.41-34.71-34.76,0,0-.41-.42-.41-.42,0,0,.14-.57.14-.57l3.17-13.05c-.05-.13,11.85-31.18,11.84-31.31,0,0,.16-.43.16-.43,0,0,.43-.18.43-.18l59.1-24.5s5.64-2.34,5.64-2.34c0,0-4.51,4.12-4.51,4.12-4.01,2.85-7.62,6.22-11.06,9.72-6.81,7.06-13.21,14.57-18.79,22.64-4.12,6.04-7.85,12.36-10.03,19.34-4.59,15.81-3.57,32.72-.8,48.78,0,0,.17.89.17.89,0,0,.09.44.09.44l.05.22.02.11c.16.42-.17,1.14-.5,1.29h0Z"
            ]
        , waistPath = "m154.46,408.39s16.22,7.97,38.46,7.97c23.89,0,45.59-7.97,45.59-7.97,0,0,4.87,24.85,3.17,36.22-1.71,11.37-47.76,41.67-65.21,30.3-17.46-11.37-27.69-52.88-27.69-52.88l5.69-13.65Z"
        , hipCoverPoints =
            [ "175.74 456.63 172.29 431.4 197.54 438.517 192.91 461.77 175.74 456.63"
            , "206.29 459.37 185.561 438.865 220.57 435.09 222.51 464.22 206.29 459.37"
            ]
        , pantsPaths =
            [ "m172.05,433.96s5.32,60.76,6.13,69.94c1.07,12.04,5.28,40.47.85,48.95-4.42,8.48-16.93,10.44-16.19,14.5.74,4.06,15.47,6.28,26.56,2.9,5.23-1.59,9.56-3.48,12.35-5.68,2.03-1.6,3.46-3.82,3.29-6.4-.83-12.22-3.2-48.84-3.63-56.48-.63-11.24-4-63.04-4-63.04"
            , "m221.17,438.65s7.64,60.51,8.78,69.66c1.5,12,5.66,41.55,9.34,50.38,4.24,10.15,14.09,18.22,14.23,22.34s-6.86,3.07-17.25-.93c-5.1-1.96-11.17-4.89-14.37-6.45-2.32-1.13-5.36-4.36-5.73-6.92-1.77-12.12-8.09-48.13-9.4-55.67-1.57-9.09-11.1-60.92-11.1-60.92"
            ]
        , shoePaths =
            [ "m172.43,547.45s3.31,5.79,15.31,6.21c12,.41,25.25-3.1,25.25-3.1l-4.76,19.87-38.28,10.35-21.52-13.35,24-19.97Z"
            , "M 247.33 555.94 C 247.33 555.94 241.62 560.69 229.61 560.5 C 213.92 560.24 209.533 552.453 209.533 552.453 L 216.81 587.41 L 256.42 585.45 L 272.78 566.12 L 247.34 555.94 L 247.33 555.94 Z"
            ]
        , rightShirtPath = "m211.53,351.05s3.45-4.98,27.45-2.15c24,2.83,41.46,19.19,38.19,37.7-3.27,18.54-32.21,29.84-32.21,29.84l11.76,6.61s-1.11,6.6-11.79,7.08c-10.68.47-15.47-6.9-15.47-6.9l1.84-18.26s18.23-8.38,22.85-13.12c5.12-5.25,5.65-14.13-3.18-19.25-8.83-5.13-27.75,0-27.75,0l-11.68-21.56Z"
        , rightWristPath = "m244.17,392.19s13.93.38,19.5,4.62c5.57,4.25,10.31,17.01,10.31,17.01l-11.53,16.34-30.69,8.88-15.98-22.3,28.39-24.54Z"
        , rightHandPath = "m230.56,402.22s7,.77,11.12,3.7c4.12,2.93,5.75,9.64,5.75,9.64l15.94,3.15-6.49,15.26-17.8,3.1-12.98-5.01,2.04-34.3,2.41,4.46Z"
        , rightCuffPath = "m279.71,402.22s-4.73-11.67-13.64-14.71c-8.91-3.04-26.27,6.83-26.27,6.83"
        , leftArmOutlinePath = "m169.65,388.57s-43.38,13.64-67.15-14.13c-25.99-30.37-18.78-64.28-15.18-69.34,2.19-3.09,5.83-7.88,9.18,3.44,3.34,11.32,1.29,24.43,18.54,46.57,8.76,11.23,24.69,15.63,35.06,13.97,13.96-2.24,24.01-16.55,31.22-18.52,7.54-2.07,6.39,8.19,1.51,14-6.02,7.18-13.18,24.02-13.18,24.02Z"
        , leftWristPath = "m169.65,388.57s-43.38,13.64-67.15-14.13c-25.99-30.37-18.78-64.28-15.18-69.34,2.19-3.09,5.83-7.88,9.18,3.44,3.34,11.32,1.29,24.43,18.54,46.57,8.76,11.23,24.69,15.63,35.06,13.97,13.96-2.24,24.01-16.55,31.22-18.52,7.54-2.07,6.39,8.19,1.51,14-6.02,7.18-13.18,24.02-13.18,24.02Z"
        , leftClipPath = "m169.65,388.57s-43.38,13.64-67.15-14.13c-25.99-30.37-18.78-64.28-15.18-69.34,2.19-3.09,5.83-7.88,9.18,3.44,3.34,11.32,1.29,24.43,18.54,46.57,8.76,11.23,24.69,15.63,35.06,13.97,13.96-2.24,24.01-16.55,31.22-18.52,7.54-2.07,6.39,8.19,1.51,14-6.02,7.18-13.18,24.02-13.18,24.02Z"
        , leftShirtPath = "m70.53,344.23s19.31,1.23,26.36-.63c7.05-1.86,14.56-9.61,14.56-9.61l35.85,17.94,34.87-11.38,16.87,26.57-20.72,36.77-72.94-9.13-46.12-50.64,11.28.1Z"
        , leftCuffPath = "m82.12,355.89s10.25,1.24,18.84-2.61,14.18-9.05,14.18-9.05"
        , leftHandPath = "m85.3,317.71s-8.45-3.27-13.14-5.47c-10.98-5.14-14.23-11.72-10.8-14.62,2.86-2.42,6.12,2.14,9.83,3.78,3.66,1.62,4.74,1.73,4.74,1.73,2.08,2.2-14.67-14.4-7.51-17.49,3.05-1.31,8.3,5.6,11.17,8.66,4.63,4.94,4.01,3.34,4.01,3.34,0,0-10.48-12.87-5.34-15.91,6.89-4.08,13.39,14.13,19.48,18.32,1.98,1.36,5.98-8.57,10.73-7.04,6.34,2.05-7.32,22.86-11.1,24.67"
        , leftHandWristEllipse = Svg.ellipse [ cx "90.48", cy "315.15", rx "9.21", ry "5.6", transform "translate(-17.53 5.57) rotate(-3.21)" ] []
        , leftFingersPath = ""
        , leftHandCoverPoints = ""
        , shoulderCovers =
            [ Svg.path [ d "m163.73,365.53s9.19-8.41,14.6-12.05c6.11-4.11,4.35-2.84,4.32-2.82l-5.07,33.43-17.15,4.91,3.31-23.47Z" ] []
            , Svg.path [ d "m214.17,351.5s3.41-1.89,7.63-2.01c4.22-.12,7.44,0,7.44,0l8.19,18.53-17.97,6.59-5.29-23.11Z" ] []
            ]
        , eyeEllipses =
            [ Svg.ellipse [ cx "169.79", cy "263.77", rx "13.97", ry "12.57", transform "translate(-116.16 392.77) rotate(-81.51)" ] []
            , Svg.ellipse [ cx "221.45", cy "269.94", rx "14.24", ry "14.98", transform "translate(-57.17 59.51) rotate(-13.57)" ] []
            ]
        , irisEllipses =
            [ Svg.ellipse [ cx "172.54", cy "263.99", rx "6.32", ry "7.67" ] []
            , Svg.ellipse [ cx "218.94", cy "269.94", rx "7.84", ry "6.68", transform "translate(-86.11 436.52) rotate(-79.59)" ] []
            ]
        , eyebrowPaths =
            [ "m182.19,234.46c1.6-.08,2.45-1.91,1.48-3.18-2.64-3.45-7.49-8.5-12.64-7.53s-8.3,7.08-9.91,11.15c-.6,1.52.8,3.06,2.38,2.61,2.75-.78,6.45-1.77,8.82-2.18,2.49-.43,6.8-.71,9.87-.87Z"
            , "m218.32,230.06c-1.73-.85-1.91-3.23-.32-4.14,4.34-2.48,11.78-5.64,17.06-2.14s6.22,11.59,6.31,16.76c.03,1.93-2.15,2.92-3.71,1.69-2.71-2.15-6.38-4.98-8.82-6.55-2.57-1.65-7.21-4-10.52-5.62Z"
            ]
        , mouthPatchPath = "m161.26,334.8s-13.6-31.89-6.1-46.85c7.74-15.45,23.72-11.21,31.55-9.44,26.77,6.06,37.78,25.63,39.1,45.02,1.32,19.39,1.9,19.17,1.9,19.17l-66.45-7.89Z"
        , cheekPatchPaths =
            [ "m152.14,329.73s-5.26-3.47-7.64-18.37c-2.39-14.9,1.85-40.24-9.46-42.47-11.31-2.24-12.47,19.02-11.77,29.26s4.05,31.97,4.05,31.97l24.83-.39Z"
            , "m232.61,340.54s6.86-2.75,11.66-17.1c4.8-14.35,2.84-39.82,16.94-40.58s12.82,20.29,10.68,30.27c-2.15,9.99-9,30.94-9,30.94l-30.28-3.53Z"
            ]
        , mouth =
            Svg.g
                []
                [ Svg.path
                    [ css [ redPalette.fill.mouth, redPalette.stroke.black, redPalette.stroke.width 2.25 ]
                    , d "m161.71,309.58c0-.85,3.03.99,8.08,1.84,8.6,1.45,17.63,1.74,17.63,1.74,0,0-4.56,10.66-13.53,9.97-7.12-.54-12.16-7.72-12.18-13.56Z"
                    ]
                    []
                , Svg.path
                    [ css [ redPalette.fill.button, redPalette.stroke.black, redPalette.stroke.width 1.87 ]
                    , d "m170.15,321.48s2.27-3.09,6.03-4.28,9.34-.69,9.34-.69c0,0-2.46,4.51-6.75,5.98-4.29,1.46-8.65-.35-8.65-.35l.03-.66Z"
                    ]
                    []
                ]
        , nosePath = "m161.04,295.41c-2.08-.31-2.63-2.54-2.29-4.52.9-5.2,4.38-11.33,13.57-10.03,7.06,1,9.09,9.2,9.43,13.76.15,2.01-1.59,3.65-3.54,3.35l-17.18-2.56Z"
        , noseBridgeLine = Svg.path [ d "m169.02,291.83l-1.17,7.26c-.59,10.52,1.31,14.08,1.31,14.08" ] []
        , pointerPath = ""
        , pointerMagicPaths = []
        , collarPoints = "212.93 349.49 205.05 379.1 195.6 356 177.81 372.6 183.61 351.21"
        , shirtLine = Svg.path [ d "m190.86,414.52s-.81-7.56-.76-17.78c.09-18.52,5.62-40.74,5.62-40.74" ] []
        , buttonEllipses =
            [ Svg.ellipse [ cx "184.49", cy "406.39", rx "1.72", ry "2.46" ] []
            , Svg.ellipse [ cx "184.73", cy "391.83", rx "1.72", ry "2.46" ] []
            , Svg.ellipse [ cx "186.51", cy "376.4", rx "2.46", ry "1.72", transform "translate(-215.26 499.05) rotate(-80.62)" ] []
            ]
        , accentPaths =
            [ "m255.63,425.42c-1.64,3.25-6.31,3.35-9.18,1.9-3.1-1.39-5.18-4.06-6.55-7,2.3,1.8,4.63,3.95,7.39,4.73,2.9.73,5.29,1.22,8.34.38h0Z"
            , "m248.66,429.88c-2.96,2.15-7.3,2.41-10.36.12-2.12-1.77-3.36-4.39-3.81-7.07-.11-.88-.1-1.8.22-2.62.79,3.04,2.75,5.68,5.12,7.59,2.54,1.7,5.68,1.79,8.83,1.98h0Z"
            , "m79.49,303.91c.97-3.74,4.4-6.81,8.23-7.34-2.51,2.8-5.17,5.15-8.23,7.34h0Z"
            , "m182.46,447.08c4.38.85,8.69,1.88,13.07,2,4.39.16,8.68-1.23,13.16-1.45-4.06,2.26-8.42,4.18-13.21,4.07-4.72-.19-9.26-1.8-13.01-4.62h0Z"
            , "m241.67,370.37c-6.47,1.1-12.89,2.67-19.17,4.62,4.54-5.23,12.74-7.2,19.17-4.62h0Z"
            , "m164.1,400.14c-.52-3.62-.25-7.17.26-10.74.59-3.55,1.35-7.08,3.11-10.3.38,7.24-.56,14.37-3.37,21.04h0Z"
            ]
        }


renderFullSal :
    { name : String
    , platformEllipse : Svg.Svg Never
    , legPaths : List String
    , sockPaths : List String
    , shoePaths : List String
    , shoeLacePaths : List String
    , shoeStripPaths : List String
    , skirtPath : String
    , headPaths : List String
    , facePath : String
    , eyeStreakPaths : List String
    , nosePath : String
    , armPaths : List String
    , sweaterPaths : List String
    , shoulderFillPaths : List String
    , shoulderFillPoints : List String
    , eyeEllipses : List (Svg.Svg Never)
    , irisEllipses : List (Svg.Svg Never)
    , eyelidPaths : List String
    , mouthPath : String
    , eyebrowPaths : List String
    , pointerPath : String
    , pointerMagicPaths : List String
    , fingerPaths : List String
    , accentPaths : List String
    , jewel1Ellipse : Svg.Svg Never
    , jewel2Points : String
    , jewel3Points : String
    , cuffPaths : List String
    }
    -> Nri.Ui.Svg.V1.Svg
renderFullSal config =
    let
        stroke =
            { dark = Css.property "stroke" "#231F20"
            , black = Css.property "stroke" "#000000"
            , light = Css.property "stroke-width" "1.5"
            , medium = Css.property "stroke-width" "1.87"
            , heavy = Css.property "stroke-width" "2.62"
            }

        fill =
            { platform = Css.fill (Css.hex "9E9EA9")
            , body = Css.fill (Css.hex "B298A2")
            , sock = Css.fill (Css.hex "F5C377")
            , shoe = Css.fill (Css.hex "D33353")
            , white = Css.fill (Css.hex "FFFFFF")
            , none = Css.property "fill" "none"
            , skirt = Css.fill (Css.hex "D88B29")
            , face = Css.fill (Css.hex "E7DDE1")
            , streak = Css.fill (Css.hex "6F5861")
            , nose = Css.fill (Css.hex "352C31")
            , sweater = Css.fill (Css.hex "FDF8DD")
            , iris = Css.fill (Css.hex "231F20")
            , eyebrow = Css.fill (Css.hex "5B4951")
            , pointer = Css.fill (Css.hex "06CBBA")
            , jewel3 = Css.fill (Css.hex "B1263F")
            }
    in
    Nri.Ui.Svg.V1.init "0 0 480 600"
        [ Svg.defs []
            [ Svg.g
                [ id (config.name ++ "_legs"), css [ stroke.heavy ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.legPaths)
            , Svg.path
                [ id (config.name ++ "_face"), css [ stroke.light ], d config.facePath ]
                []
            ]
        , Svg.g
            [ css
                [ Css.color (Css.hex "231F20")
                , Css.property "stroke-miterlimit" "10"
                ]
            ]
            [ Svg.g
                [ css [ fill.platform, Css.opacity (Css.num 0.35), Css.property "enable-background" "new" ] ]
                [ config.platformEllipse
                ]
            , Svg.use [ xlinkHref ("#" ++ config.name ++ "_legs"), css [ fill.body, stroke.dark ] ] []
            , Svg.mask [ id (config.name ++ "_legMask") ]
                [ Svg.use [ xlinkHref ("#" ++ config.name ++ "_legs"), maskStyle ] []
                ]
            , Svg.g [ css [ Css.property "mask" ("url(#" ++ config.name ++ "_legMask" ++ ")") ] ]
                [ Svg.g
                    [ css [ fill.sock, stroke.black, stroke.light ]
                    ]
                    (List.map (\path -> Svg.path [ d path ] []) config.sockPaths)
                , Svg.g
                    [ css [ fill.shoe, stroke.dark, stroke.medium ]
                    ]
                    (List.map (\path -> Svg.path [ d path ] []) config.shoePaths)
                , Svg.g
                    [ css [ fill.white, stroke.black, stroke.light ] ]
                    (List.map (\path -> Svg.path [ d path ] []) config.shoeStripPaths)
                , Svg.g
                    [ css [ fill.none, stroke.black, stroke.heavy ] ]
                    (List.map (\path -> Svg.path [ d path ] []) config.shoeLacePaths)
                ]
            , Svg.path [ d config.skirtPath, css [ fill.skirt, stroke.dark, stroke.heavy ] ] []
            , Svg.g
                [ css [ fill.body, stroke.dark, stroke.heavy ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.headPaths)
            , Svg.use [ xlinkHref ("#" ++ config.name ++ "_face"), css [ fill.face, stroke.dark ] ] []
            , Svg.mask [ id (config.name ++ "_faceMask") ]
                [ Svg.use [ xlinkHref ("#" ++ config.name ++ "_face"), maskStyle ] []
                ]
            , Svg.g
                [ css [ fill.streak, stroke.dark, stroke.light, Css.property "mask" ("url(#" ++ config.name ++ "_faceMask" ++ ")") ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.eyeStreakPaths)
            , Svg.path [ d config.nosePath, css [ fill.nose, stroke.dark, stroke.medium ] ] []
            , Svg.g
                [ css [ fill.body, stroke.dark, stroke.heavy ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.armPaths)
            , Svg.g
                [ css [ fill.sweater, stroke.dark, stroke.heavy ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.sweaterPaths)
            , Svg.g
                [ css [ fill.sweater ]
                ]
                (List.map (\path -> Svg.path [ d path ] []) config.shoulderFillPaths
                    ++ List.map (\pts -> Svg.polygon [ points pts ] []) config.shoulderFillPoints
                )
            , Svg.g
                [ css [ fill.white, stroke.dark, stroke.heavy ] ]
                config.eyeEllipses
            , Svg.g
                [ css [ fill.iris ] ]
                config.irisEllipses
            , Svg.g
                [ css [ fill.streak, stroke.dark, stroke.heavy ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.eyelidPaths)
            , Svg.path [ d config.mouthPath, css [ fill.iris ] ] []
            , Svg.g
                [ css [ fill.eyebrow, stroke.dark, stroke.heavy ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.eyebrowPaths)
            , Svg.path [ d config.pointerPath, css [ fill.pointer, stroke.black, stroke.medium ] ] []
            , Svg.g
                [ css [ fill.iris ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.pointerMagicPaths)
            , Svg.g
                [ css [ fill.iris ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.accentPaths)
            , Svg.g
                [ css [ fill.sock, stroke.dark, stroke.medium ] ]
                [ config.jewel1Ellipse
                ]
            , Svg.polygon [ points config.jewel2Points, css [ fill.shoe, stroke.dark, stroke.medium ] ] []
            , Svg.polygon [ points config.jewel3Points, css [ fill.jewel3, stroke.dark, stroke.medium ] ] []
            , Svg.g
                [ css [ fill.none, stroke.dark, stroke.medium ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.cuffPaths)
            , Svg.g
                [ css [ fill.streak, stroke.dark, stroke.heavy ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.fingerPaths)
            ]
        ]


{-| Full height Sal to be shown when presenting a question.
-}
salInstructive : Nri.Ui.Svg.V1.Svg
salInstructive =
    renderFullSal
        { name = "salInstructive"
        , platformEllipse = Svg.ellipse [ cx "219.6", cy "572.4", rx "92.5", ry "17.4" ] []
        , legPaths =
            [ "M184.8,528c0.4,10.9,0.8,19.3,0.8,19.3s-15,9.7-16.5,15.9c-1.7,6.8,9.2,16.3,29,8.1c19.2-8,17.3-12.5,17.3-21.6c0-9,0-54.8,0-54.8l-30.2,4.2C185.2,499.2,184.4,517.2,184.8,528L184.8,528z"
            , "M261.3,525.7c0.5,10.9,0.9,19.3,0.9,19.3s15.7,8.3,17.8,14.4c2.3,6.6-7.8,17-28.2,10.5c-19.8-6.3-18.3-11-19.1-20c-0.8-9-4.7-54.6-4.7-54.6l30.4,1.6C258.5,497,260.8,514.8,261.3,525.7L261.3,525.7z"
            ]
        , sockPaths =
            [ "M195.7,547.1c0,0,6.1,0.8,12,0.6s9.7-1.8,9.7-1.8s0.5,11.6,0.3,12.1s-3.3,3.9-7.7,3.3C205.7,560.7,195.7,547.1,195.7,547.1L195.7,547.1z"
            , "M252.2,545.6c0,0-5.9,1.3-11.9,1.6c-5.7,0.3-9.8-1-9.8-1s0.5,11.6,0.7,12.1s3.6,3.6,7.9,2.6S252.2,545.6,252.2,545.6z"
            ]
        , shoePaths =
            [ "M221,559.3c0,0-6.7-6-13.4-7.5c-4.1-0.9-8.2-1.1-9.4-0.8c-0.4,0.1-0.8-0.3-0.7-0.7c0.6-2.1,1.6-6.5-0.4-7.4c-2.6-1.2-16.1,0.5-16.1,0.5v3.3c0,0-19.4,8.2-19.4,14.5s0.6,21,18.4,21.4S221,559.4,221,559.3L221,559.3z"
            , "M228,560c0,0,6.1-6.6,12.7-8.6c4-1.2,8.1-1.8,9.2-1.7c0.4,0.1,0.8-0.4,0.6-0.8c-0.8-2-2.2-6.3-0.3-7.5c2.5-1.5,16.1-0.8,16.1-0.8l0.3,3.3c0,0,20,6.5,20.6,12.8c0.5,6.3,1.2,21-16.5,22.9S228,560,228,560L228,560z"
            ]
        , shoeLacePaths =
            [ "M179.2,551.6c0.1,0.2,5.3-1.1,10.3,4.1"
            , "M269,548.7c-0.1,0.2-4.9-0.2-9.7,5.5"
            ]
        , shoeStripPaths =
            [ "M168.4,554.7c0,0,7.6-1.5,13,3.8c5.4,5.3,10.9,17.5,10.9,17.5s-8,9.4-15.9,6.3c-7.9-3.1-20.1-10.2-17.9-17.3C160.6,557.9,168.4,554.7,168.4,554.7L168.4,554.7z"
            , "M166.4,565.9c0,0,5.8,3.3,15.6,2.9c9.9-0.4,23.5-6.3,28.5-10s5.4-6.1,5.4-6.1l3.1,1.3c0,0,2.8,3.6-0.5,9.4c-3.3,5.8-12.1,11.8-22.3,12.9s-23.7,4.3-28.1,0.6C163.7,573.3,166.4,565.9,166.4,565.9L166.4,565.9z"
            , "M280,550.8c0,0-7.7-0.8-12.6,4.9c-4.9,5.7-9.3,18.4-9.3,18.4s8.8,8.7,16.4,4.9s19.1-11.9,16.3-18.8S280,550.8,280,550.8L280,550.8z"
            , "M283,561.8c0,0-5.5,3.8-15.3,4.2s-24-4.3-29.2-7.5c-5.3-3.2-5.9-5.6-5.9-5.6l-3,1.5c0,0-2.5,3.8,1.3,9.3c3.8,5.5,13.1,10.7,23.3,10.9c10.2,0.2,24,2.2,28.1-1.8S283,561.8,283,561.8L283,561.8z"
            ]
        , skirtPath = "M158.8,527.3c0,0,5.8-17,8-26.5c2-8.6,5.6-28,5.6-28s18.9,5.3,46.7,3.8c26.7-1.4,45.9-7.8,45.9-7.8s1.3,15.1,4.4,25.8c3.1,10.7,10.3,25.4,10.3,25.4s-18.6,11.7-60.5,12.7C177.9,533.7,158.8,527.3,158.8,527.3z"
        , headPaths =
            [ "M182,382.7l2.1-15.5c0,0-23.5-2.7-35.7-22.1c-12.2-19.4-9.2-46.2,10.3-66.8c19.5-20.5,46.7-34,89.5-22.2s46.1,53,45.7,61.2c-0.4,9.2-3.3,28.7-21.4,39.6c-17.5,10.6-27.3,11.2-27.3,11.2l1.8,8.2c0,0-2,14.1-31.5,17.4C192.9,396.1,182,382.7,182,382.7L182,382.7z"
            , "M193.5,257.2c0,0-8-2.2-7-14.6c1.1-13.9,15.3-17.3,15.3-17.3s-0.9,5.4,4.3,12.3c7.2,9.5,30.6,14,42.3,18.4"
            ]
        , facePath = "M270.6,307.4c3,12.4,3.8,26.1-5.1,37.2c-8.6,10.7-23.5,17.5-52.9,15.6c-32.5-2-47.2-11.5-56.8-24.7c-5.4-7.3-3.7-21.1-1.5-27.7c7.4-23,27.8-42,59.7-42S265.1,283.9,270.6,307.4L270.6,307.4z"
        , eyeStreakPaths =
            [ "M240.1,289.8c0,0,10.8,1.2,20.8,8.5c9.9,7.3,13.8,15.6,13.8,15.6l-0.6,15.5c0,0-18.9-6.5-28.5-6.7c-9.6-0.2-10.5-0.4-10.5-0.4L240.1,289.8L240.1,289.8z"
            , "M184.6,284.7c0,0-14.6,2.9-24.5,10.2c-9.9,7.3-14.5,15.5-14.5,15.5l2.6,13c0,0,8.1-7.8,17.8-8.6c5.3-0.5,12.9-0.1,12.9-0.1L184.6,284.7L184.6,284.7z"
            ]
        , nosePath = "M191.4,325.1c-0.7-0.5-1.1-1.3-0.9-2.1c0.7-3.6,3.8-13.3,15.1-12.5c10.9,0.8,14.6,10.1,15.7,14.1c0.3,1-0.2,2.1-1.2,2.5c-2.9,1.2-9.5,3.3-16.9,2C196.5,327.9,193,326.1,191.4,325.1L191.4,325.1z"
        , armPaths =
            [ "M63.6,349c0,0-8.4-26-8-35.8c0.4-9.9,4-11.9,8-11.1s15.6,35.6,15.6,35.6L63.6,349z"
            , "M308.1,454.7c0,0-27.3,1.5-36.7-1.4c-9.4-2.9-10.4-6.9-8.6-10.6c1.8-3.7,38.4-6,38.4-6L308.1,454.7z"
            ]
        , sweaterPaths =
            [ "M166.6,434.1c0,0-46.2,3.2-69.3-16.3S62,355.1,62,355.1s-7.2-7-3.5-8.2c9.2-2.9,13.7-5.5,20.2-11.9c4.1-4,5.4,7.9,5.4,7.9s14.1,29.2,46.4,41.6c32.3,12.5,48.3,4.2,48.3,4.2"
            , "M250.8,385.3c0,0,20.4-7.4,51.4,2.9c39.5,13.2,52.4,39,50.1,47.7c0,0-3,14.4-21.5,8.5c-16.3-5.2-23.6-12.1-37.4-15.5c-11.8-3-20.3-3.7-20.3-3.7"
            , "M172.3,472.8c0,0-10.1-6.7-11.9-24.5c-3.2-32.6,19.8-58.9,19.8-58.9s-3.9-8.5-1.1-10.3c3.5-2.4,11.9,4.7,35.2,3.7s34-8.1,36.4-7.5c3.9,1,0,10,0,10s24.7,19.5,26,49c1.4,31.3-12.3,34.5-12.3,34.5s-19,8-45.3,10C194.2,480.7,172.3,472.8,172.3,472.8L172.3,472.8z"
            , "M329.1,425.9c0,0-12.3,0.1-21.4,3.1c-3.9,1.3-11.2,5-14.7,4c-5.7-1.6-8.4,1.4-7.8,4.5c0.5,2.5,2.1,7.7,2.1,10.5c0,6.3-0.7,8.8,2.7,10.7s9.5-1.5,9.5-1.5s7.1,0.8,23.4-2.1s29.5-10.7,29.5-19.2"
            ]
        , shoulderFillPaths = []
        , shoulderFillPoints =
            [ "248.8,385.4 258.7,385.4 279.5,402.7 279.5,415.6 253,415.6"
            , "160.1,393.9 182.6,390.5 184.6,427.2 147.2,420.4 "
            ]
        , eyeEllipses =
            [ Svg.ellipse [ transform "matrix(0.2565 -0.9666 0.9666 0.2565 -118.7166 459.3136)", cx "239.2", cy "306.8", rx "16", ry "12.3" ] []
            , Svg.ellipse [ transform "matrix(0.999 -4.431684e-02 4.431684e-02 0.999 -13.1116 8.3431)", cx "181.6", cy "299.9", rx "12.3", ry "16" ] []
            ]
        , irisEllipses =
            [ Svg.ellipse [ transform "matrix(0.2134 -0.977 0.977 0.2134 -112.2264 470.4945)", cx "236.1", cy "304.9", rx "7.3", ry "5.3" ] []
            , Svg.ellipse [ cx "185.1", cy "298.7", rx "5.3", ry "7.3" ] []
            ]
        , eyelidPaths =
            [ "M228.4,300.6c0,0,3.6-1.8,11.6,0.2c8.4,2.1,11.8,6,11.8,6s1.9-8.1-2.9-13c-4.1-4.3-9.7-5.3-14.6-2.1C230.2,294.5,228.4,300.6,228.4,300.6L228.4,300.6z"
            , "M193.5,296.1c0,0-3.2-2.5-11.4-2.3c-8.6,0.2-12.9,3.3-12.9,3.3s-0.1-8.3,5.6-12.1c4.9-3.3,10.6-3.1,14.7,1.1C193,289.8,193.5,296.1,193.5,296.1z"
            ]
        , mouthPath = "M196.5,334.1c11.2,4.4,22.8,1.3,32.7-4.7C222.4,339.5,205.6,343.5,196.5,334.1L196.5,334.1z"
        , eyebrowPaths =
            [ "M254.3,279.8c0,0-8.8-4.4-14.2-5.9s-9.6-2.5-7.5-6.4c2.1-4,9.8-3.9,14.4,0S254.3,279.8,254.3,279.8z"
            , "M178.1,275.9c0,0,9.3-2.9,14.9-3.6c5.6-0.7,9.8-0.9,8.4-5.2s-9-5.4-14.2-2.3S178.1,275.9,178.1,275.9z"
            ]
        , pointerPath = "M37.9,206.2l27.2,106.2l8.3-0.5L40.6,204.8c0,0-0.2-0.2-0.5-0.4C38.9,203.8,37.5,204.9,37.9,206.2L37.9,206.2z"
        , pointerMagicPaths =
            [ "M36.2,192.7c-1.9-5.7-3.9-11.5-5.8-17.2c-0.3-0.8-0.5-1.6-0.8-2.4c-0.6-1.7-3.3-1-2.7,0.8c1.9,5.7,3.9,11.5,5.8,17.2c0.3,0.8,0.5,1.6,0.8,2.4C34.1,195.1,36.8,194.4,36.2,192.7L36.2,192.7z"
            , "M28.9,197.2c-3.3-2.2-6.5-4.4-9.8-6.7c-0.5-0.3-0.9-0.6-1.4-0.9c-0.6-0.4-1.6-0.1-1.9,0.5c-0.4,0.7-0.1,1.5,0.5,1.9c3.3,2.2,6.5,4.4,9.8,6.7c0.5,0.3,0.9,0.6,1.4,0.9c0.6,0.4,1.6,0.1,1.9-0.5C29.8,198.4,29.5,197.6,28.9,197.2L28.9,197.2z"
            , "M45.2,193.4c1-2.9,2.1-5.7,3.1-8.6l0.5-1.2c0.3-0.7-0.3-1.5-1-1.7c-0.8-0.2-1.5,0.3-1.7,1c-1,2.9-2.1,5.7-3.1,8.6l-0.5,1.2c-0.3,0.7,0.3,1.5,1,1.7C44.3,194.6,45,194.1,45.2,193.4L45.2,193.4z"
            ]
        , fingerPaths =
            [ "M83.5,289.4c1.3-0.4,1.2-2.2-0.2-2.4c-6.3-1-17-0.6-22.2,1.5c-7.8,3.2-9.7,10.1-7.3,14.6c1.3,2.4,0-1.7,11.2-6.8C71.5,293.4,79.3,290.6,83.5,289.4L83.5,289.4z"
            , "M82.8,310.8c1.2,0.4,2.2-1.1,1.3-2.1c-4.6-4.4-13.6-10.2-19-11.5c-8.2-1.9-13.7,2.7-14.3,7.8c-0.3,2.7,1-1.4,13.1,0.9C70.7,307.1,82.8,310.8,82.8,310.8z"
            , "M81.3,314c1.3,0.1,1.9-1.6,0.8-2.3c-5.4-3.4-14.4-8.3-20-8.4c-8.4,0-12.6,3.4-12.6,7.1c0,2.7,1.6,3.1,13.9,2.7C70.4,313,81.3,314,81.3,314z"
            ]
        , accentPaths =
            [ "M178.1,384.8c1.4,20,8.7,50.7,35.1,43.3c20.1-6.1,33.4-31.2,41.1-49.5c-5.9,19.9-19.1,45.8-40.3,52.3C185.4,438.6,177.1,406.9,178.1,384.8L178.1,384.8L178.1,384.8z"
            , "M199.7,226.6c-1.4,12,5.7,22.6,15.1,29.4C203.5,252.1,194.7,238.3,199.7,226.6L199.7,226.6z"
            , "M161.1,429.2c-0.6-4.6,0.6-9.6,3.7-13.1C165.1,420.8,163.9,425.5,161.1,429.2L161.1,429.2z"
            , "M275.9,425.9c-3.5-5.1-4.7-11-6.3-16.8C274,412.4,276.1,420.5,275.9,425.9L275.9,425.9z"
            ]
        , jewel1Ellipse = Svg.ellipse [ transform "matrix(0.2565 -0.9666 0.9666 0.2565 -268.3343 508.7718)", cx "196.5", cy "428.8", rx "6.2", ry "5.2" ] []
        , jewel2Points = "205.9,426.7 203.2,442.7 216.2,442.7 213.4,425.9 "
        , jewel3Points = "217.6,425.1 221.6,434.3 229.8,429.7 224.3,421.3 "
        , cuffPaths =
            [ "M178.8,388c0,0,9.1,7.8,30.9,7.4c21.8-0.5,41-10.1,41-10.1"
            , "M163.6,461.1c0,0,12.6,10.1,52.1,8.8c34.1-1.1,58.9-14.3,58.9-14.3"
            , "M61.1,355.8c0,0,4.6,1.1,11.9-3c7.3-4.1,12.1-10.9,12.1-10.9"
            , "M296.7,458.2c0,0,3-3.4,2.4-11.3s-4.6-14.7-4.6-14.7"
            ]
        }


{-| Full height Sal to be shown on a wrong answer.
-}
salSupportive : Nri.Ui.Svg.V1.Svg
salSupportive =
    renderFullSal
        { name = "salSupportive"
        , platformEllipse = Svg.ellipse [ cx "220.2", cy "571.8", rx "92.4", ry "17.4" ] []
        , legPaths =
            [ "M185.5,527.5c0.4,10.9,0.8,19.3,0.8,19.3s-14.9,9.7-16.5,15.9c-1.7,6.8,9.2,16.3,29,8c19.1-8,17.3-12.5,17.3-21.5s0-54.7,0-54.7l-30.2,4.2C185.9,498.7,185.1,516.6,185.5,527.5L185.5,527.5z"
            , "M261.9,525.1c0.5,10.9,0.9,19.3,0.9,19.3s15.7,8.3,17.8,14.4c2.3,6.6-7.8,17-28.2,10.5c-19.8-6.3-18.3-11-19.1-20c-0.8-9-4.7-54.5-4.7-54.5l30.4,1.6C259,496.5,261.4,514.3,261.9,525.1L261.9,525.1z"
            ]
        , sockPaths =
            [ "M196.3,546.5c0,0,6,0.8,12,0.6c5.7-0.2,9.7-1.8,9.7-1.8s0.5,11.6,0.3,12.1s-3.3,3.9-7.7,3.3S196.3,546.5,196.3,546.5L196.3,546.5z"
            , "M252.7,545c0,0-5.9,1.3-11.9,1.6s-9.8-1-9.8-1s0.5,11.6,0.7,12.1s3.6,3.6,7.9,2.6S252.7,545,252.7,545L252.7,545z"
            ]
        , shoePaths =
            [ "M221.6,558.7c0,0-6.7-6-13.4-7.5c-4.1-0.9-8.2-1.1-9.3-0.8c-0.4,0.1-0.8-0.3-0.7-0.7c0.6-2.1,1.6-6.5-0.4-7.4c-2.6-1.2-16.1,0.5-16.1,0.5v3.3c0,0-19.4,8.2-19.4,14.5s0.6,21,18.4,21.4S221.6,558.8,221.6,558.7L221.6,558.7z"
            , "M228.6,559.4c0,0,6.1-6.6,12.7-8.6c4-1.2,8.1-1.8,9.2-1.7c0.4,0.1,0.8-0.4,0.6-0.8c-0.8-2-2.2-6.3-0.3-7.4c2.5-1.5,16.1-0.8,16.1-0.8l0.3,3.3c0,0,20,6.5,20.5,12.8s1.2,21-16.5,22.9S228.6,559.4,228.6,559.4L228.6,559.4z"
            ]
        , shoeLacePaths =
            [ "M179.8,551c0.1,0.2,5.3-1.1,10.3,4"
            , "M269.6,548.1c-0.1,0.2-4.9-0.2-9.7,5.5"
            ]
        , shoeStripPaths =
            [ "M169.1,554.1c0,0,7.6-1.5,12.9,3.8c5.4,5.3,10.8,17.5,10.8,17.5s-8,9.4-15.9,6.3s-20.1-10.2-17.9-17.3C161.3,557.3,169.1,554.1,169.1,554.1z"
            , "M167.1,565.3c0,0,5.7,3.3,15.6,2.9c9.8-0.4,23.5-6.3,28.4-9.9s5.4-6.1,5.4-6.1l3.1,1.3c0,0,2.8,3.5-0.5,9.4c-3.3,5.8-12.1,11.8-22.2,12.8c-10.1,1.1-23.7,4.3-28.1,0.6C164.4,572.7,167.1,565.3,167.1,565.3L167.1,565.3z"
            , "M280.5,550.2c0,0-7.7-0.8-12.6,4.9c-4.9,5.7-9.3,18.4-9.3,18.4s8.8,8.7,16.3,4.9c7.5-3.8,19.1-11.9,16.3-18.8C288.5,552.7,280.5,550.2,280.5,550.2L280.5,550.2z"
            , "M283.5,561.2c0,0-5.4,3.8-15.3,4.2s-23.9-4.3-29.2-7.5c-5.3-3.2-5.9-5.6-5.9-5.6l-3,1.5c0,0-2.5,3.8,1.3,9.3c3.8,5.5,13.1,10.7,23.3,10.9c10.2,0.2,24,2.2,28-1.8S283.5,561.2,283.5,561.2L283.5,561.2z"
            ]
        , skirtPath = "M159.5,526.7c0,0,5.8-17,7.9-26.4c2-8.6,5.6-28,5.6-28s18.9,5.3,46.6,3.8c26.7-1.4,45.8-7.8,45.8-7.8s1.3,15.1,4.4,25.8c3.1,10.7,10.3,25.4,10.3,25.4s-18.5,11.6-60.4,12.7C178.6,533.1,159.5,526.7,159.5,526.7L159.5,526.7z"
        , headPaths =
            [ "M182.7,382.3l2.1-15.5c0,0-23.5-2.7-35.6-22.1c-12.1-19.4-9.2-46.2,10.3-66.7s46.7-33.9,89.4-22.2c42.7,11.7,46,52.9,45.7,61.1c-0.4,9.2-3.3,28.6-21.3,39.6C255.8,367,246,367.6,246,367.6l1.8,8.2c0,0-2,14.1-31.5,17.4C193.6,395.7,182.7,382.3,182.7,382.3L182.7,382.3z"
            , "M194.1,257c0,0-8-2.2-7-14.5c1.1-13.9,15.2-17.3,15.2-17.3s-0.9,5.4,4.3,12.2c7.2,9.5,30.6,14,42.2,18.4"
            ]
        , facePath = "M271.2,307.1c3,12.4,3.8,26.1-5.1,37.1c-8.6,10.7-23.4,17.4-52.8,15.6c-32.5-2-47.1-11.5-56.8-24.6c-5.4-7.3-3.7-21-1.5-27.7c7.4-23,27.8-41.9,59.7-41.9S265.6,283.6,271.2,307.1z"
        , eyeStreakPaths =
            [ "M240.6,290.8c0,0,11-0.1,20.9,7.2c9.9,7.3,13.8,15.6,13.8,15.6l-0.6,15.5c0,0-18.9-6.5-28.5-6.7s-10.4-0.4-10.4-0.4L240.6,290.8L240.6,290.8z"
            , "M185.3,284.4c0,0-14.6,2.9-24.5,10.2c-9.9,7.3-14.5,15.5-14.5,15.5l2.5,13c0,0,8.1-7.8,17.7-8.6c5.3-0.5,12.9-0.1,12.9-0.1L185.3,284.4L185.3,284.4z"
            ]
        , nosePath = "M192.9,324.7c-0.7-0.5-1.1-1.3-0.9-2.1c0.7-3.6,3.8-13.3,15.1-12.5c10.9,0.8,14.6,10.1,15.7,14c0.3,1-0.2,2.1-1.2,2.5c-2.9,1.2-9.5,3.3-16.8,2c-6.7-1.1-9.3-2.3-10.9-3.4L192.9,324.7z"
        , armPaths =
            [ "M111.3,367.7c0,0,11.8-17.7,39-24.1c6.7-1.6,6.6,6.5,5.9,10.8c-1.6,9.4-9.9,2.7-22.3,15.8c-6.3,6.7-11.7,8.1-11.7,8.1L111.3,367.7L111.3,367.7z"
            , "M313.9,469.1c0,0-25,9.5-34.7,9.6c-9.7,0.1-11.8-3.4-11.2-7.3c0.6-4,34.2-17.1,34.2-17.1L313.9,469.1L313.9,469.1z"
            ]
        , sweaterPaths =
            [ "M173.2,428.4c0,0-84.3,37.1-98.1,9.3c-14.6-29.5,38.7-76.6,38.7-76.6s1.9-8.5,5.7-7.1c6.2,2.3,13.9,11.6,15.1,16.1c1.1,4-2.3,3.6-5.6,5.9s-7.3,8.1-10.5,16.5c-5.6,15-8.2,16.8-6,19.5c2.5,3.1,36.4-21.3,56.1-23.4c23.1-2.4,32.3,0,32.3,0"
            , "M252.3,381.9c0,0,31.5-6.2,61,7.9c37.5,18,47,45.2,43.7,53.6c0,0-4.8,13.9-22.3,5.7c-15.5-7.2-21.8-14.9-35-20.1c-11.4-4.4-32.9-8.2-32.9-8.2"
            , "M173,472.3c0,0-10.1-6.7-11.9-24.4c-3.2-32.6,19.8-58.9,19.8-58.9s-3.9-8.4-1.1-10.3c3.5-2.4,11.9,4.7,35.2,3.7s34-8.1,36.4-7.5c3.9,1,0,10,0,10s24.7,19.5,26,48.9c1.4,31.3-12.3,34.5-12.3,34.5s-18.9,8-45.3,10C194.9,480.1,173,472.2,173,472.3L173,472.3z"
            , "M332.2,433.8c0,0-11.8,3.5-19.7,9.1c-3.3,2.3-9.3,7.9-12.9,8c-5.9,0.1-7.6,3.7-6.2,6.6c1.1,2.2,4.2,6.8,5,9.5c1.8,6.1,1.9,8.6,5.6,9.5s8.7-4.2,8.7-4.2s7-1.2,21.8-8.7s25.2-18.5,22.8-26.7"
            ]
        , shoulderFillPaths =
            [ "M167.4,393.5c0,0,3.2-1.4,9.7-3s6.1-0.4,6.1-0.4l2.1,36.6l-37.4-6.8L167.4,393.5L167.4,393.5z" ]
        , shoulderFillPoints = [ "249.4,385 259.3,385 280,402.2 280,415.1 253.6,415.1 " ]
        , eyeEllipses =
            [ Svg.ellipse [ transform "matrix(0.2565 -0.9666 0.9666 0.2565 -117.9869 459.6626)", cx "239.8", cy "306.5", rx "16", ry "12.3" ] []
            , Svg.ellipse [ transform "matrix(0.9999 -1.204248e-02 1.204248e-02 0.9999 -3.5928 2.2433)", cx "184.5", cy "299.5", rx "12.3", ry "16" ] []
            ]
        , irisEllipses =
            [ Svg.ellipse [ transform "matrix(0.2009 -0.9796 0.9796 0.2009 -110.6234 476.4557)", cx "236.7", cy "306", rx "8.1", ry "6.1" ] []
            , Svg.ellipse [ cx "187.3", cy "299.5", rx "6.1", ry "8.1" ] []
            ]
        , eyelidPaths =
            [ "M232,295.9c-0.7,0.4,0.1,0.6,0.3,0.4c1.6-0.8,4.8-1.7,9.8-0.5c6.8,1.7,9,6.4,9.6,8.1c0,0.1,0.2,0.1,0.2,0c0.1-1.7-0.1-6.5-3.1-9.8c-4.1-4.3-10.7-3.9-13.4-1.5C233.8,294,232.7,295.2,232,295.9L232,295.9z"
            , "M194.6,291.1c0.1,0.3-0.2,0.5-0.4,0.3c-1.4-1.2-4.2-2.8-9.4-2.9c-7,0-10.3,4-11.2,5.5c-0.1,0.1-0.2,0-0.2-0.1c0.4-1.7,1.6-6.3,5.5-8.8c5-3.2,11.3-1.1,13.3,1.9C193.4,288.8,194.1,290.1,194.6,291.1L194.6,291.1z"
            ]
        , mouthPath = "M198.9,332.3c4.2,1.7,8,3,12.5,2.3c1.5-0.1,2.9-0.4,4.5-0.7C211.4,338.7,202,338.7,198.9,332.3L198.9,332.3L198.9,332.3z"
        , eyebrowPaths =
            [ "M255.2,280.1c0,0-10.2-5.1-14.7-7c-5.2-2.1-9.1-3.8-6.5-7.4s10.2-2.5,14.3,2c3.2,3.4,6.9,11.8,6.9,11.8V280.1z"
            , "M178.9,275c0,0,8.8-3,13.7-4.3s8.7-2.1,6.8-5.7s-8.8-3.6-13-0.2C182.2,268.2,178.9,275,178.9,275L178.9,275z"
            ]
        , pointerPath = ""
        , pointerMagicPaths = []
        , fingerPaths =
            [ "M180.1,339.3c1.3-0.4,1.2-2-0.1-2.2c-6.2-0.6-16.7,0.2-21.8,2.3c-7.8,3.2-9.9,9.5-7.8,13.4c1.2,2.1,0.1-1.5,11.3-6.6C168.1,343.4,175.9,340.6,180.1,339.3L180.1,339.3z"
            , "M161.5,364.7c1.5,0.2,13.1-3.3,13.7-8.5c0.8-6.3-6.6-9.1-13.1-10c-9.8-1.2-14.2,2.1-13.1,6.1c0.8,3.1,2.4,3.6,15.2,2.7C174.4,354.4,156.3,364.2,161.5,364.7L161.5,364.7z"
            , "M158.3,368.4c1.3,0.1,11.1-3,11.7-7.7c0.7-5.7-5.6-8.3-11.1-9c-8.4-1.1-12,1.9-11.1,5.6c0.7,2.8,2,3.3,12.9,2.5C169.3,359,153.9,367.9,158.3,368.4L158.3,368.4z"
            , "M261.8,445.7c-0.6-0.9,0.6-2,1.6-1.4c4.6,2.9,11.1,9.2,13,13.4c2.9,6.3-0.1,11.6-4.4,13c-2.3,0.8,1.1-1-2.9-10.5C266.9,454.9,263.7,448.8,261.8,445.7L261.8,445.7z"
            , "M248,460.4c-1.2-0.3-1.1-1.9,0.1-2.1c5.7-1.2,15.3-1.4,20.1,0.2c7.1,2.4,9,8.4,7,12.5c-1.1,2.2,0-1.5-10.3-5.5C258.9,463.3,251.8,461.3,248,460.4L248,460.4z"
            , "M248.5,464.3c-0.9-0.7-0.3-1.5,0.8-1.8c5.9-1.4,15-0.5,20,2.6c6.1,3.8,4.9,9.7,1.1,11.7c-2,1-0.8-2.4-8.1-7.2C257.4,466.3,251.6,466.6,248.5,464.3L248.5,464.3z"
            ]
        , accentPaths =
            [ "M178.8,384.4c1.4,20,8.7,50.6,35,43.3c20.1-6.1,33.3-31.2,41.1-49.4c-5.9,19.9-19,45.8-40.2,52.3C186,438.1,177.8,406.5,178.8,384.4L178.8,384.4L178.8,384.4z"
            , "M200.3,226.4c-1.4,12,5.7,22.6,15.1,29.4C204.1,251.8,195.4,238,200.3,226.4L200.3,226.4z"
            , "M161.8,428.8c-0.6-4.6,0.6-9.5,3.7-13.1C165.8,420.3,164.6,425,161.8,428.8L161.8,428.8z"
            , "M276.4,425.5c-3.5-5.1-4.7-11-6.3-16.8C274.6,412,276.7,420.1,276.4,425.5L276.4,425.5z"
            ]
        , jewel1Ellipse = Svg.ellipse [ transform "matrix(0.2565 -0.9666 0.9666 0.2565 -267.4112 509.0645)", cx "197.2", cy "428.3", rx "6.2", ry "5.2" ] []
        , jewel2Points = "206.5,426.3 203.9,442.2 216.9,442.2 214,425.5"
        , jewel3Points = "218.3,424.7 222.2,433.8 230.4,429.2 224.9,420.8"
        , cuffPaths =
            [ "M179.5,387.6c0,0,9.1,7.8,30.9,7.4c21.8-0.5,41-10,41-10"
            , "M164.3,460.6c0,0,12.6,10,52.1,8.8c34.1-1.1,58.9-14.2,58.9-14.2"
            , "M113.8,361c0,0-0.1,1.6,5,6.5c5,4.9,10.5,8.2,10.5,8.2"
            , "M310.3,473.9c0,0,1.9-4.1-0.9-11.5c-2.7-7.4-8.6-12.8-8.6-12.8"
            ]
        }


{-| Use this to create a mask that **excludes** the stroke (this is almost always what we want)
-}
maskStyle : Svg.Attribute msg
maskStyle =
    css
        [ Css.fill (Css.hex "FFFFFF")
        , Css.property "stroke" "#000000"
        ]


lindyPalette =
    { fill =
        { glasses = Css.fill (Css.hex "E70D4F")
        , body = Css.fill (Css.hex "9F9FAA")
        , head = Css.fill (Css.hex "9F9FA5")
        , pants = Css.fill (Css.hex "1E2840")
        , white = Css.fill (Css.hex "FFFFFF")
        , shirt = Css.fill (Css.hex "#95B7B7")
        , scarf = Css.fill (Css.hex "5578B5")
        , hoof = Css.fill (Css.hex "5B5B5B")
        , shoe = Css.fill (Css.hex "6E81B8")
        , pointer = Css.fill (Css.hex "#06CCBB")
        , brows = Css.fill (Css.hex "5B5B5B")
        , iris = Css.fill (Css.hex "030303")
        , line = Css.fill (Css.hex "231F20")
        , black = Css.fill (Css.hex "000000")
        , none = Css.property "fill" "none"
        }
    }


renderHeadshotFrame :
    { name : String
    , primaryPath : String
    , primaryColor : Css.Color
    , clippedSvg : Svg.Svg Never
    , secondaryPath : String
    , secondaryColor : Css.Color
    }
    -> Svg.Svg Never
renderHeadshotFrame config =
    Svg.g
        []
        [ Svg.defs
            []
            [ Svg.path [ id (config.name ++ "_circle"), d config.primaryPath ] []
            , Svg.path [ id (config.name ++ "_circleShadow"), d config.secondaryPath ] []
            ]
        , Svg.use [ xlinkHref ("#" ++ config.name ++ "_circle"), css [ Css.fill config.primaryColor ] ] []
        , Svg.clipPath
            [ id (config.name ++ "_circleClip") ]
            [ Svg.use [ xlinkHref ("#" ++ config.name ++ "_circle") ] [] ]
        , Svg.g [ css [ Css.property "clip-path" ("url(#" ++ config.name ++ "_circleClip)") ] ]
            [ config.clippedSvg
            , Svg.use [ css [ Css.fill config.secondaryColor ], xlinkHref ("#" ++ config.name ++ "_circleShadow") ] []
            ]
        ]


lindyHeadshot : Nri.Ui.Svg.V1.Svg
lindyHeadshot =
    let
        fill =
            lindyPalette.fill

        stroke =
            { thin = Css.batch [ Css.property "stroke" "#231F20", Css.property "stroke-width" "0.469025" ]
            , lightest = Css.batch [ Css.property "stroke" "#231F20", Css.property "stroke-width" "0.584675" ]
            , light = Css.batch [ Css.property "stroke" "#231F20", Css.property "stroke-width" "0.703538" ]
            , medium = Css.batch [ Css.property "stroke" "#231F20", Css.property "stroke-width" "0.819187" ]
            }
    in
    Nri.Ui.Svg.V1.init "0 0 60 103"
        [ Svg.g
            [ css
                [ Css.color (Css.hex "231F20")
                , Css.property "stroke-miterlimit" "10"
                ]
            ]
            [ renderHeadshotFrame
                { name = "lindyHeadshot"
                , primaryPath = "M0 72.0356C0 55.4671 13.4315 42.0356 30 42.0356V42.0356C46.5685 42.0356 60 55.4671 60 72.0356V72.0356C60 88.6042 46.5685 102.036 30 102.036V102.036C13.4315 102.036 0 88.6042 0 72.0356V72.0356Z"
                , primaryColor = Css.hex "EAF1F1"
                , clippedSvg =
                    -- Neck
                    Svg.path [ css [ fill.body, stroke.medium ], d "M33.8917 148.356C30.4543 148.26 6.03934 149.127 2.31284 133.193C-0.192913 122.592 5.75021 116.36 11.5327 107.3C16.8012 99.0764 20.0137 92.4587 20.7847 89.4389C21.7806 85.5197 24.5433 50.407 24.5433 50.407C24.5433 50.407 24.19 42.4079 32.9601 41.9903C41.3768 41.5727 43.2722 47.9334 43.5613 51.9812C43.8826 56.0289 44.2038 83.4636 47.9625 91.5591C51.7211 99.6546 53.0703 102.032 57.7606 108.585C62.4508 115.139 66.5307 118.737 66.338 127.796C65.9525 147.939 38.8711 148.517 33.8917 148.356Z" ] []
                , secondaryPath = "M0 42.0356H60H0ZM62.5 73.2856C62.5 90.5445 48.5089 104.536 31.25 104.536C13.9911 104.536 0 90.5445 0 73.2856V72.0356C0 87.2235 13.4315 99.5356 30 99.5356C45.1878 99.5356 57.5 87.2235 57.5 72.0356L62.5 73.2856ZM0 102.036V42.0356V102.036ZM31.25 42.0356C48.5089 42.0356 62.5 56.0267 62.5 73.2856C62.5 90.5445 48.5089 104.536 31.25 104.536L30 99.5356C45.1878 99.5356 57.5 87.2235 57.5 72.0356C57.5 55.4671 45.1878 42.0356 30 42.0356H31.25Z"
                , secondaryColor = Css.hex "95B7B7"
                }

            -- Head
            , Svg.path [ css [ fill.head, stroke.medium ], d "M40.7022 52.1421H26.2459C22.7764 52.1421 19.9815 49.3472 19.9815 45.8777L19.2748 32.4494C19.2748 28.9799 22.0697 26.1851 25.5392 26.1851H41.6338C45.1033 26.1851 47.8982 28.9799 47.8982 32.4494L46.9665 45.8777C46.9665 49.3151 44.1717 52.1421 40.7022 52.1421Z" ] []

            -- Neck Cover
            , Svg.path [ css [ fill.head ], d "M24.6718 50.0541V60.6874L41.955 61.7154L41.1519 47.6125L24.6718 50.0541Z" ] []

            -- Left Ear
            , Svg.path [ css [ fill.body, stroke.medium ], d "M37.4576 24.065C37.4576 24.065 36.9436 16.0338 34.5021 9.93001C32.0606 3.82626 26.9848 -1.05674 23.6438 1.15988C19.2748 4.05113 28.8481 10.6368 30.1973 14.299C31.5466 17.9613 32.0285 24.065 32.0285 24.065H37.4576Z" ] []
            , Svg.path [ css [ fill.black ], d "M27.3383 4.88599C30.101 7.29536 32.1249 10.54 33.3135 14.0416C34.4379 17.5432 34.9198 21.2055 35.2089 24.8677C34.47 21.3019 33.8918 17.7039 32.7674 14.2344C31.6109 10.797 29.7476 7.58449 27.3383 4.88599Z" ] []

            -- Right ear
            , Svg.path [ css [ fill.body, stroke.medium ], d "M43.4971 25.382C43.4971 25.382 44.1396 17.3507 46.5811 11.247C49.0226 5.14322 54.2268 0.0674664 57.4393 2.47684C61.9368 5.84997 52.2351 11.9537 50.8858 15.616C49.5366 19.2782 48.9262 25.382 48.9262 25.382H43.4971Z" ] []
            , Svg.path [ css [ fill.black ], d "M53.7449 6.20312C51.3355 8.90162 49.4723 12.1141 48.3158 15.5515C47.1914 19.021 46.6131 22.619 45.8743 26.1849C46.1634 22.5226 46.6453 18.8604 47.7696 15.3587C48.9583 11.8571 50.9821 8.6125 53.7449 6.20312Z" ] []

            -- Hair
            , Svg.path [ css [ fill.body ], d "M20.0138 30.1042C20.0138 30.1042 13.4924 36.3686 11.1794 32.3851C8.22392 27.2772 14.6489 25.8637 14.6489 25.8637C14.6489 25.8637 6.32854 22.9403 8.86642 18.4428C15.0023 7.58457 24.5434 19.0211 24.5434 19.0211C24.5434 19.0211 20.7848 8.64469 30.1974 7.58457C42.3728 6.20319 43.6578 22.0087 43.6578 22.0087C43.6578 22.0087 51.9782 18.1858 53.5202 23.2616C55.6725 30.2648 46.6454 30.8109 46.6454 30.8109" ] []
            , Svg.path [ css [ fill.none, stroke.medium ], d "M20.0138 30.1042C20.0138 30.1042 13.4924 36.3686 11.1794 32.3851C8.22392 27.2772 14.6489 25.8637 14.6489 25.8637C14.6489 25.8637 6.32854 22.9403 8.86642 18.4428C15.0023 7.58457 24.5434 19.0211 24.5434 19.0211C24.5434 19.0211 20.7848 8.64469 30.1974 7.58457C42.3728 6.20319 43.6578 22.0087 43.6578 22.0087C43.6578 22.0087 51.9782 18.1858 53.5202 23.2616C55.6725 30.2648 46.6454 30.8109 46.6454 30.8109" ] []

            -- Left Eye
            , Svg.path [ css [ fill.white, stroke.lightest ], d "M25.698 38.3433C27.2133 38.164 28.2845 36.6903 28.0906 35.0516C27.8967 33.413 26.5112 32.23 24.9959 32.4093C23.4806 32.5885 22.4094 34.0623 22.6032 35.7009C22.7971 37.3395 24.1827 38.5226 25.698 38.3433Z" ] []
            , Svg.path [ css [ fill.iris ], d "M26.2344 37.0026C26.9744 36.915 27.4932 36.1584 27.3932 35.3127C27.2931 34.4669 26.6121 33.8523 25.872 33.9399C25.132 34.0274 24.6132 34.784 24.7133 35.6297C24.8134 36.4755 25.4944 37.0901 26.2344 37.0026Z" ] []

            -- Glasses
            , Svg.path [ css [ fill.glasses, stroke.thin ], d "M50.0184 34.7624C51.207 34.9872 52.0744 35.8225 51.721 37.2039C51.5604 37.8785 50.3075 38.007 50.3075 36.6256C50.3075 36.1116 49.4722 36.208 49.4722 36.208L42.469 36.0795L42.5332 34.5054C42.5332 34.5375 48.9582 34.5375 50.0184 34.7624Z" ] []
            , Svg.path [ css [ fill.glasses ], d "M43.0151 35.9835C42.983 36.2084 43.0472 35.1804 43.0794 34.57C43.0794 34.4415 43.0794 34.2809 43.1115 34.1203C43.1757 32.7068 43.2721 31.1005 42.1477 30.0404C41.8907 29.8155 41.2482 29.5585 40.7664 29.3979L40.67 29.3658L32.446 29.3015L32.3496 29.3336C31.8999 29.5264 31.2895 29.7834 31.0646 30.0725C30.2936 31.0041 30.2936 32.8353 30.3579 34.4094H29.0407C29.0407 34.3451 29.0407 34.2809 29.0407 34.2166C29.105 32.7389 29.1692 31.1969 28.1734 30.1689C27.9485 29.9119 27.3381 29.687 26.8884 29.5264L26.792 29.4943L19.2747 29.43L19.1462 29.4943C18.7929 29.6549 18.1825 29.944 17.9576 30.2331C17.2187 31.229 17.283 33.2529 17.3151 34.8913C17.3151 35.2125 17.3472 35.5016 17.3472 35.7586C17.3472 39.4209 19.5639 41.7018 23.0976 41.7018C26.6314 41.7018 28.8801 39.196 29.0086 35.598H30.3579C30.3579 35.6301 30.3579 35.6301 30.3579 35.6623C30.3579 39.3245 32.7672 41.5733 36.6222 41.5733C40.2202 41.5733 42.7902 39.196 43.0472 35.7265M27.8521 34.2166C27.82 34.57 27.82 34.9555 27.82 35.3089C27.82 36.8188 27.3702 38.1359 26.5029 39.0996C25.6355 40.0634 24.4469 40.5774 23.0655 40.5774C20.1421 40.5774 18.4716 38.8426 18.4716 35.8229C18.4716 35.5659 18.4716 35.2446 18.4395 34.9234C18.4074 33.7026 18.3431 31.6788 18.825 31.0041C18.8892 30.9399 19.1784 30.7793 19.4354 30.6508L26.535 30.715C26.8562 30.8435 27.2096 30.972 27.306 31.0684C27.9485 31.743 27.9164 32.9316 27.8521 34.2166ZM40.4451 38.9711C39.4814 39.9349 38.1642 40.4489 36.6222 40.4489C33.3776 40.4489 31.5144 38.7141 31.5144 35.6944C31.5144 35.4374 31.5144 35.1161 31.4822 34.7949C31.4501 33.5741 31.3859 31.5824 31.932 30.9078C32.0284 30.8435 32.3496 30.6829 32.6387 30.5544L40.4772 30.6186C40.8627 30.7471 41.2482 30.9078 41.3446 30.972C42.0514 31.6466 41.9871 32.8674 41.9229 34.1203C41.8907 34.4736 41.8907 34.8591 41.8907 35.2125V35.2446C41.8907 36.6903 41.3767 38.0074 40.4451 38.9711Z" ] []
            , Svg.path [ css [ fill.none, stroke.thin ], d "M43.0151 35.9835C42.983 36.2084 43.0472 35.1804 43.0794 34.57C43.0794 34.4415 43.0794 34.2809 43.1115 34.1203C43.1757 32.7068 43.2721 31.1005 42.1477 30.0404C41.8907 29.8155 41.2482 29.5585 40.7664 29.3979L40.67 29.3658L32.446 29.3015L32.3496 29.3336C31.8999 29.5264 31.2895 29.7834 31.0646 30.0725C30.2936 31.0041 30.2936 32.8353 30.3579 34.4094H29.0407C29.0407 34.3451 29.0407 34.2809 29.0407 34.2166C29.105 32.7389 29.1692 31.1969 28.1734 30.1689C27.9485 29.9119 27.3381 29.687 26.8884 29.5264L26.792 29.4943L19.2747 29.43L19.1462 29.4943C18.7929 29.6549 18.1825 29.944 17.9576 30.2331C17.2187 31.229 17.283 33.2529 17.3151 34.8913C17.3151 35.2125 17.3472 35.5016 17.3472 35.7586C17.3472 39.4209 19.5639 41.7018 23.0976 41.7018C26.6314 41.7018 28.8801 39.196 29.0086 35.598H30.3579C30.3579 35.6301 30.3579 35.6301 30.3579 35.6623C30.3579 39.3245 32.7672 41.5733 36.6222 41.5733C40.2202 41.5733 42.7902 39.196 43.0472 35.7265M27.8521 34.2166C27.82 34.57 27.82 34.9555 27.82 35.3089C27.82 36.8188 27.3702 38.1359 26.5029 39.0996C25.6355 40.0634 24.4469 40.5774 23.0655 40.5774C20.1421 40.5774 18.4716 38.8426 18.4716 35.8229C18.4716 35.5659 18.4716 35.2446 18.4395 34.9234C18.4074 33.7026 18.3431 31.6788 18.825 31.0041C18.8892 30.9399 19.1784 30.7793 19.4354 30.6508L26.535 30.715C26.8562 30.8435 27.2096 30.972 27.306 31.0684C27.9485 31.743 27.9164 32.9316 27.8521 34.2166ZM40.4451 38.9711C39.4814 39.9349 38.1642 40.4489 36.6222 40.4489C33.3776 40.4489 31.5144 38.7141 31.5144 35.6944C31.5144 35.4374 31.5144 35.1161 31.4822 34.7949C31.4501 33.5741 31.3859 31.5824 31.932 30.9078C32.0284 30.8435 32.3496 30.6829 32.6387 30.5544L40.4772 30.6186C40.8627 30.7471 41.2482 30.9078 41.3446 30.972C42.0514 31.6466 41.9871 32.8674 41.9229 34.1203C41.8907 34.4736 41.8907 34.8591 41.8907 35.2125V35.2446C41.8907 36.6903 41.3767 38.0074 40.4451 38.9711Z" ] []

            -- Muzzle
            , Svg.path [ css [ fill.body ], d "M30.1972 39.7098C30.1972 39.7098 16.4477 38.3605 15.4197 42.4083C14.7451 45.171 15.5482 48.6084 15.7731 50.568C15.8695 51.4354 16.6405 52.11 17.5721 52.11H26.2137" ] []
            , Svg.path [ css [ fill.line ], d "M30.1972 39.4204C28.6552 39.2598 27.0811 39.1956 25.5391 39.1956C22.9691 39.1956 20.1742 39.2277 17.7006 40.1272C16.8011 40.4484 15.8695 40.9303 15.3876 41.7656C15.0021 42.4081 14.9379 43.2433 14.9057 43.9501C14.8094 45.1066 14.9057 46.2309 15.0342 47.3874C15.1627 48.3512 15.3234 49.3471 15.4519 50.3108C15.5804 51.2103 15.9337 51.9492 16.8332 52.3026C17.54 52.5596 18.4716 52.4311 19.2105 52.4311C21.4914 52.4311 23.7722 52.4632 26.0852 52.4311C26.1495 52.4311 26.1816 52.4311 26.2459 52.4311C26.6314 52.4311 26.6314 51.8528 26.2459 51.8528H20.4312C19.5317 51.8528 18.6322 51.8528 17.7649 51.8528C17.2187 51.8528 16.7047 51.6922 16.3835 51.2424C16.1265 50.8891 16.0944 50.4714 16.0301 50.0538C15.9659 49.6041 15.9016 49.1222 15.8052 48.6403C15.6446 47.5802 15.5161 46.4879 15.4519 45.3957C15.4197 44.3998 15.4197 43.3076 15.741 42.3438C16.0301 41.5407 16.8332 41.0909 17.5721 40.7697C19.853 39.8381 22.5194 39.7738 24.9287 39.7738C26.6956 39.7738 28.4625 39.8059 30.1972 39.9987C30.5827 40.0629 30.5827 39.4847 30.1972 39.4204Z" ] []

            -- Right Eye
            , Svg.path [ css [ fill.white, stroke.lightest ], d "M39.3889 35.4909C39.5953 33.7466 38.4485 32.177 36.8275 31.9852C35.2065 31.7935 33.7251 33.0521 33.5187 34.7964C33.3123 36.5408 34.4591 38.1103 36.0801 38.3021C37.7011 38.4939 39.1825 37.2353 39.3889 35.4909Z" ] []
            , Svg.path [ css [ fill.iris ], d "M37.0844 35.2801C37.1928 34.3639 36.638 33.5451 35.8451 33.4513C35.0522 33.3575 34.3215 34.0242 34.2131 34.9404C34.1047 35.8566 34.6596 36.6754 35.4525 36.7692C36.2454 36.863 36.976 36.1963 37.0844 35.2801Z" ]
                []

            -- Scarf
            , Svg.g
                [ css [ fill.scarf, stroke.medium ] ]
                [ Svg.path [ d "M1.02771 88.7005C1.02771 88.7005 5.30034 83.2071 10.5367 82.1791C15.7731 81.1511 20.0457 82.8216 20.0457 82.8216C20.0457 82.8216 16.8653 88.315 10.5367 89.5678C7.06721 90.3067 0.99559 88.7005 1.02771 88.7005Z" ] []
                , Svg.path [ d "M19.821 83.2391C19.821 83.2391 14.3276 81.3116 10.6332 86.4195C6.93884 91.5274 6.93884 97.5348 6.93884 97.5348C6.93884 97.5348 13.4923 96.3783 16.2551 93.7119C22.2303 87.833 19.821 83.2391 19.821 83.2391Z" ] []
                , Svg.path [ d "M20.0136 82.8216C20.0136 82.8216 23.7723 85.1667 34.245 84.6527C41.9229 84.2672 46.8701 80.7656 46.8701 80.7656C46.8701 80.7656 48.7977 81.9221 49.0547 84.974C49.3117 88.0259 48.5085 89.4394 48.5085 89.4394C48.5085 89.4394 39.385 92.8446 31.8999 91.3347C21.9411 89.3109 20.0458 87.03 20.0458 87.03C20.0458 87.03 18.8893 85.6165 19.1463 84.4921C19.4033 83.3677 20.0136 82.8216 20.0136 82.8216Z" ] []
                ]
            , Svg.path [ css [ fill.line ], d "M19.6603 84.9739C26.2138 87.6724 33.4419 87.7688 40.413 87.2548C33.6989 88.9574 25.6998 88.7325 19.6603 84.9739Z" ] []

            -- Eyebrows
            , Svg.g
                [ css [ fill.hoof ] ]
                [ Svg.path [ d "M20.5598 27.4703C20.5598 27.4703 20.7204 25.1894 22.7122 23.9044C24.7039 22.6194 26.9527 23.5832 26.9527 24.9003C26.9527 26.2175 24.8324 25.8641 23.7402 26.1853C22.6479 26.5066 20.5598 27.4703 20.5598 27.4703Z" ] []
                , Svg.path [ d "M39.6741 27.4703C39.6741 27.4703 39.5135 25.1894 37.5217 23.9044C35.53 22.6194 33.2812 23.5832 33.2812 24.9003C33.2812 26.2175 35.4015 25.8641 36.4938 26.1853C37.586 26.5066 39.6741 27.4703 39.6741 27.4703Z" ] []
                ]

            -- Smile
            , Svg.g
                [ css [ fill.iris ] ]
                [ Svg.path [ d "M16.3192 46.8414C16.4477 47.0662 16.7368 47.1947 16.9617 47.2911C17.7327 47.5481 18.5358 47.5802 19.3711 47.6124C21.5235 47.6445 23.6758 47.1947 25.5391 46.1025C26.4707 45.5564 27.3381 44.9139 28.1091 44.0786C26.535 47.2269 22.68 48.6725 19.339 48.2227C18.3431 48.0942 16.7368 47.8694 16.3192 46.8414Z" ] []
                , Svg.path [ d "M18.5039 45.2992C18.5039 45.9738 18.5039 46.6806 18.5039 47.3552V47.6443C18.5039 48.0298 19.0822 48.0298 19.0822 47.6443V45.2671C19.1143 44.9458 18.5039 44.9458 18.5039 45.2992Z" ] []
                , Svg.path [ d "M16.1908 42.8262C16.8012 43.9184 17.7007 44.8822 18.9214 45.0749C20.1422 45.1392 21.2665 44.4967 22.3909 44.0148C23.1619 43.6293 23.9008 43.2438 24.6718 42.8262C24.0614 43.4365 23.3547 43.9505 22.6158 44.4003C21.4914 45.0749 20.2385 45.7817 18.825 45.621C17.4758 45.364 16.4478 44.079 16.1908 42.8262Z" ] []
                ]
            ]
        ]


redPalette =
    { stroke =
        { black = Css.property "stroke" "#000000"
        , width = \x -> Css.property "stroke-width" (String.fromFloat x)
        }
    , fill =
        { platform = Css.fill (Css.hex "9f9faa")
        , body = Css.fill (Css.hex "DF633F")
        , bodySecondary = Css.fill (Css.hex "822D19")
        , bodyTertiary = Css.fill (Css.hex "FFE4D1")
        , shirt = Css.fill (Css.hex "F0EFED")
        , pants = Css.fill (Css.hex "895C48")
        , shoes = Css.fill (Css.hex "11222B")
        , white = Css.fill (Css.hex "FFFFFF")
        , iris = Css.fill (Css.hex "0E0A0B")
        , dark = Css.fill (Css.hex "231F20")
        , pointer = Css.fill (Css.hex "06CCBB")
        , none = Css.property "fill" "none"
        , button = Css.fill (Css.hex "F2336C")
        , mouth = Css.fill (Css.hex "8c1111")
        , black = Css.fill (Css.hex "000000")
        }
    }


redHeadshot : Nri.Ui.Svg.V1.Svg
redHeadshot =
    let
        fill =
            redPalette.fill

        stroke =
            { tiny = Css.batch [ Css.property "stroke" "#000000", Css.property "stroke-width" "0.35" ]
            , light = Css.batch [ Css.property "stroke" "#000000", Css.property "stroke-width" "0.584375" ]
            , medium = Css.batch [ Css.property "stroke" "#000000", Css.property "stroke-width" "0.81875" ]
            , thick = Css.batch [ Css.property "stroke" "#000000", Css.property "stroke-width" "0.85" ]
            , thickest = Css.batch [ Css.property "stroke" "#000000", Css.property "stroke-width" "0.978125" ]
            }
    in
    Nri.Ui.Svg.V1.init "0 0 60 104"
        [ Svg.defs
            []
            [ Svg.g
                [ id "redHeadshot_ears", css [ Css.property "stroke-width" "0.81875" ] ]
                [ Svg.path [ d "M23.4007 41.9574C23.4007 41.9574 23.7132 37.2386 19.6507 30.8949C15.9944 25.1449 12.2757 23.9886 9.83819 26.8011C8.65069 28.1761 4.93194 34.7699 2.77569 43.2386C0.775691 51.1136 0.525691 58.7699 0.431941 60.7074C0.275691 64.4574 23.4007 41.9574 23.4007 41.9574Z" ] []
                , Svg.path [ d "M36.9944 42.0199C36.9944 42.0199 37.2756 37.3011 42.0569 31.4886C46.4006 26.2386 50.2131 25.5511 52.3069 28.6136C53.3381 30.1136 56.1819 37.1136 57.3069 45.7699C58.3381 53.8324 57.6194 61.4574 57.4944 63.3949C57.2131 67.1761 36.9944 42.0199 36.9944 42.0199Z" ] []
                ]
            , Svg.path [ id "redHeadshot_head", css [ Css.property "stroke-width" "0.81875" ], d "M0.431885 61.3325C0.431885 61.3325 3.30689 49.5513 16.6506 43.1763C26.1506 38.645 37.3069 39.4263 47.3381 47.7075C58.7444 57.1138 57.7444 68.02 57.7444 68.02C57.7444 68.02 51.6194 77.02 43.4006 79.8638C41.4944 80.52 37.6194 81.0201 35.7756 80.8951C28.8381 80.4263 27.1506 80.6763 21.4319 79.9263C19.6506 79.6763 17.9006 79.5513 15.9006 78.895C3.46314 74.895 0.431885 61.3325 0.431885 61.3325Z" ] []
            ]
        , renderHeadshotFrame
            { name = "redHeadshot"
            , primaryPath = "M0 73.1428C0 56.5743 13.4315 43.1428 30 43.1428V43.1428C46.5685 43.1428 60 56.5743 60 73.1428V73.1428C60 89.7114 46.5685 103.143 30 103.143V103.143C13.4315 103.143 0 89.7114 0 73.1428V73.1428Z"
            , primaryColor = Css.hex "E6D5D1"
            , clippedSvg =
                Svg.g []
                    [ -- Neck
                      Svg.path [ css [ fill.body, stroke.medium ], d "M24.4943 76.895L23.1506 84.5513C23.1506 84.5513 22.0256 87.77 27.0256 88.27C32.0256 88.7388 31.8693 85.3325 31.8693 85.3325L30.6506 76.7388L24.4943 76.895Z" ] []

                    -- Right Arm
                    , Svg.path [ css [ fill.shirt, stroke.thick ], d "M39.6195 83.989C47.1195 84.864 52.5883 89.989 51.557 95.7702C50.5258 101.551 41.4945 105.083 41.4945 105.083L45.182 107.145C45.182 107.145 44.8383 109.208 41.4945 109.364C38.1508 109.52 36.6508 107.208 36.6508 107.208L37.2133 101.489C37.2133 101.489 42.9008 98.864 44.3695 97.3952C45.9633 95.7702 46.1195 92.989 43.3695 91.3952C40.6195 89.8015 34.682 91.3952 34.682 91.3952L31.2445 84.7077C31.2758 84.7077 32.1195 83.0827 39.6195 83.989Z" ] []

                    -- Left Arm
                    , Svg.path [ css [ fill.shirt, stroke.thick ], d "M20.6818 96.3639C20.6818 96.3639 7.99427 102.77 -0.755727 95.4264C-10.3182 87.3951 -9.81823 76.5514 -8.97448 74.8326C-8.44323 73.7701 -7.56823 72.1139 -5.97448 75.4264C-4.38073 78.7389 -4.34948 82.8951 2.11927 88.8326C5.40052 91.8326 10.5255 92.3951 13.6505 91.3639C17.838 89.9576 19.7755 86.0826 21.9005 85.0826C24.0255 84.0826 24.713 86.2389 23.5255 88.3014C22.0255 90.8014 20.6818 96.3639 20.6818 96.3639Z" ] []

                    -- Shirt body
                    , Svg.path [ css [ fill.shirt, stroke.thick ], d "M38.3694 100.614C38.3694 109.708 38.5881 115.239 27.9944 115.239C17.4006 115.239 17.4006 109.739 17.4006 100.614C17.4006 94.8013 18.9944 87.9888 21.9006 85.0513C23.5569 83.3638 23.0569 85.2388 27.8694 85.2388C31.8694 85.2388 31.8381 83.1763 33.4006 84.7075C36.4319 87.6138 38.3694 94.645 38.3694 100.614Z" ] []

                    -- Shirt line
                    , Svg.path [ css [ stroke.light ], d "M27.4631 105.176V86.9263" ] []
                    ]
            , secondaryPath = "M0 43.1428H60H0ZM62.5 74.3928C62.5 91.6517 48.5089 105.643 31.25 105.643C13.9911 105.643 0 91.6517 0 74.3928V73.1428C0 88.3307 13.4315 100.643 30 100.643C45.1878 100.643 57.5 88.3307 57.5 73.1428L62.5 74.3928ZM0 103.143V43.1428V103.143ZM31.25 43.1428C48.5089 43.1428 62.5 57.1339 62.5 74.3928C62.5 91.6517 48.5089 105.643 31.25 105.643L30 100.643C45.1878 100.643 57.5 88.3307 57.5 73.1428C57.5 56.5743 45.1878 43.1428 30 43.1428H31.25Z"
            , secondaryColor = Css.hex "822D19"
            }

        -- Ears
        , Svg.use [ xlinkHref "#redHeadshot_ears", css [ fill.bodyTertiary, stroke.medium ] ] []
        , Svg.mask [ id "redHeadshot_earsMask" ]
            [ Svg.use [ xlinkHref "#redHeadshot_ears", maskStyle ] []
            ]
        , Svg.g
            [ css [ Css.property "mask" "url(#redHeadshot_earsMask)" ] ]
            [ Svg.g
                [ css [ fill.bodySecondary ] ]
                [ Svg.path [ d "M13.9319 48.77C13.9319 48.77 18.1506 40.8638 17.3069 33.9888C16.4944 27.4888 9.46313 17.5513 9.46313 17.5513L24.8069 28.27L26.0256 37.6763V41.4888L13.9319 48.77Z" ] []
                , Svg.path [ d "M45.5569 49.9263C45.5569 49.9263 42.3381 41.5513 44.0256 34.8325C45.6194 28.4888 53.8381 19.4575 53.8381 19.4575L37.2756 28.2075L34.9006 37.395L34.4319 41.1763L45.5569 49.9263Z" ] []
                ]
            , Svg.g
                [ css [ fill.black ] ]
                [ Svg.path [ d "M13.7133 48.989C13.6195 48.8953 13.5883 48.6765 13.6508 48.5515L13.682 48.5203L13.7133 48.4578C13.7445 48.3953 13.8383 48.1765 13.8695 48.114C15.7758 43.8953 17.2758 39.2703 17.057 34.5828C16.9008 32.489 16.2758 30.489 15.4945 28.5515C14.4633 25.9578 13.1195 23.4578 11.682 21.0828C10.932 19.8953 10.1195 18.739 9.18201 17.7078L8.08826 16.1453L9.65076 17.239L24.9945 27.9578L25.1195 28.0515L25.1508 28.2078C25.1508 28.2703 26.3695 37.614 26.3695 37.6453V41.4578V41.6453L26.2133 41.739C22.6508 43.9578 17.4008 46.8953 13.7133 48.989ZM14.182 48.5203C17.5883 46.3015 22.432 43.239 25.8695 41.1765L25.7133 41.4578V37.6453C25.7445 37.864 24.4945 28.239 24.4945 28.3015L24.6195 28.5203L9.27576 17.8015L9.74451 17.3328C11.7445 21.0828 14.1508 24.5515 15.8695 28.4265C17.057 30.989 17.932 33.8015 17.7758 36.6765C17.5883 40.7703 16.307 44.7703 14.5258 48.4265L14.4008 48.6765L14.3383 48.8015L14.307 48.864L14.2758 48.8953C14.307 48.7703 14.2445 48.5828 14.182 48.5203Z" ] []
                , Svg.path [ d "M45.3695 49.6765C45.2758 49.7078 45.182 49.9265 45.2445 50.0203V49.989C45.1508 49.739 44.9945 49.3015 44.9008 49.0515C43.307 44.0515 42.3383 38.3953 44.2445 33.3328C45.307 30.7078 46.8695 28.3328 48.5258 26.0203C50.1508 23.7078 51.9945 21.614 53.6195 19.2703L54.0258 19.8015L37.4633 28.5515L37.6195 28.3328C37.6508 28.3328 35.2133 37.6453 35.2758 37.4578L34.807 41.239L34.682 40.9265C37.807 43.3953 42.2445 47.0828 45.3695 49.6765ZM45.7445 50.2078C42.3383 47.6453 37.5258 44.114 34.2445 41.489L34.0883 41.364L34.1195 41.1765L34.5883 37.3953C34.557 37.364 36.9633 28.2078 36.9633 28.1765L36.9945 28.0203L37.1195 27.9578L53.682 19.2078L55.3695 18.3015L54.0883 19.7078C53.0258 20.614 52.057 21.6765 51.182 22.7703C49.432 24.9578 47.807 27.2703 46.4633 29.739C45.4633 31.5828 44.5883 33.4578 44.182 35.5203C43.3695 40.1453 44.307 44.9265 45.682 49.3328L45.7758 49.5828L45.807 49.7078L45.8383 49.7703V49.8015C45.932 49.9265 45.8383 50.1453 45.7445 50.2078Z" ] []
                ]
            ]

        -- Shirt tie
        , Svg.path [ css [ fill.none, stroke.light ], d "M32.3694 84.145L31.8694 93.645L27.4631 86.2075L23.0569 94.145L22.3069 84.7075" ] []

        -- Head
        , Svg.use [ xlinkHref "#redHeadshot_head", css [ fill.body, stroke.medium ] ] []
        , Svg.mask [ id "redHeadshot_headMask" ]
            [ Svg.use [ xlinkHref "#redHeadshot_head", maskStyle ] []
            ]

        -- Cheek patches
        , Svg.g
            [ css [ fill.bodyTertiary, Css.property "mask" "url(#redHeadshot_headMask)" ] ]
            [ Svg.path [ css [ stroke.medium ], d "M19.4319 80.7073C19.4319 80.7073 20.1194 72.7698 23.1194 68.3948C25.6506 64.6761 27.7444 63.8948 30.8694 64.2698C33.9319 64.6136 36.9631 69.3323 37.7444 75.3636C38.5256 81.3948 38.3381 82.0823 38.3381 82.0823L19.4319 80.7073Z" ] []
            , Svg.path [ css [ stroke.light ], d "M41.1505 80.395C41.1505 80.395 43.338 79.4887 44.6505 75.2075C45.963 70.9575 44.838 63.5512 49.4318 63.0825C54.0255 62.6137 53.9943 68.77 53.4943 71.7387C52.9943 74.6762 51.1193 80.9262 51.1193 80.9262L41.1505 80.395Z" ] []
            , Svg.path [ css [ stroke.light ], d "M16.5568 79.1137C16.5568 79.1137 14.3693 78.2075 13.0568 73.9262C11.7443 69.6762 12.8693 62.27 8.27553 61.8012C3.68178 61.3325 3.71303 67.4887 4.21303 70.4575C4.74428 73.395 6.58803 79.645 6.58803 79.645L16.5568 79.1137Z" ] []
            ]

        -- Eyes
        , Svg.g
            [ css [ fill.white, stroke.medium ] ]
            [ Svg.path [ d "M26.3071 61.771C26.879 59.4233 25.5618 57.0864 23.3651 56.5512C21.1684 56.0161 18.924 57.4854 18.3521 59.833C17.7802 62.1806 19.0973 64.5176 21.294 65.0527C23.4907 65.5879 25.7351 64.1186 26.3071 61.771Z" ] []
            , Svg.path [ d "M39.6248 65.3296C41.8215 64.7945 43.1386 62.4575 42.5667 60.1099C41.9948 57.7623 39.7504 56.293 37.5537 56.8281C35.357 57.3633 34.0398 59.7002 34.6117 62.0479C35.1837 64.3955 37.4281 65.8648 39.6248 65.3296Z" ] []
            ]

        -- Iris
        , Svg.g
            [ css [ fill.iris ] ]
            [ Svg.path [ d "M25.2416 60.9739C25.366 59.8417 24.7019 58.8398 23.7584 58.7361C22.8148 58.6325 21.9491 59.4663 21.8247 60.5986C21.7003 61.7308 22.3644 62.7327 23.3079 62.8364C24.2515 62.94 25.1172 62.1062 25.2416 60.9739Z" ] []
            , Svg.path [ d "M37.591 63.1318C38.5346 63.0281 39.1986 62.0262 39.0742 60.894C38.9498 59.7617 38.0841 58.9279 37.1406 59.0316C36.197 59.1352 35.533 60.1371 35.6574 61.2694C35.7817 62.4016 36.6475 63.2355 37.591 63.1318Z" ] []
            ]

        -- Nose
        , Svg.path [ css [ fill.iris ], d "M26.1194 70.7075C25.4006 70.7075 24.9006 70.0512 25.1194 69.4262C25.6819 67.8637 26.9319 65.4887 29.4006 65.7387C31.8694 65.9887 32.7444 68.1137 33.1194 69.5199C33.2756 70.1449 32.7756 70.7075 32.0881 70.7075H26.1194Z" ] []

        -- Nose line
        , Svg.path [ css [ stroke.light ], d "M28.9319 69.8015L28.6506 76.1453" ] []

        -- Smile
        , Svg.path [ css [ fill.dark ], d "M25.0881 75.3323C25.9319 75.926 26.9006 76.1135 27.8694 76.1448C28.8381 76.176 29.8069 76.1135 30.7131 75.8323C32.6194 75.2385 34.0256 73.7698 34.9319 72.0198C34.5569 74.0823 32.9631 75.9573 30.9631 76.6135C29.0256 77.2073 26.3069 77.1135 25.0881 75.3323Z" ] []

        -- Right eyebrow
        , Svg.g
            [ css [ fill.bodyTertiary, stroke.medium ] ]
            [ Svg.path [ d "M24.6194 52.8638C25.1507 52.7701 25.3694 52.1138 24.9944 51.7076C23.9319 50.6451 22.0569 49.145 20.3382 49.7075C18.6194 50.27 17.8069 52.4888 17.4319 53.9576C17.3069 54.5201 17.8382 54.9576 18.3694 54.7388C19.2757 54.3638 20.4944 53.8326 21.3069 53.5826C22.1194 53.3638 23.5882 53.0513 24.6194 52.8638Z" ] []
            , Svg.path [ d "M35.0568 48.7386C34.5256 48.6448 34.3068 47.9886 34.6818 47.5823C35.7443 46.5198 37.6193 45.0198 39.3381 45.5823C41.0568 46.1448 41.8693 48.3636 42.2443 49.8323C42.3693 50.3948 41.8381 50.8323 41.3068 50.6136C40.4006 50.2386 39.1818 49.7073 38.3693 49.4573C37.5568 49.2386 36.0881 48.9261 35.0568 48.7386Z" ] []
            ]

        -- Buttons
        , Svg.g
            [ css [ fill.button, stroke.tiny ] ]
            [ Svg.path [ d "M25.4319 98.52C25.7253 98.52 25.9631 98.1702 25.9631 97.7388C25.9631 97.3073 25.7253 96.9575 25.4319 96.9575C25.1385 96.9575 24.9006 97.3073 24.9006 97.7388C24.9006 98.1702 25.1385 98.52 25.4319 98.52Z" ] []
            , Svg.path [ d "M26.5235 93.0049C26.5938 92.5793 26.4162 92.1954 26.1267 92.1476C25.8373 92.0997 25.5456 92.4061 25.4752 92.8318C25.4049 93.2574 25.5826 93.6413 25.872 93.6891C26.1615 93.737 26.4532 93.4306 26.5235 93.0049Z" ] []
            ]

        -- Shoulder Filler
        , Svg.g
            [ css [ fill.shirt ] ]
            [ Svg.path [ d "M17.1819 89.9263L22.0256 85.4263L20.4319 95.895L15.0881 97.4263L17.1819 89.9263Z" ] []
            , Svg.path [ d "M32.6818 84.5825C32.6818 84.5825 32.9318 84.2075 34.2755 84.145C35.6193 84.0825 36.713 84.4575 36.713 84.4575L38.7443 89.7075L33.5568 91.9887L32.6818 84.5825Z" ] []
            ]

        -- Armpits
        , Svg.g
            [ css [ fill.black ] ]
            [ Svg.path [ d "M37.7756 93.926C36.6819 92.3323 36.3069 90.4885 35.8069 88.676C37.1819 89.7073 37.8381 92.2385 37.7756 93.926Z" ] []
            , Svg.path [ d "M17.2132 99.989C17.0257 98.864 17.0882 97.7703 17.2444 96.6453C17.4007 95.5515 17.6194 94.4265 18.1507 93.4265C18.2757 95.6453 18.0569 97.8953 17.2132 99.989Z" ] []
            ]
        ]


salPalette =
    { fill =
        { platform = Css.fill (Css.hex "9E9EA9")
        , body = Css.fill (Css.hex "B298A2")
        , sock = Css.fill (Css.hex "F5C377")
        , shoe = Css.fill (Css.hex "D33353")
        , white = Css.fill (Css.hex "FFFFFF")
        , black = Css.fill (Css.hex "000000")
        , none = Css.property "fill" "none"
        , skirt = Css.fill (Css.hex "D88B29")
        , face = Css.fill (Css.hex "E7DDE1")
        , streak = Css.fill (Css.hex "6F5861")
        , nose = Css.fill (Css.hex "352C31")
        , sweater = Css.fill (Css.hex "FDF8DD")
        , iris = Css.fill (Css.hex "231F20")
        , eyebrow = Css.fill (Css.hex "5B4951")
        , pointer = Css.fill (Css.hex "06CBBA")
        , jewel3 = Css.fill (Css.hex "B1263F")
        }
    }


salHeadshot : Nri.Ui.Svg.V1.Svg
salHeadshot =
    let
        blankStroke x =
            Css.property "stroke-width" (String.fromFloat x)

        stroke x =
            Css.batch
                [ blankStroke x
                , Css.property "stroke" "#231F20"
                ]

        fill =
            salPalette.fill
    in
    Nri.Ui.Svg.V1.init "0 0 60 103"
        [ Svg.defs
            []
            [ Svg.path
                [ id "salHeadshot_face"
                , css [ blankStroke 0.4725 ]
                , d "M46.839 54.9705C47.784 58.8765 48.036 63.192 45.2325 66.6885C42.5235 70.059 37.83 72.201 28.569 71.6025C18.3315 70.9725 13.701 67.98 10.677 63.822C8.976 61.5225 9.5115 57.1755 10.2045 55.0965C12.5355 47.8515 18.9615 41.8665 29.01 41.8665C39.0585 41.8665 45.1065 47.568 46.839 54.9705Z"
                ]
                []
            ]
        , renderHeadshotFrame
            { name = "salHeadshot"
            , primaryPath = "M0 72.1785C0 55.6099 13.4315 42.1785 30 42.1785V42.1785C46.5685 42.1785 60 55.6099 60 72.1785V72.1785C60 88.747 46.5685 102.178 30 102.178V102.178C13.4315 102.178 0 88.747 0 72.1785V72.1785Z"
            , primaryColor = Css.hex "FDF3E4"
            , clippedSvg =
                Svg.g []
                    [ -- Left Arm
                      Svg.path [ css [ fill.sweater ], d "M14.079 94.8814C14.079 94.8814 -0.474017 95.8894 -7.75052 89.7469C-15.027 83.6044 -18.87 69.9964 -18.87 69.9964C-18.87 69.9964 -21.138 67.7914 -19.9725 67.4134C-17.0745 66.4999 -15.657 65.6809 -13.6095 63.6649C-12.318 62.4049 -11.9085 66.1534 -11.9085 66.1534C-11.9085 66.1534 -7.46702 75.3514 2.70748 79.2574C12.882 83.1949 17.922 80.5804 17.922 80.5804" ] []
                    , Svg.path [ css [ fill.none, stroke 0.8253 ], d "M14.079 94.8814C14.079 94.8814 -0.474017 95.8894 -7.75052 89.7469C-15.027 83.6044 -18.87 69.9964 -18.87 69.9964C-18.87 69.9964 -21.138 67.7914 -19.9725 67.4134C-17.0745 66.4999 -15.657 65.6809 -13.6095 63.6649C-12.318 62.4049 -11.9085 66.1534 -11.9085 66.1534C-11.9085 66.1534 -7.46702 75.3514 2.70748 79.2574C12.882 83.1949 17.922 80.5804 17.922 80.5804" ] []

                    -- Right Arm
                    , Svg.path [ css [ fill.sweater ], d "M40.6022 79.5092C40.6022 79.5092 47.0282 77.1782 56.7932 80.4227C69.2357 84.5807 73.2992 92.7077 72.5747 95.4482C72.5747 95.4482 71.6297 99.9842 65.8022 98.1257C60.6677 96.4877 58.3682 94.3142 54.0212 93.2432C50.3042 92.2982 47.6267 92.0777 47.6267 92.0777" ] []
                    , Svg.path [ css [ fill.none, stroke 0.8253 ], d "M40.6022 79.5092C40.6022 79.5092 47.0282 77.1782 56.7932 80.4227C69.2357 84.5807 73.2992 92.7077 72.5747 95.4482C72.5747 95.4482 71.6297 99.9842 65.8022 98.1257C60.6677 96.4877 58.3682 94.3142 54.0212 93.2432C50.3042 92.2982 47.6267 92.0777 47.6267 92.0777" ] []

                    -- Body
                    , Svg.path [ css [ fill.sweater ], d "M39.9719 79.5408H43.0904L49.6424 84.9903V89.0538H41.2949L39.9719 79.5408Z" ] []
                    , Svg.path [ css [ fill.sweater ], d "M12.0314 82.2182L19.1189 81.1472L19.7489 92.7077L7.9679 90.5657L12.0314 82.2182Z" ] []
                    ]
            , secondaryPath = "M0 42.1785H60H0ZM62.5 73.4285C62.5 90.6874 48.5089 104.678 31.25 104.678C13.9911 104.678 0 90.6874 0 73.4285V72.1785C0 87.3663 13.4315 99.6785 30 99.6785C45.1878 99.6785 57.5 87.3663 57.5 72.1785L62.5 73.4285ZM0 102.178V42.1785V102.178ZM31.25 42.1785C48.5089 42.1785 62.5 56.1696 62.5 73.4285C62.5 90.6874 48.5089 104.678 31.25 104.678L30 99.6785C45.1878 99.6785 57.5 87.3663 57.5 72.1785C57.5 55.6099 45.1878 42.1785 30 42.1785H31.25Z"
            , secondaryColor = Css.hex "F6C477"
            }

        -- Head
        , Svg.path [ css [ fill.body, stroke 0.8253 ], d "M18.9301 78.6903L19.5916 73.8078C19.5916 73.8078 12.189 72.9573 8.34605 66.8463C4.50305 60.7353 5.44805 52.2933 11.5905 45.8043C17.733 39.3468 26.3011 35.0943 39.7831 38.8113C53.2651 42.5283 54.3046 55.5063 54.1786 58.0893C54.0526 60.9873 53.1391 67.1298 47.4376 70.5633C41.9251 73.9023 38.8381 74.0913 38.8381 74.0913L39.4051 76.6743C39.4051 76.6743 38.7751 81.1158 29.4826 82.1553C22.3636 82.9113 18.9301 78.6903 18.9301 78.6903Z" ] []
        , Svg.path [ css [ fill.body ], d "M22.5526 39.1579C22.5526 39.1579 20.0326 38.4649 20.3476 34.5589C20.6941 30.1804 25.1671 29.1094 25.1671 29.1094C25.1671 29.1094 24.8836 30.8104 26.5216 32.9839C28.7896 35.9764 36.1606 37.3939 39.8461 38.7799" ] []
        , Svg.path [ css [ fill.none, stroke 0.8253 ], d "M22.5526 39.1579C22.5526 39.1579 20.0326 38.4649 20.3476 34.5589C20.6941 30.1804 25.1671 29.1094 25.1671 29.1094C25.1671 29.1094 24.8836 30.8104 26.5216 32.9839C28.7896 35.9764 36.1606 37.3939 39.8461 38.7799" ] []

        -- Sweater Neck
        , Svg.mask [ id "salHeadshot_circleMask" ]
            [ Svg.use [ xlinkHref "#salHeadshot_circle", css [ fill.white ] ] []
            , Svg.use [ xlinkHref "#salHeadshot_circleShadow", css [ fill.black ] ] []
            ]
        , Svg.path [ css [ fill.sweater, stroke 0.8253, Css.property "mask" "url(#salHeadshot_circleMask)" ], d "M15.8743 107.072C15.8743 107.072 12.6928 104.961 12.1258 99.3542C11.1178 89.0852 18.3627 80.8007 18.3627 80.8007C18.3627 80.8007 17.1343 78.1232 18.0163 77.5562C19.1188 76.8002 21.7648 79.0367 29.1042 78.7217C36.4437 78.4067 39.8142 76.1702 40.5702 76.3592C41.7987 76.6742 40.5702 79.5092 40.5702 79.5092C40.5702 79.5092 48.3507 85.6517 48.7602 94.9442C49.2012 104.804 44.8857 105.812 44.8857 105.812C44.8857 105.812 38.9007 108.332 30.6162 108.962C22.7728 109.56 15.8743 107.072 15.8743 107.072Z" ] []

        -- Face
        , Svg.use [ xlinkHref "#salHeadshot_face", css [ fill.face, stroke 0.4725 ] ] []
        , Svg.mask [ id "salHeadshot_faceMask" ]
            [ Svg.use [ xlinkHref "#salHeadshot_face", maskStyle ] []
            ]
        , Svg.g
            [ css [ Css.property "mask" "url(#salHeadshot_faceMask)" ] ]
            [ Svg.path [ css [ fill.streak, stroke 0.4725 ], d "M37.2314 49.4272C37.2314 49.4272 40.6334 49.8052 43.7834 52.1047C46.9019 54.4042 48.1304 57.0187 48.1304 57.0187L47.9414 61.9012C47.9414 61.9012 41.9879 59.8538 38.9639 59.7908C35.9399 59.7278 35.6564 59.6647 35.6564 59.6647L37.2314 49.4272Z" ] []
            , Svg.path [ css [ fill.streak, stroke 0.4725 ], d "M19.749 47.8206C19.749 47.8206 15.15 48.7341 12.0315 51.0336C8.91299 53.3331 7.46399 55.9161 7.46399 55.9161L8.28299 60.0111C8.28299 60.0111 10.8345 57.554 13.89 57.302C15.5595 57.1445 17.9535 57.2706 17.9535 57.2706L19.749 47.8206Z" ] []
            ]

        -- Nose
        , Svg.path [ css [ fill.nose, stroke 0.58905 ], d "M21.891 60.5463C21.6705 60.3888 21.5445 60.1368 21.6075 59.8848C21.828 58.7508 22.8045 55.6953 26.364 55.9473C29.7975 56.1993 30.963 59.1288 31.3095 60.3888C31.404 60.7038 31.2465 61.0503 30.9315 61.1763C30.018 61.5543 27.939 62.2158 25.608 61.8063C23.4975 61.4283 22.395 60.8613 21.891 60.5463Z" ] []

        -- Eyes
        , Svg.g
            [ css [ fill.white, stroke 0.8253 ] ]
            [ Svg.path [ d "M40.6903 55.7751C41.4043 53.0846 40.3063 50.4585 38.238 49.9097C36.1696 49.3608 33.9141 51.097 33.2001 53.7875C32.4861 56.4781 33.5841 59.1041 35.6524 59.653C37.7208 60.2019 39.9763 58.4657 40.6903 55.7751Z" ] []
            , Svg.path [ d "M19.0266 57.6416C21.1643 57.5468 22.7972 55.2157 22.6738 52.435C22.5505 49.6542 20.7175 47.4769 18.5799 47.5717C16.4422 47.6665 14.8092 49.9976 14.9326 52.7784C15.0559 55.5591 16.8889 57.7365 19.0266 57.6416Z" ] []
            ]

        -- Iris
        , Svg.g
            [ css [ fill.iris ] ]
            [ Svg.path [ d "M37.5852 54.5367C37.8562 53.296 37.3456 52.1306 36.4448 51.9339C35.544 51.7371 34.594 52.5834 34.323 53.8242C34.052 55.065 34.5625 56.2303 35.4634 56.4271C36.3642 56.6238 37.3142 55.7775 37.5852 54.5367Z" ] []
            , Svg.path [ d "M19.9064 54.5297C20.8285 54.5297 21.5759 53.5001 21.5759 52.2302C21.5759 50.9602 20.8285 49.9307 19.9064 49.9307C18.9844 49.9307 18.2369 50.9602 18.2369 52.2302C18.2369 53.5001 18.9844 54.5297 19.9064 54.5297Z" ] []
            , Svg.path [ d "M23.4973 63.3816C27.0253 64.7676 30.6793 63.7911 33.7978 61.9011C31.6558 65.0826 26.3638 66.3426 23.4973 63.3816Z" ] []
            ]

        -- Eyelids
        , Svg.g
            [ css [ fill.streak, stroke 0.8253 ] ]
            [ Svg.path [ d "M33.5461 52.8291C33.5461 52.8291 34.6801 52.2621 37.2001 52.8921C39.8461 53.5536 40.9171 54.7821 40.9171 54.7821C40.9171 54.7821 41.5156 52.2306 40.0036 50.6871C38.7121 49.3326 36.9481 49.0176 35.4046 50.0256C34.1131 50.9076 33.5461 52.8291 33.5461 52.8291Zs" ] []
            , Svg.path [ d "M22.5526 51.4112C22.5526 51.4112 21.5446 50.6237 18.9616 50.6867C16.2526 50.7497 14.8981 51.7262 14.8981 51.7262C14.8981 51.7262 14.8666 49.1117 16.6621 47.9147C18.2056 46.8752 20.0011 46.9382 21.2926 48.2612C22.3951 49.4267 22.5526 51.4112 22.5526 51.4112Z" ] []
            ]

        -- Eyebrows
        , Svg.g
            [ css [ fill.eyebrow, stroke 0.8253 ] ]
            [ Svg.path [ d "M41.7044 46.277C41.7044 46.277 38.9324 44.891 37.2314 44.4185C35.5304 43.946 34.2074 43.631 34.8689 42.4025C35.5304 41.1425 37.9559 41.174 39.4049 42.4025C40.8539 43.631 41.7044 46.277 41.7044 46.277Z" ] []
            , Svg.path [ d "M17.7015 45.0487C17.7015 45.0487 20.631 44.1352 22.395 43.9147C24.159 43.6942 25.482 43.6312 25.041 42.2767C24.6 40.9222 22.206 40.5757 20.568 41.5522C18.93 42.5287 17.7015 45.0487 17.7015 45.0487Z" ] []
            ]

        -- Necklace
        , Svg.path [ css [ fill.iris ], d "M17.7014 79.3522C18.1424 85.6522 20.4419 95.3227 28.7579 92.9917C35.0894 91.0702 39.2789 83.1637 41.7044 77.3992C39.8459 83.6677 35.6879 91.8262 29.0099 93.8737C20.0009 96.2992 17.3864 86.3137 17.7014 79.3522Z" ] []

        -- Gem 1
        , Svg.path [ css [ fill.sock, stroke 0.58905 ], d "M25.0953 93.639C25.3719 92.5965 24.8873 91.5632 24.0129 91.3311C23.1385 91.0991 22.2053 91.7562 21.9287 92.7988C21.652 93.8413 22.1366 94.8746 23.011 95.1067C23.8854 95.3387 24.8186 94.6816 25.0953 93.639Z" ] []

        -- Gem 2
        , Svg.path [ css [ fill.shoe, stroke 0.58905 ], d "M26.4583 92.5501L25.6078 97.5901H29.7028L28.8208 92.2981L26.4583 92.5501Z" ] []

        -- Gem 3
        , Svg.path [ css [ fill.jewel3, stroke 0.58905 ], d "M30.1439 92.0464L31.4039 94.9444L33.9869 93.4954L32.2544 90.8494L30.1439 92.0464Z" ] []
        , Svg.g
            [ css [ fill.none, stroke 0.58905 ] ]
            [ Svg.path [ d "M17.9221 80.3598C17.9221 80.3598 20.7886 82.8168 27.6556 82.6908C34.5226 82.5333 40.5706 79.5093 40.5706 79.5093" ] []
            ]
        , Svg.g
            [ css [ fill.iris ] ]
            [ Svg.path [ d "M24.5054 29.519C24.0644 33.299 26.3009 36.638 29.2619 38.78C25.7024 37.5515 22.9304 33.2045 24.5054 29.519Z" ] []
            , Svg.path [ d "M12.3465 93.3379C12.1575 91.8889 12.5355 90.3139 13.512 89.2114C13.6065 90.6919 13.2285 92.1724 12.3465 93.3379Z" ] []
            , Svg.path [ d "M48.5084 92.2983C47.4059 90.6918 47.0279 88.8333 46.5239 87.0063C47.9099 88.0458 48.5714 90.5973 48.5084 92.2983Z" ] []
            ]
        ]
