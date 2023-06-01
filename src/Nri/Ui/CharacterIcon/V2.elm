module Nri.Ui.CharacterIcon.V2 exposing (lindyInstructive, lindySupportive)

{-|

@docs lindyHeadshot, lindyInstructive, lindySupportive
@docs redHeadshot, redInstructive, redSupportive
@docs salHeadshot, salInstructive, salSupportive

-}

import Css
import Nri.Ui.Svg.V1
import Svg.Styled as Svg
import Svg.Styled.Attributes exposing (..)


{-| -}
lindyHeadshot : Nri.Ui.Svg.V1.Svg
lindyHeadshot =
    Nri.Ui.Svg.V1.init ""
        []


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
    , tailPath : String
    , bodyPath : String
    , legPaths : List String
    , armPaths : List String
    , wristPaths : List String
    , hoofPaths : List String
    , shoePaths : List String
    , pointerPath : String
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
            [ Svg.ellipse [ css [ fill.body, Css.opacity (Css.num 0.35), Css.property "enable-background" "new" ], cx "229.6", cy "573.6", rx "90.1", ry "16.9" ] []
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
            , Svg.mask [ id (config.name ++ "_armMask") ]
                [ Svg.use [ xlinkHref ("#" ++ config.name ++ "_arms"), maskStyle ] []
                ]
            , Svg.g
                [ css [ fill.body, stroke.black, stroke.light, Css.property "mask" ("url(#" ++ config.name ++ "_armMask" ++ ")") ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.wristPaths)
            , Svg.path [ d config.pointerPath, css [ fill.pointer, stroke.black, stroke.light ] ] []
            , Svg.g
                [ css [ fill.hoof, stroke.dark, stroke.heavy ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.hoofPaths)
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
                [ css [ fill.scarf, stroke.dark, stroke.heavy ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.scarfPaths)
            , Svg.path [ d config.scarfLinePath, css [ fill.line ] ] []
            , Svg.g
                [ css [ fill.brows, stroke.dark, stroke.heavy ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.browPaths)
            , Svg.g
                [ css [ fill.iris ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.smilePaths)
            , Svg.g
                []
                (List.map (\path -> Svg.path [ d path ] []) config.armpitPaths)
            , Svg.g
                [ css [ fill.line ] ]
                (List.map (\path -> Svg.path [ d path ] []) config.pointerMagicPaths)
            , Svg.path [ d config.crotchLine1Path, css [ fill.pants ] ] []
            , Svg.path [ d config.crotchLine2Path, css [ fill.line ] ] []
            ]
        ]


lindyInstructive : Nri.Ui.Svg.V1.Svg
lindyInstructive =
    renderFullLindy
        { name = "lindyInstructive"
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
            [ "M175.1,548.7c0,0,6.3,9.7,19.7,10.9c13.4,1.2,21.8,0.9,21.8,0.9s-5,26.1-22.1,27.7c-17.1,1.5-55.8,2.1-51.3-12.5C147.7,561,175.1,548.7,175.1,548.7zs"
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


{-| -}
lindySupportive : Nri.Ui.Svg.V1.Svg
lindySupportive =
    Debug.todo ""


lindyPlatform : Svg.Svg msg
lindyPlatform =
    Svg.ellipse [ cx "229.6", cy "573.6", rx "90.1", ry "16.9", style "opacity:0.35;fill:#9F9FAA;enable-background:new" ] []


{-| -}
redHeadshot : Nri.Ui.Svg.V1.Svg
redHeadshot =
    Nri.Ui.Svg.V1.init ""
        []


{-| -}
redInstructive : Nri.Ui.Svg.V1.Svg
redInstructive =
    Nri.Ui.Svg.V1.init ""
        []


{-| -}
redSupportive : Nri.Ui.Svg.V1.Svg
redSupportive =
    Nri.Ui.Svg.V1.init ""
        []


{-| -}
salHeadshot : Nri.Ui.Svg.V1.Svg
salHeadshot =
    Nri.Ui.Svg.V1.init ""
        []


{-| -}
salInstructive : Nri.Ui.Svg.V1.Svg
salInstructive =
    Nri.Ui.Svg.V1.init ""
        []


{-| -}
salSupportive : Nri.Ui.Svg.V1.Svg
salSupportive =
    Nri.Ui.Svg.V1.init ""
        []
