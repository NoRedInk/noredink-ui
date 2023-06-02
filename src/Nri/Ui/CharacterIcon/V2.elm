module Nri.Ui.CharacterIcon.V2 exposing
    ( lindyInstructive, lindySupportive
    , redInstructive
    , salInstructive, salSupportive
    )

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
            [ {- "M175.1,548.7c0,0,6.3,9.7,19.7,10.9c13.4,1.2,21.8,0.9,21.8,0.9s-5,26.1-22.1,27.7c-17.1,1.5-55.8,2.1-51.3-12.5C147.7,561,175.1,548.7,175.1,548.7zs"
                 ,
              -}
              "M281.2,548.7c0,0-6.3,9.7-19.7,10.9s-21.8,0.9-21.8,0.9s5,26.1,22.1,27.7c17.1,1.5,55.8,2.1,51.3-12.5S281.2,548.7,281.2,548.7L281.2,548.7z"
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


{-| -}
redHeadshot : Nri.Ui.Svg.V1.Svg
redHeadshot =
    Nri.Ui.Svg.V1.init ""
        []


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
    , mouthPath : String
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
        stroke =
            { black = Css.property "stroke" "#000000"
            , width = \x -> Css.property "stroke-width" (String.fromFloat x)
            }

        mask name =
            Css.property "mask" ("url(#" ++ config.name ++ name ++ ")")

        fill =
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
            }
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
            , Svg.path [ d config.mouthPath, css [ fill.dark ] ] []
            , Svg.path [ d config.nosePath, css [ fill.iris ] ] []
            , Svg.g [ css [ fill.none, stroke.black, stroke.width 1.87 ] ] [ config.noseBridgeLine ]
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


{-| -}
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
            [ "172.9,456.3 169.4,431.1 192.5,437.6 192.5,462.1 "
            , "213.1,457.6 205.3,431.1 227.4,433.3 229.3,462.4 "
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
        , mouthPath = "M189.6,320.9c2.7,1.9,5.8,2.5,8.9,2.6c3.1,0.1,6.2-0.1,9.1-1c6.1-1.9,10.6-6.6,13.5-12.2c-1.2,6.6-6.3,12.6-12.7,14.7C202.2,326.9,193.5,326.6,189.6,320.9L189.6,320.9L189.6,320.9z"
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


{-| -}
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


{-| -}
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
