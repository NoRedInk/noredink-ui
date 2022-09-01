module Nri.Ui.AnimatedIcon.V1 exposing (mobileOpenClose)

import Css
import Css.Animations
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Svg.V1
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attributes


animatedXHamburger : List ( List Css.Animations.Property, List Css.Animations.Property ) -> Nri.Ui.Svg.V1.Svg
animatedXHamburger lineAnimations =
    let
        animate : ( List Css.Animations.Property, List Css.Animations.Property ) -> Svg.Attribute Never
        animate ( start, end ) =
            Attributes.css
                [ Css.animationDuration (Css.ms 300)
                , Css.property "animation-timing-function" "linear"
                , Css.property "animation-fill-mode" "forwards"
                , Css.animationName
                    (Css.Animations.keyframes
                        [ ( 0, start )
                        , ( 20, [ Css.Animations.custom "width" "25px" ] )
                        , ( 80, [ Css.Animations.custom "width" "25px" ] )
                        , ( 100, end )
                        ]
                    )
                ]

        line ( x_, y ) animation =
            Svg.rect
                [ Attributes.x (String.fromFloat x_)
                , Attributes.y (String.fromFloat y)
                , Attributes.width "25"
                , Attributes.height "5"
                , Attributes.rx "2.5"
                , animate animation
                ]
                []
    in
    Nri.Ui.Svg.V1.init "0 0 25 27"
        (List.map2 identity
            [ line ( 0, 0 ), line ( 0, 10 ), line ( 0, 20 ) ]
            lineAnimations
        )


topLineAsX : List Css.Animations.Property
topLineAsX =
    [ Css.Animations.transform
        [ Css.rotate (Css.deg 45)
        , Css.translate3d (Css.px 3) (Css.px -1) Css.zero
        ]
    , Css.Animations.custom "width" "32px"
    ]


middleLineAsX : List Css.Animations.Property
middleLineAsX =
    [ Css.Animations.opacity (Css.num 0) ]


bottomLineAsX : List Css.Animations.Property
bottomLineAsX =
    [ Css.Animations.transform
        [ Css.translate3d (Css.px -15) (Css.px 7.5) Css.zero
        , Css.rotate (Css.deg -45)
        ]
    , Css.Animations.custom "width" "32px"
    ]


{-| -}
mobileOpenClose : Bool -> Nri.Ui.Svg.V1.Svg
mobileOpenClose isOpen =
    animatedXHamburger
        [ ( topLineAsX, [] )
        , ( middleLineAsX, [] )
        , ( bottomLineAsX, [] )
        ]
