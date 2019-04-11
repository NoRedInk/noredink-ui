module Nri.Ui.Slide.V1 exposing
    ( AnimationDirection(..)
    , animateIn
    , animateOut
    , withSlidingContents
    )

import Css
import Css.Animations


type AnimationDirection
    = FromRTL
    | FromLTR


translateXBy : Float
translateXBy =
    700


slideDuration : Css.Style
slideDuration =
    Css.animationDuration (Css.ms 1000)


slideTimingFunction : Css.Style
slideTimingFunction =
    Css.property "animation-timing-function" "ease-in-out"


withSlidingContents : Css.Style
withSlidingContents =
    Css.batch
        [ Css.position Css.relative
        , Css.overflowX Css.hidden
        ]


animateIn : AnimationDirection -> Css.Style
animateIn direction =
    let
        ( start, end ) =
            case direction of
                FromRTL ->
                    ( Css.px translateXBy, Css.zero )

                FromLTR ->
                    ( Css.px -translateXBy, Css.zero )
    in
    Css.batch
        [ slideDuration
        , slideTimingFunction
        , Css.property "animation-delay" "-50ms"
        , Css.animationName
            (Css.Animations.keyframes
                [ ( 0, [ Css.Animations.transform [ Css.translateX start ] ] )
                , ( 100, [ Css.Animations.transform [ Css.translateX end ] ] )
                ]
            )
        ]


animateOut : AnimationDirection -> Css.Style
animateOut direction =
    let
        ( start, end ) =
            case direction of
                FromRTL ->
                    ( Css.zero, Css.px -translateXBy )

                FromLTR ->
                    ( Css.zero, Css.px translateXBy )
    in
    Css.batch
        [ Css.position Css.absolute
        , Css.transform (Css.translate2 end Css.zero)
        , Css.property "animation-delay" "-50ms"
        , Css.batch
            [ slideDuration
            , slideTimingFunction
            , Css.animationName
                (Css.Animations.keyframes
                    [ ( 0, [ Css.Animations.transform [ Css.translateX start ] ] )
                    , ( 100, [ Css.Animations.transform [ Css.translateX end ] ] )
                    ]
                )
            ]
        ]
