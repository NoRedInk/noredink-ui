module Nri.Ui.Slide.V1 exposing
    ( AnimationDirection(..)
    , withSlidingContents
    , animateIn, animateOut
    )

{-|

@docs AnimationDirection
@docs withSlidingContents
@docs animateIn, animateOut

-}

import Css
import Css.Animations


{-| Slide from right to left or from left to right.
-}
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


{-| Add this class to the container whose descendents are sliding.
-}
withSlidingContents : Css.Style
withSlidingContents =
    Css.batch
        [ Css.position Css.relative
        , Css.overflowX Css.hidden
        ]


{-| Add this style to the element you want to animate in.
-}
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


{-| Add this style to the element you want to animate out.
Note: this will absolutely position the element.
You must add `withSlidingContents` to one of its ancestors.
-}
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
