module Nri.Ui.Fade.V1 exposing (fadeIn, fadeOut)

{-|

@docs fadeIn, fadeOut

-}

import Css
import Css.Animations


fadeDuration : Css.Style
fadeDuration =
    Css.animationDuration (Css.ms 1000)


fadeTimingFunction : Css.Style
fadeTimingFunction =
    Css.property "animation-timing-function" "linear"


{-| Add this style to the element you want to animate in.
-}
fadeIn : Css.Style
fadeIn =
    Css.batch
        [ fadeDuration
        , fadeTimingFunction
        , Css.property "animation-delay" "-50ms"
        , Css.animationName
            (Css.Animations.keyframes
                [ ( 0, [ Css.Animations.opacity Css.zero ] )
                , ( 100, [ Css.Animations.opacity (Css.num 1) ] )
                ]
            )
        ]


{-| Add this style to the element you want to animate out.
-}
fadeOut : Css.Style
fadeOut =
    Css.batch
        [ Css.position Css.absolute
        , Css.property "animation-delay" "-50ms"
        , Css.opacity Css.zero
        , Css.batch
            [ fadeDuration
            , fadeTimingFunction
            , Css.animationName
                (Css.Animations.keyframes
                    [ ( 0, [ Css.Animations.opacity (Css.num 1) ] )
                    , ( 100, [ Css.Animations.opacity Css.zero ] )
                    ]
                )
            ]
        ]
