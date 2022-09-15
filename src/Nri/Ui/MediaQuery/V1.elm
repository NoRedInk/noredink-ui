module Nri.Ui.MediaQuery.V1 exposing
    ( anyMotion, prefersReducedMotion
    , highContrastMode
    , mobile, notMobile
    , mobileBreakpoint
    , quizEngineMobile, notQuizEngineMobile
    , quizEngineBreakpoint
    , narrowMobile, notNarrowMobile
    , narrowMobileBreakPoint
    )

{-| Standard media queries for responsive pages.

    import Css
    import Css.Media as Media
    import Nri.Ui.MediaQuery.V1 as MediaQuery

    style : Css.Style
    style =
        Media.withMedia
            [ MediaQuery.mobile ]
            [ Css.padding (Css.px 2)
            ]

@docs anyMotion, prefersReducedMotion
@docs highContrastMode


### 1000px breakpoint

@docs mobile, notMobile
@docs mobileBreakpoint


### 750px breakpoint

@docs quizEngineMobile, notQuizEngineMobile
@docs quizEngineBreakpoint


### 500px breakpoint

@docs narrowMobile, notNarrowMobile
@docs narrowMobileBreakPoint

-}

import Css exposing (Style, px)
import Css.Media exposing (MediaQuery, maxWidth, minWidth, only, screen, withMediaQuery)


{-| -}
anyMotion : List Style -> Style
anyMotion =
    withMediaQuery [ "(prefers-reduced-motion: no-preference)" ]


{-| -}
prefersReducedMotion : List Style -> Style
prefersReducedMotion =
    withMediaQuery [ "(prefers-reduced-motion)" ]


{-| -}
highContrastMode : List Style -> Style
highContrastMode =
    withMediaQuery [ "(forced-colors: active)" ]


{-| Styles using the `mobileBreakpoint` value as the maxWidth.
-}
mobile : MediaQuery
mobile =
    only screen
        [ --`minWidth (px 1)` is for a bug in IE which causes the media query to initially trigger regardless of window size
          --See: <http://stackoverflow.com/questions/25673707/ie11-triggers-css-transition-on-page-load-when-non-applied-media-query-exists/25850649#25850649>
          minWidth (px 1)
        , maxWidth mobileBreakpoint
        ]


{-| Styles using the `mobileBreakpoint` value as the minWidth.
-}
notMobile : MediaQuery
notMobile =
    only screen [ minWidth mobileBreakpoint ]


{-| 1000px
-}
mobileBreakpoint : Css.Px
mobileBreakpoint =
    px 1000


{-| Styles using the `quizEngineBreakpoint` value as the maxWidth.
-}
quizEngineMobile : MediaQuery
quizEngineMobile =
    only screen
        [ --`minWidth (px 1)` is for a bug in IE which causes the media query to initially trigger regardless of window size
          --See: <http://stackoverflow.com/questions/25673707/ie11-triggers-css-transition-on-page-load-when-non-applied-media-query-exists/25850649#25850649>
          minWidth (px 1)
        , maxWidth quizEngineBreakpoint
        ]


{-| Styles using the `quizEngineBreakpoint` value as the minWidth.
-}
notQuizEngineMobile : MediaQuery
notQuizEngineMobile =
    only screen [ minWidth quizEngineBreakpoint ]


{-| 750px
-}
quizEngineBreakpoint : Css.Px
quizEngineBreakpoint =
    px 750


{-| Styles using the `narrowMobileBreakPoint` value as the maxWidth
-}
narrowMobile : MediaQuery
narrowMobile =
    only screen
        [ --`minWidth (px 1)` is for a bug in IE which causes the media query to initially trigger regardless of window size
          --See: <http://stackoverflow.com/questions/25673707/ie11-triggers-css-transition-on-page-load-when-non-applied-media-query-exists/25850649#25850649>
          minWidth (px 1)
        , maxWidth narrowMobileBreakPoint
        ]


{-| Styles using the `quizEngineBreakpoint` value as the minWidth.
-}
notNarrowMobile : MediaQuery
notNarrowMobile =
    only screen [ minWidth narrowMobileBreakPoint ]


{-| 500px
-}
narrowMobileBreakPoint : Css.Px
narrowMobileBreakPoint =
    px 500
