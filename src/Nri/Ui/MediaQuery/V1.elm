module Nri.Ui.MediaQuery.V1 exposing
    ( mobile, notMobile
    , mobileBreakpoint
    , quizEngineMobile
    , quizEngineBreakpoint
    , narrowMobile
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

@docs mobile, notMobile
@docs mobileBreakpoint

@docs quizEngineMobile
@docs quizEngineBreakpoint

@docs narrowMobile
@docs narrowMobileBreakPoint

-}

import Css exposing (px)
import Css.Media exposing (MediaQuery, maxWidth, minWidth, only, screen)


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


{-| 500px
-}
narrowMobileBreakPoint : Css.Px
narrowMobileBreakPoint =
    px 500
