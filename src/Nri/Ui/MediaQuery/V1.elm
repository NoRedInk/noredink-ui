module Nri.Ui.MediaQuery.V1 exposing
    ( anyMotion, prefersReducedMotion
    , highContrastMode, notHighContrastMode
    , withViewport
    , mobile, notMobile
    , mobileBreakpoint
    , quizEngineMobile, notQuizEngineMobile
    , quizEngineBreakpoint
    , narrowMobile, notNarrowMobile
    , narrowMobileBreakpoint, narrowMobileBreakPoint
    )

{-| Patch changes:

  - remove min-width:1 from media queries in order to support better composibility
  - adds narrowMobileBreakpoint and deprecates narrowMobileBreakPoint
  - adds withViewport for convenience when matching specific viewport size ranges

Standard media queries for responsive pages.

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
@docs highContrastMode, notHighContrastMode

@docs withViewport


### 1000px breakpoint

@docs mobile, notMobile
@docs mobileBreakpoint


### 750px breakpoint

@docs quizEngineMobile, notQuizEngineMobile
@docs quizEngineBreakpoint


### 500px breakpoint

@docs narrowMobile, notNarrowMobile
@docs narrowMobileBreakpoint, narrowMobileBreakPoint

-}

import Css exposing (Style, px)
import Css.Media exposing (MediaQuery, maxWidth, minWidth, only, screen, withMedia, withMediaQuery)


{-|

    MediaQuery.withViewport
        MediaQuery.narrowMobileBreakpoint
        MediaQuery.quizEngineBreakpoint
        [ Css.flexDirection Css.column
        ]

-}
withViewport : Maybe Css.Px -> Maybe Css.Px -> List Style -> Style
withViewport maybeMin maybeMax =
    withMedia
        [ only screen
            (List.filterMap identity
                [ Maybe.map minWidth maybeMin, Maybe.map maxWidth maybeMax ]
            )
        ]


{-| -}
anyMotion : List Style -> Style
anyMotion =
    withMediaQuery [ "(prefers-reduced-motion: no-preference)" ]


{-| -}
prefersReducedMotion : List Style -> Style
prefersReducedMotion =
    withMediaQuery [ "(prefers-reduced-motion)" ]


{-| -}
notHighContrastMode : List Style -> Style
notHighContrastMode =
    withMediaQuery [ "(forced-colors: none) and (prefers-contrast: no-preference)" ]


{-| -}
highContrastMode : List Style -> Style
highContrastMode =
    -- works on Windows and macOS, but not on Chrome OS
    withMediaQuery [ "(forced-colors: active) or (prefers-contrast: more)" ]


{-| Styles using the `mobileBreakpoint` value as the maxWidth.
-}
mobile : MediaQuery
mobile =
    only screen [ maxWidth mobileBreakpoint ]


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
    only screen [ maxWidth quizEngineBreakpoint ]


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


{-| Styles using the `narrowMobileBreakpoint` value as the maxWidth
-}
narrowMobile : MediaQuery
narrowMobile =
    only screen [ maxWidth narrowMobileBreakpoint ]


{-| Styles using the `quizEngineBreakpoint` value as the minWidth.
-}
notNarrowMobile : MediaQuery
notNarrowMobile =
    only screen [ minWidth narrowMobileBreakpoint ]


{-| 500px
-}
narrowMobileBreakpoint : Css.Px
narrowMobileBreakpoint =
    px 500


{-| DEPRECATED: prefer narrowMobileBreakpoint, which follows other casing conventions.
-}
narrowMobileBreakPoint : Css.Px
narrowMobileBreakPoint =
    px 500
