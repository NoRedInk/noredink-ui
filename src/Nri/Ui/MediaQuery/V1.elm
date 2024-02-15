module Nri.Ui.MediaQuery.V1 exposing
    ( anyMotion, prefersReducedMotion
    , highContrastMode, notHighContrastMode
    , withViewport
    , mobile, notMobile
    , mobileBreakpoint
    , quizEngineMobile, notQuizEngineMobile
    , quizEngineBreakpoint
    , narrowMobile, notNarrowMobile
    , narrowMobileBreakpoint
    )

{-| Major version changes:

  - remove narrowMobileBreakPoint

Patch changes:

  - remove min-width:1 from media queries in order to support better composibility
  - adds narrowMobileBreakpoint and deprecates narrowMobileBreakPoint
  - adds withViewport for convenience when matching specific viewport size ranges
  - fix `not` queries to not overlap with the regular breakpoint queries

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
@docs narrowMobileBreakpoint

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
    withMediaQuery [ "(forced-colors: none)" ]


{-| -}
highContrastMode : List Style -> Style
highContrastMode =
    withMediaQuery [ "(forced-colors: active)" ]


{-| Styles using the `mobileBreakpoint` value as the maxWidth.
-}
mobile : MediaQuery
mobile =
    only screen [ maxWidth mobileBreakpoint ]


{-| Styles using the `mobileBreakpoint` value as the minWidth.
-}
notMobile : MediaQuery
notMobile =
    Css.Media.not screen [ maxWidth mobileBreakpoint ]


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
    Css.Media.not screen [ maxWidth quizEngineBreakpoint ]


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
    Css.Media.not screen [ maxWidth narrowMobileBreakpoint ]


{-| 500px
-}
narrowMobileBreakpoint : Css.Px
narrowMobileBreakpoint =
    px 500
