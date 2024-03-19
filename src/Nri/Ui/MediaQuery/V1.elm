module Nri.Ui.MediaQuery.V1 exposing
    ( anyMotion, prefersReducedMotion
    , highContrastMode, notHighContrastMode
    , withViewport
    , Builder, builder, toStyles
    , onMobile, onQuizEngineMobile, onNarrowMobile, onDesktop
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
  - adds `Builder` for more efficient and fool-proof media query construction

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


### Builder

@docs Builder, builder, toStyles
@docs onMobile, onQuizEngineMobile, onNarrowMobile, onDesktop


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
import Maybe.Extra as Maybe


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

    Note: This does *not* use the min-width query, rather it uses the max-width query
    with `not`. This has implications for screen readers and print–mobile is ignored by
    these media types, but included for notMobile. Unfortunately, elm-css does not have
    `all` as a MediaType (and no means to use a custom one).

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

    Note: This does *not* use the min-width query, rather it uses the max-width query
    with `not`. This has implications for screen readers and print–mobile is ignored by
    these media types, but included for notMobile. Unfortunately, elm-css does not have
    `all` as a MediaType (and no means to use a custom one).

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

    Note: This does *not* use the min-width query, rather it uses the max-width query
    with `not`. This has implications for screen readers and print–mobile is ignored by
    these media types, but included for notMobile. Unfortunately, elm-css does not have
    `all` as a MediaType (and no means to use a custom one).

-}
notNarrowMobile : MediaQuery
notNarrowMobile =
    Css.Media.not screen [ maxWidth narrowMobileBreakpoint ]


{-| 500px
-}
narrowMobileBreakpoint : Css.Px
narrowMobileBreakpoint =
    px 500


{-| A builder for creating efficient media queries.

    Removes the need to think about the order of media queries or the nuances of
    `not` queries with respect to screen readers and print.

-}
type alias Builder =
    { baseStyles : List Style
    , mobileStyles : Maybe (List Style)
    , quizEngineStyles : Maybe (List Style)
    , narrowMobileStyles : Maybe (List Style)
    , desktopStyles : Maybe (List Style)
    }


{-| Initialize a desktop-first media query builder with the given base styles.
-}
builder : List Style -> Builder
builder baseStyles =
    { baseStyles = baseStyles
    , mobileStyles = Nothing
    , quizEngineStyles = Nothing
    , narrowMobileStyles = Nothing
    , desktopStyles = Nothing
    }


{-| Add mobile styles to the builder.

    Smaller breakpoints will inherit these styles unless overridden.

-}
onMobile : List Style -> Builder -> Builder
onMobile mobileStyles b =
    { b | mobileStyles = Just mobileStyles }


{-| Add quiz engine mobile styles to the builder.

    Smaller breakpoints will inherit these styles unless overridden.

-}
onQuizEngineMobile : List Style -> Builder -> Builder
onQuizEngineMobile quizEngineStyles b =
    { b | quizEngineStyles = Just quizEngineStyles }


{-| Add narrow mobile styles to the builder.

    Smaller breakpoints will inherit these styles unless overridden.

-}
onNarrowMobile : List Style -> Builder -> Builder
onNarrowMobile narrowMobileStyles b =
    { b | narrowMobileStyles = Just narrowMobileStyles }


{-| Add desktop ONLY styles to the builder.

    Smaller breakpoints will NOT inherit these styles. For styles that should apply across
    all breakpoints, use the base styles.

-}
onDesktop : List Style -> Builder -> Builder
onDesktop desktopStyles b =
    { b | desktopStyles = Just desktopStyles }


{-| Generate a list of styles from the a builder.
-}
toStyles : Builder -> List Style
toStyles b =
    let
        maybeAddBreakpoint breakpoint maybeStyles =
            -- Yes, cons adds to the front of the list.
            -- Yes, these are in the wrong order for a media query when you consider that fact.
            -- Yes, this is intentional.
            --
            -- elm-css reverses the order of media queries when it generates the CSS.
            --
            -- Very confusing, I know.
            --
            -- That's why we have a builder.
            Maybe.cons (Maybe.map (withMedia [ breakpoint ]) maybeStyles)
    in
    b.baseStyles
        |> maybeAddBreakpoint mobile b.mobileStyles
        |> maybeAddBreakpoint quizEngineMobile b.quizEngineStyles
        |> maybeAddBreakpoint narrowMobile b.narrowMobileStyles
        -- Don't use notMobile here, lest `onDesktop` will behave inconsistently with
        -- the other builder functions with respect to screen readers and print.
        -- ONLY things in the base styles should be visible to these other media types.
        |> maybeAddBreakpoint (only screen [ minWidth (px 1001) ]) b.desktopStyles
