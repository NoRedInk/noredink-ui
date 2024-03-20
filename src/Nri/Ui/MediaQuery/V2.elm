module Nri.Ui.MediaQuery.V2 exposing
    ( MediaQuery, toStyles, toStyle
    , not
    , mobile, narrowMobile, quizEngineMobile
    , prefersReducedMotion, highContrastMode
    )

{-| Patch changes:

<placeholder>

Changes from V1:

  - Use Attribute-style API instead of direct integration with `Css.Media` module. This
    abstracts away the ordering of media queries and resolves some footguns related to
    the previous way "not" media queries were handled (e.g. `notMobile` and `notNarrowMobile`,
    they were using the `not` keyword which caused a discrepancy between the device types
    affected. tl;dr: sometimes `content` and `display` rules affected screenreaders and printers,
    sometimes they didn't. In this version, media query rules are _strictly_ screen only).

Build media queries for responsive design.

    import Nri.Ui.MediaQuery.V2 as MediaQuery exposing (Attribute, mobile, narrowMobile, styles)

    MediaQuery.styles
        [ MediaQuery.mobile [ Css.paddingTop (Css.px 10) ]
        , MediaQuery.narrowMobile [ Css.paddingTop (Css.px 20) ]

        -- negate a media query with `not`
        , MediaQuery.not MediaQuery.mobile [ Css.fontSize (Css.px 30) ]
        ]


### Basics

@docs MediaQuery, toStyles, toStyle


### Utilities

@docs not


### Viewport Media Queries

@docs mobile, narrowMobile, quizEngineMobile


### Accessibility Media Queries

@docs prefersReducedMotion, highContrastMode

-}

import Css exposing (Style, target)
import Css.Media exposing (MediaQuery, maxWidth, minWidth, only, screen, withMedia, withMediaQuery)
import Maybe.Extra as Maybe


{-| Type representing a Media Query in the format

    `(Target, SatisfiesTargetStyles, DoesNotSatisfyTargetStyles)`, where:

    - `Target` is the media this query is targeting (e.g. Mobile, NarrowMobile, etc.)
    - `SatisfiesTargetStyles` is the style to apply when the media query is satisfied
    - `DoesNotSatisfyTargetStyles` is the style to apply when the media query is NOT satisfied

    Example:

    MediaQuery Mobile (Just [ Css.paddingTop (Css.px 10) ]) (Just [ Css.paddingTop (Css.px 20) ]

-}
type MediaQuery
    = MediaQuery Target (Maybe (List Style)) (Maybe (List Style))


type Target
    = Mobile
    | QuizEngineMobile
    | NarrowMobile
    | ReducedMotion
    | HighContrast


{-| Negate a MediaQuery

    Note: This is not a true "not" media query. Rather, this module will use whatever
    the logical inverse of the media query target is. For example, `not mobile` will
    not do `not screen and (max-width: 1000px)`, but rather `screen and (min-width: 1001px)`.
    This is because the `not` keyword in media queries causes some hard-to-track-down
    unexpected behavior with screen readers and printers.

-}
not : (List Style -> MediaQuery) -> List Style -> MediaQuery
not query s =
    let
        (MediaQuery target _ _) =
            query s
    in
    MediaQuery target Nothing (Just s)


{-| Set styles for mobile and smaller devices (<= 1000px)
-}
mobile : List Style -> MediaQuery
mobile s =
    MediaQuery Mobile (Just s) Nothing


{-| Set styles for quiz engine mobile and smaller devices (<= 750px)
-}
quizEngineMobile : List Style -> MediaQuery
quizEngineMobile s =
    MediaQuery QuizEngineMobile (Just s) Nothing


{-| Set styles for narrow mobile and smaller devices (<= 500px)
-}
narrowMobile : List Style -> MediaQuery
narrowMobile s =
    MediaQuery NarrowMobile (Just s) Nothing


{-| Set styles for reduced motion
-}
prefersReducedMotion : List Style -> MediaQuery
prefersReducedMotion s =
    MediaQuery ReducedMotion (Just s) Nothing


{-| Set styles for high contrast mode
-}
highContrastMode : List Style -> MediaQuery
highContrastMode s =
    MediaQuery HighContrast (Just s) Nothing


{-| Build a list of `Css.Style` from a list of media queries.
-}
toStyles : List MediaQuery -> List Style
toStyles queries =
    let
        config =
            List.foldl
                (\(MediaQuery target satisfiesTargetStyles doesNotSatisfyTargetStyles) acc ->
                    let
                        ( get, set ) =
                            case target of
                                Mobile ->
                                    ( .mobile, \r v -> { r | mobile = v } )

                                QuizEngineMobile ->
                                    ( .quizEngineMobile, \r v -> { r | quizEngineMobile = v } )

                                NarrowMobile ->
                                    ( .narrowMobile, \r v -> { r | narrowMobile = v } )

                                ReducedMotion ->
                                    ( .reducedMotion, \r v -> { r | reducedMotion = v } )

                                HighContrast ->
                                    ( .highContrast, \r v -> { r | highContrast = v } )

                        ( maybeExistingSatisfies, maybeExistingDoesNotSatisfy ) =
                            get acc
                    in
                    set acc
                        ( merge maybeExistingSatisfies satisfiesTargetStyles
                        , merge maybeExistingDoesNotSatisfy doesNotSatisfyTargetStyles
                        )
                )
                { mobile = ( Nothing, Nothing )
                , quizEngineMobile = ( Nothing, Nothing )
                , narrowMobile = ( Nothing, Nothing )
                , reducedMotion = ( Nothing, Nothing )
                , highContrast = ( Nothing, Nothing )
                }
                queries

        merge a b =
            Maybe.map ((++) (Maybe.withDefault [] a)) b |> Maybe.orElse a

        prepend =
            Maybe.cons

        append maybeItem list =
            Maybe.map (List.singleton >> List.append list) maybeItem |> Maybe.withDefault list

        mkBooleanQuery onQuery offQuery ( onStyles, offStyles ) =
            Maybe.values
                [ Maybe.map (withMediaQuery [ onQuery ]) onStyles
                , Maybe.map (withMediaQuery [ offQuery ]) offStyles
                ]

        mkViewportQuery rule px =
            Maybe.map <| withMedia [ only screen [ rule <| Css.px px ] ]

        addViewportQuery ( px, getStyles ) =
            let
                ( desktopFirst, mobileFirst ) =
                    getStyles config
            in
            prepend (mkViewportQuery maxWidth px desktopFirst) >> append (mkViewportQuery minWidth (px + 1) mobileFirst)
    in
    List.concat
        [ mkBooleanQuery "(prefers-reduced-motion)" "(prefers-reduced-motion: no-preference)" config.reducedMotion
        , mkBooleanQuery "(forced-colors: active)" "(forced-colors: none)" config.highContrast
        ]
        |> addViewportQuery ( 1000, .mobile )
        |> addViewportQuery ( 750, .quizEngineMobile )
        |> addViewportQuery ( 500, .narrowMobile )


{-| Build a single `Css.Style` from a list of media queries.

    This is a convenience function for `Css.batch (toStyles queries)`.

-}
toStyle : List MediaQuery -> Style
toStyle =
    toStyles >> Css.batch
