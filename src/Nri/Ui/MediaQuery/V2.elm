module Nri.Ui.MediaQuery.V2 exposing
    ( MediaQuery, fromList, toStyles, toStyle
    , not, offset
    , breakpoint
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

@docs MediaQuery, fromList, init, on, toStyles, toStyle


### Utilities

@docs not, offset


### Breakpoint

@docs breakpoint
@docs mobile, narrowMobile, quizEngineMobile


### User Preferences

@docs prefersReducedMotion, highContrastMode

-}

import Css exposing (Style, target)
import Css.Media exposing (MediaQuery, maxWidth, minWidth, only, screen, withMedia, withMediaQuery)
import Dict exposing (Dict)
import Maybe.Extra as Maybe


{-| Type representing a media query.

    There is a lone constructor `MediaQuery` that takes 3 arguments:

    (Target, SatisfiesTargetStyles, DoesNotSatisfyTargetStyles)`

-}
type MediaQuery properties
    = MediaQuery Target (Maybe (List Style)) (Maybe (List Style))


type Target
    = Breakpoint Float
    | UserPreference String String


{-| Negate a MediaQuery

    Note: This is not a true "not" media query. Rather, this module will use whatever
    the logical inverse of the media query target is. For example, `not mobile` will
    not do `not screen and (max-width: 1000px)`, but rather `screen and (min-width: 1001px)`.
    This is because the `not` keyword in media queries causes some hard-to-track-down
    unexpected behavior with screen readers and printers.

-}
not : (List Style -> MediaQuery properties) -> List Style -> MediaQuery properties
not mq s =
    case mq s of
        MediaQuery target _ _ ->
            MediaQuery target Nothing (Just s)


{-| Offset a viewport breakpoint

    For example,

    `offset 20 mobile [ fontSize (px 20) ]`

    Will apply the `font-size: 20px` rule at the breakpoint 1020px.

    No effect if used with a non-breakpoint media query.

-}
offset : Float -> (List Style -> MediaQuery { properties | offsettable : () }) -> List Style -> MediaQuery { properties | offsettable : () }
offset px mq s =
    case mq s of
        MediaQuery (Breakpoint orig) _ _ ->
            MediaQuery (Breakpoint (max 0 (orig + px))) (Just s) Nothing

        other ->
            other


{-| Construct a media query for a viewport breakpoint

    For example,

    `breakpoint 1000 [ fontSize (px 20) ]`

    Will apply the `font-size: 20px` rule at the breakpoint 1000px.

-}
breakpoint : Float -> List Style -> MediaQuery { properties | offsettable : () }
breakpoint px s =
    MediaQuery (Breakpoint px) (Just s) Nothing


{-| Construct a media query for a user preference

    For example,

    `userPreference "(prefers-reduced-motion)" "(prefers-reduced-motion: no-preference)" [ fontSize (px 20) ]`

    Will apply the `font-size: 20px` rule when the user prefers reduced motion.

-}
userPreference : String -> String -> List Style -> MediaQuery properties
userPreference on_ off s =
    MediaQuery (UserPreference on_ off) (Just s) Nothing


{-| Set styles for mobile and smaller devices (<= 1000px)
-}
mobile : List Style -> MediaQuery { properties | offsettable : () }
mobile =
    breakpoint 1000


{-| Set styles for quiz engine mobile and smaller devices (<= 750px)
-}
quizEngineMobile : List Style -> MediaQuery { properties | offsettable : () }
quizEngineMobile =
    breakpoint 750


{-| Set styles for narrow mobile and smaller devices (<= 500px)
-}
narrowMobile : List Style -> MediaQuery { properties | offsettable : () }
narrowMobile =
    breakpoint 500


{-| Set styles for users who prefer reduced motion

    Generally, you will want to wrap any use of animations/transitions with

    `not (prefersReducedMotion [ ... ])`

-}
prefersReducedMotion : List Style -> MediaQuery {}
prefersReducedMotion =
    userPreference "(prefers-reduced-motion)" "(prefers-reduced-motion: no-preference)"


{-| Set styles for high contrast mode

    This media query indicates that the user has FORCED high contrast mode on.

    It is NOT the same as `prefers-color-scheme: high-contrast`.

    The practical difference is that this query targets users who are using high contrast
    or inverted colors at the system/browser level (e.g. Windows High Contrast mode,
    Mac OS Invert Colors), while `prefers-color-scheme: high-contrast` targets users who
    have expressed a preference for high contrast mode, but are otherwise seeing the same
    experience as everyone else.

    `prefers-color-scheme: high-contrast` is similar to `prefers-reduced-motion` and
    `prefers-color-scheme: dark` in that it is up to us to respect it and provide a
    high-contrast experience, while `forced-colors: active` is a signal that the user
    is using high contrast mode and intended to be used when there is a need to make
    more significant changes or compensate for a suboptimal experience for those users.

-}
highContrastMode : List Style -> MediaQuery {}
highContrastMode =
    userPreference "(forced-colors: active)" "(forced-colors: none)"


{-| A record representing the styles to apply at various breakpoints and user preferences.

    This is the internal representation of media queries. It is not recommended to use it directly.

    Instead, use `MediaQuery.fromList` or `MediaQuery.init |> MediaQuery.on ...`.

    `breakpoints` - Dict of breakpoints where the key is the pixel value and the value is a tuple
    of (Maybe (List Style), Maybe (List Style)) where the first item is the styles to apply when the
    media query is satisfied and the second item is the styles to apply when the media query is NOT
    satisfied.

    `userPreferences` - Dict of user preferences where the key is the media expression
    (e.g. "(prefers-reduced-motion)") and the value is a list of styles to apply when the
    media query is satisfied.

    This is the internal representation of media queries. It is not recommended to use it directly.

-}
type alias ResponsiveStyles =
    { breakpoints : Dict Float ( Maybe (List Style), Maybe (List Style) )
    , userPreferences : Dict String (List Style)
    }


{-| Initialize a ResponsiveStyles record that can be used to compose media queries.
-}
init : ResponsiveStyles
init =
    { breakpoints = Dict.empty, userPreferences = Dict.empty }


{-| Add a MediaQuery to a ResponsiveStyles record.

    This is the primary way to build a ResponsiveStyles record.

    For example,

    ```
    MediaQuery.init
        |> MediaQuery.on (MediaQuery.mobile [ Css.paddingTop (Css.px 10) ])
        |> MediaQuery.on (MediaQuery.narrowMobile [ Css.paddingTop (Css.px 20) ])
    ```

-}
on : MediaQuery properties -> ResponsiveStyles -> ResponsiveStyles
on mq internal =
    let
        merge a b =
            Maybe.map ((++) (Maybe.withDefault [] a)) b |> Maybe.orElse a
    in
    case mq of
        MediaQuery (Breakpoint px) satisfiesTargetStyles doesNotSatisfyTargetStyles ->
            { internal
                | breakpoints =
                    Dict.update px
                        (Maybe.withDefault ( Nothing, Nothing )
                            >> Tuple.mapBoth
                                (merge satisfiesTargetStyles)
                                (merge doesNotSatisfyTargetStyles)
                            >> Just
                        )
                        internal.breakpoints
            }

        MediaQuery (UserPreference onQuery offQuery) satisfiesTargetStyles doesNotSatisfyTargetStyles ->
            { internal
                | userPreferences =
                    internal.userPreferences
                        |> Dict.update onQuery (merge satisfiesTargetStyles)
                        |> Dict.update offQuery (merge doesNotSatisfyTargetStyles)
            }


{-| Build a list of `Css.Style` from a ResponsiveStyles record.

    Styles are output in the following order:

    1. User preferences
    2. Mobile-first breakpoints (min-width, ascending)
    3. Desktop-first breakpoints (max-width, descending)

    This ordering (at least for the breakpoints) is important to ensure the correct styles are applied.

    CASCADES, BABY!

-}
toStyles : ResponsiveStyles -> List Style
toStyles { breakpoints, userPreferences } =
    let
        prepend =
            Maybe.cons

        append maybeItem list =
            Maybe.map (List.singleton >> List.append list) maybeItem |> Maybe.withDefault list

        mkUserPreferenceQuery ( preference, styles ) =
            withMediaQuery [ preference ] styles

        mkViewportQuery rule px =
            Maybe.map <| withMedia [ only screen [ rule <| Css.px px ] ]

        addViewportQuery ( px, ( desktopFirst, mobileFirst ) ) =
            prepend (mkViewportQuery maxWidth px desktopFirst) >> append (mkViewportQuery minWidth (px + 1) mobileFirst)
    in
    List.map mkUserPreferenceQuery (Dict.toList userPreferences)
        ++ List.foldr addViewportQuery [] (Dict.toList breakpoints |> List.sortBy Tuple.first)


{-| Build a single `Css.Style` from a ResponsiveStyles record.

    Styles are output in the following order:

    1. User preferences
    2. Mobile-first breakpoints (min-width, ascending)
    3. Desktop-first breakpoints (max-width, descending)

    This ordering (at least for the breakpoints) is important to ensure the correct styles are applied.

    This is a convenience function for `Css.batch (toStyles queries)`.

-}
toStyle : ResponsiveStyles -> Style
toStyle =
    toStyles >> Css.batch


{-| Build a single `Css.Style` from a list of media queries.

    !!!  ARE YOU TRYING TO USE THIS FUNCTION AND GETTING A TYPE ERROR ON YOUR LIST OF MEDIA QUERIES? READ THIS! !!!

    This module is designed with a pipeline-style API in mind, and uses phantom types to enforce certain restrictions.

    These phantom types prevent using modifiers like `not` and `offset` with media queries that are not designed to accept them.

    This function is an alias for `List.foldl on init >> toStyle` provided for convenience and the common case.

    If you're stuck, you have 2 options:

    1. Use 2 independent calls to `MediaQuery.fromList`, one with your breakpoints and one with your user preferences.

    2. Use the pipeline-style API directly.

        For example,

        ```
        MediaQuery.fromList
            [ MediaQuery.mobile [ Css.paddingTop (Css.px 10) ]
            , MediaQuery.narrowMobile [ Css.paddingTop (Css.px 20) ]
            ]
        ```

        translates to

        ```
        MediaQuery.query
            |> MediaQuery.on (MediaQuery.mobile [ Css.paddingTop (Css.px 10) ])
            |> MediaQuery.on (MediaQuery.narrowMobile [ Css.paddingTop (Css.px 20) ])
            |> MediaQuery.toStyle
        ```

-}
fromList : List (MediaQuery properties) -> Style
fromList =
    List.foldl on init >> toStyle
