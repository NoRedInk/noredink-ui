module Nri.Ui.MediaQuery.V2 exposing
    ( MediaQuery, fromList, toStyles, toStyle
    , not, offset
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

@docs mobile, narrowMobile, quizEngineMobile


### User Preferences

@docs prefersReducedMotion, highContrastMode

-}

import Css exposing (Style, target)
import Css.Media exposing (MediaQuery, maxWidth, minWidth, only, screen, withMedia, withMediaQuery)
import Dict exposing (Dict)
import Maybe.Extra as Maybe


{-| Type representing a Media Query in the format

    `(Target, SatisfiesTargetStyles, DoesNotSatisfyTargetStyles)`, where:

    - `Target` is the media this query is targeting (e.g. Mobile, NarrowMobile, etc.)
    - `SatisfiesTargetStyles` is the style to apply when the media query is satisfied
    - `DoesNotSatisfyTargetStyles` is the style to apply when the media query is NOT satisfied

    Example:

    MediaQuery Mobile (Just [ Css.paddingTop (Css.px 10) ]) (Just [ Css.paddingTop (Css.px 20) ]

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


breakpoint : Float -> List Style -> MediaQuery { properties | offsettable : () }
breakpoint px s =
    MediaQuery (Breakpoint px) (Just s) Nothing


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


{-| Set styles for reduced motion
-}
prefersReducedMotion : List Style -> MediaQuery {}
prefersReducedMotion =
    userPreference "(prefers-reduced-motion)" "(prefers-reduced-motion: no-preference)"


{-| Set styles for high contrast mode
-}
highContrastMode : List Style -> MediaQuery {}
highContrastMode =
    userPreference "(forced-colors: active)" "(forced-colors: none)"


type alias ResponsiveStyles =
    { breakpoints : Dict Float ( Maybe (List Style), Maybe (List Style) )
    , userPreferences : Dict String (List Style)
    }


init : ResponsiveStyles
init =
    { breakpoints = Dict.empty, userPreferences = Dict.empty }


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


{-| Build a list of `Css.Style` from a list of media queries.
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


{-| Build a single `Css.Style` from a list of media queries.

    This is a convenience function for `Css.batch (toStyles queries)`.

-}
toStyle : ResponsiveStyles -> Style
toStyle =
    toStyles >> Css.batch


{-| Build a single `Css.Style` from a list of media queries.

    This is a convenience function for `List.foldl MediaQuery.on MediaQuery.query >> MediaQuery.toStyle`

    In other words,

    ```
    MediaQuery.fromList
        [ MediaQuery.mobile [ Css.paddingTop (Css.px 10) ]
        , MediaQuery.narrowMobile [ Css.paddingTop (Css.px 20) ]
        ]
    ```

    is the same as

    ```
    MediaQuery.query
        |> MediaQuery.on (MediaQuery.mobile [ Css.paddingTop (Css.px 10) ])
        |> MediaQuery.on (MediaQuery.narrowMobile [ Css.paddingTop (Css.px 20) ])
        |> MediaQuery.toStyle
    ```

    Why do both of these exist?

    `fromList` is convenient, but restricts the use of phantom types. That is, you can't
    create a media query that contains both a breakpoint and a user preference. You can only
    create a media query that contains one or the other.

    This usually isn't a concern, but to enforce certain restrictions internally it
    makes more sense to design the API around pipeline-style usage and expose this
    convenience function for the common case.

-}
fromList : List (MediaQuery properties) -> Style
fromList =
    List.foldl on init >> toStyle
