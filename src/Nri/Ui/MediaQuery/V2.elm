module Nri.Ui.MediaQuery.V2 exposing
    ( MediaQuery
    , mobile
    , narrowMobile
    , not
    , quizEngineMobile
    , toStyles
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
        , MediaQuery.not MediaQuery.mobile [ Css.fontSize (Css.px 30) ]
        ]

-}

import Css exposing (Style, target)
import Css.Media exposing (MediaQuery, maxWidth, minWidth, only, screen, withMedia)
import Maybe.Extra as Maybe


{-| Type representing a pair of styles for a given target.
-}
type MediaQuery
    = MediaQuery Target (Maybe (List Style)) (Maybe (List Style))


type Target
    = Mobile
    | QuizEngineMobile
    | NarrowMobile


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


{-| Build a `Css.Style` from a list of breakpoints
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
                    in
                    set acc
                        ( Maybe.or (Tuple.first (get acc)) satisfiesTargetStyles
                        , Maybe.or (Tuple.second (get acc)) doesNotSatisfyTargetStyles
                        )
                )
                { mobile = ( Nothing, Nothing )
                , quizEngineMobile = ( Nothing, Nothing )
                , narrowMobile = ( Nothing, Nothing )
                }
                queries

        mkStyle rule px =
            Maybe.map <| withMedia [ only screen [ rule <| Css.px px ] ]

        prepend =
            Maybe.cons

        append maybeItem list =
            Maybe.map (List.singleton >> List.append list) maybeItem |> Maybe.withDefault list

        addBreakpointStyles ( px, getStyles ) =
            let
                ( desktopFirst, mobileFirst ) =
                    getStyles config
            in
            prepend (mkStyle maxWidth px desktopFirst) >> append (mkStyle minWidth (px + 1) mobileFirst)
    in
    []
        |> addBreakpointStyles ( 1000, .mobile )
        |> addBreakpointStyles ( 750, .quizEngineMobile )
        |> addBreakpointStyles ( 500, .narrowMobile )
