module Nri.Ui.MediaQuery.V2 exposing
    ( Attribute, styles
    , mobile, quizEngineMobile, narrowMobile
    , notMobile, notQuizEngineMobile, notNarrowMobile
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
        ]

@docs Attribute, styles

@docs mobile, quizEngineMobile, narrowMobile
@docs notMobile, notQuizEngineMobile, notNarrowMobile

-}

import Css exposing (Style)
import Css.Media exposing (MediaQuery, maxWidth, minWidth, only, screen, withMedia)
import Maybe.Extra as Maybe


{-| Media query attribute, contains CSS styles for a given media query.
-}
type Attribute
    = Attribute (MediaQuery -> MediaQuery)


{-| Represents a pair of styles for a given breakpoint, where the first
element is the styles to apply when the viewport is less than or equal to
the breakpoint, and the second element is the styles to apply when the
viewport is greater than the breakpoint.
-}
type alias BreakpointStyles =
    ( Maybe (List Style), Maybe (List Style) )


type alias MediaQuery =
    { mobile : BreakpointStyles
    , quizEngineMobile : BreakpointStyles
    , narrowMobile : BreakpointStyles
    }


{-| Set styles for mobile and smaller devices (<= 1000px)
-}
mobile : List Style -> Attribute
mobile s =
    Attribute (\q -> { q | mobile = ( Just s, Tuple.second q.mobile ) })


{-| Set styles for larger-than-mobile devices (> 1000px)
-}
notMobile : List Style -> Attribute
notMobile s =
    Attribute (\q -> { q | mobile = ( Tuple.first q.mobile, Just s ) })


{-| Set styles for quiz engine mobile and smaller devices (<= 750px)
-}
quizEngineMobile : List Style -> Attribute
quizEngineMobile s =
    Attribute (\q -> { q | quizEngineMobile = ( Just s, Tuple.second q.quizEngineMobile ) })


{-| Set styles for larger-than-quiz-engine-mobile devices (> 750px)
-}
notQuizEngineMobile : List Style -> Attribute
notQuizEngineMobile s =
    Attribute (\b -> { b | quizEngineMobile = ( Tuple.first b.quizEngineMobile, Just s ) })


{-| Set styles for narrow mobile and smaller devices (<= 500px)
-}
narrowMobile : List Style -> Attribute
narrowMobile s =
    Attribute (\b -> { b | narrowMobile = ( Just s, Tuple.second b.narrowMobile ) })


{-| Set styles for larger-than-narrow-mobile devices (> 500px)
-}
notNarrowMobile : List Style -> Attribute
notNarrowMobile s =
    Attribute (\b -> { b | narrowMobile = ( Tuple.first b.narrowMobile, Just s ) })


{-| Build a `Css.Style` from a list of breakpoints
-}
styles : List Attribute -> Style
styles attributes =
    let
        config =
            List.foldl (\(Attribute f) -> f)
                { mobile = ( Nothing, Nothing )
                , quizEngineMobile = ( Nothing, Nothing )
                , narrowMobile = ( Nothing, Nothing )
                }
                attributes

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
        |> Css.batch
