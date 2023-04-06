module Spec.PseudoElements exposing
    ( hasBefore, hasAfter
    , hasNotBefore, hasNotAfter
    )

{-|

@docs hasBefore, hasAfter
@docs hasNotBefore, hasNotAfter

-}

import Expect exposing (Expectation)
import Regex exposing (Regex)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)


hasBefore : String -> String -> Query.Single msg -> Expectation
hasBefore =
    hasPseudoElement "::before"


hasAfter : String -> String -> Query.Single msg -> Expectation
hasAfter =
    hasPseudoElement "::after"


hasNotBefore : String -> String -> Query.Single msg -> Expectation
hasNotBefore =
    hasNotPseudoElement "::before"


hasNotAfter : String -> String -> Query.Single msg -> Expectation
hasNotAfter =
    hasNotPseudoElement "::after"


hasPseudoElement : String -> String -> String -> Query.Single msg -> Expectation
hasPseudoElement pseudoElement highlightMarker relevantHighlightableText view =
    case pseudoElementSelector pseudoElement highlightMarker view of
        Just className ->
            Query.has
                [ className
                , Selector.containing [ Selector.text relevantHighlightableText ]
                ]
                view

        Nothing ->
            ("Expected to find a class defining a " ++ pseudoElement ++ " element with content: `")
                ++ highlightMarker
                ++ "`, but failed to find the class in the styles: \n\n"
                ++ rawStyles view
                |> Expect.fail


hasNotPseudoElement : String -> String -> String -> Query.Single msg -> Expectation
hasNotPseudoElement pseudoElement highlightMarker relevantHighlightableText view =
    case pseudoElementSelector pseudoElement highlightMarker view of
        Just className ->
            view
                |> Query.findAll [ className ]
                |> Query.each (Query.hasNot [ Selector.containing [ Selector.text relevantHighlightableText ] ])

        Nothing ->
            Expect.pass


pseudoElementSelector : String -> String -> Query.Single msg -> Maybe Selector
pseudoElementSelector pseudoElement highlightMarker view =
    let
        startHighlightClassRegex : Maybe Regex
        startHighlightClassRegex =
            ("\\.(\\_[a-zA-Z0-9]+)" ++ pseudoElement ++ "\\{content:\\\\\"\\s*\\s*")
                ++ highlightMarker
                |> Regex.fromString

        maybeClassName : Maybe String
        maybeClassName =
            startHighlightClassRegex
                |> Maybe.andThen
                    (\regex ->
                        Regex.find regex (rawStyles view)
                            |> List.head
                            |> Maybe.andThen (.submatches >> List.head)
                    )
                |> Maybe.withDefault Nothing
    in
    Maybe.map Selector.class maybeClassName


rawStyles : Query.Single msg -> String
rawStyles view =
    view
        |> Query.find [ Selector.tag "style" ]
        |> Query.children []
        |> Debug.toString
