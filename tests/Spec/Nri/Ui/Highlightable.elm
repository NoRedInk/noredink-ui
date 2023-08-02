module Spec.Nri.Ui.Highlightable exposing (spec)

import Expect
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Highlightable.V3 as Highlightable exposing (Type(..))
import Nri.Ui.HighlighterTool.V1 as Tool
import Sort
import Sort.Set
import Test exposing (..)


spec : Test
spec =
    describe "Highlightable"
        [ describe "fromMarkdown" fromMarkdownSpec
        , describe "joinAdjacentInteractiveHighlights" joinAdjacentInteractiveHighlightsSpec
        , describe "usedMarkers" usedMarkersSpec
        , describe "asFragmentTuples" asFragmentTuplesSpec
        , describe "byId" byIdSpec
        ]


fromMarkdownSpec : List Test
fromMarkdownSpec =
    let
        testFromMarkdown startingString expected =
            Highlightable.fromMarkdown startingString
                |> Expect.all
                    [ -- the right words are marked
                      List.map (\{ text, marked } -> ( text, marked ))
                        >> Expect.equal expected
                    , -- the indexing is correct
                      \highlightables ->
                        Expect.equal
                            (List.indexedMap (\index _ -> index) highlightables)
                            (List.map (\{ index } -> index) highlightables)
                    ]

        defaultMark =
            Tool.buildMarker
                { highlightColor = Colors.highlightYellow
                , hoverColor = Colors.highlightYellow
                , hoverHighlightColor = Colors.highlightYellow
                , kind = ()
                , name = Nothing
                }
    in
    [ test "with an empty string, produces empty list of Highlightables" <|
        \() ->
            testFromMarkdown "" []
    , test "does not mark non-highlighted content" <|
        \() ->
            testFromMarkdown "A sentence without highlighted content"
                [ ( "A sentence without highlighted content", [] ) ]
    , test "does not strip emphasized content" <|
        \() ->
            testFromMarkdown "A *sentence without* **highlighted content**"
                [ ( "A *sentence without* **highlighted content**", [] ) ]
    , test "marks a single segment as highlighted" <|
        \() ->
            testFromMarkdown "[fake link]()"
                [ ( "fake link", [ defaultMark ] ) ]
    , test "does mark content that's intended to be highlighted" <|
        \() ->
            testFromMarkdown "A sentence with [highlighted content]()"
                [ ( "A sentence with ", [] )
                , ( "highlighted content", [ defaultMark ] )
                ]
    , test "marks content that's intended to be highlighted within an emphasis" <|
        \() ->
            testFromMarkdown "A sentence with *emphasized [highlighted content]()!*"
                [ ( "A sentence with *emphasized *", [] )
                , ( "*highlighted content*", [ defaultMark ] )
                , ( "*!*", [] )
                ]
    , test "marks content that's intended to be highlighted that also contains an emphasis" <|
        \() ->
            testFromMarkdown "A sentence with [highlighted *and partially emphasized* content]()"
                [ ( "A sentence with ", [] )
                , ( "highlighted *and partially emphasized* content", [ defaultMark ] )
                ]
    , test "does not highlight actual links" <|
        \() ->
            testFromMarkdown "I am a [real link](google.com)"
                [ ( "I am a [real link](google.com)", [] )
                ]
    , test "does not get confused by parentheses" <|
        \() ->
            testFromMarkdown "main thought (parenthetical)"
                [ ( "main thought (parenthetical)", [] )
                ]
    ]


joinAdjacentInteractiveHighlightsSpec : List Test
joinAdjacentInteractiveHighlightsSpec =
    let
        init init_ marks index =
            init_ marks index "Content"

        expectJoinsTo sorter expected starting =
            let
                processedExpected =
                    List.indexedMap (\i f -> f i) expected
                        |> Highlightable.asFragmentTuples
            in
            List.indexedMap (\i f -> f i) starting
                |> Highlightable.joinAdjacentInteractiveHighlights sorter
                |> Highlightable.asFragmentTuples
                |> Expect.equal processedExpected

        unitSorter =
            Sort.custom (\() () -> EQ)
    in
    [ test "without any segments, does nothing" <|
        \() ->
            []
                |> expectJoinsTo unitSorter []
    , test "with 1 interactive highlighted segment, does nothing" <|
        \() ->
            let
                segment =
                    init Highlightable.initInteractive [ mark () ]
            in
            [ segment ]
                |> expectJoinsTo unitSorter [ segment ]
    , test "with 1 interactive unmarked segment, does nothing" <|
        \() ->
            let
                segment =
                    init Highlightable.initInteractive []
            in
            [ segment ]
                |> expectJoinsTo unitSorter [ segment ]
    , test "with 1 static highlighted segment, does nothing" <|
        \() ->
            let
                segment =
                    init Highlightable.initStatic [ mark () ]
            in
            [ segment ]
                |> expectJoinsTo unitSorter [ segment ]
    , test "with 1 static unmarked segment, does nothing" <|
        \() ->
            let
                segment =
                    init Highlightable.initStatic []
            in
            [ segment ]
                |> expectJoinsTo unitSorter [ segment ]
    , test "with only 2 segments [highlighted interactive, plain static], does nothing" <|
        \() ->
            let
                segments =
                    [ init Highlightable.initInteractive [ mark () ]
                    , init Highlightable.initStatic []
                    ]
            in
            segments
                |> expectJoinsTo unitSorter segments
    , test "with only 2 segments [plain static, highlighted interactive], does nothing" <|
        \() ->
            let
                segments =
                    [ init Highlightable.initStatic []
                    , init Highlightable.initInteractive [ mark () ]
                    ]
            in
            segments
                |> expectJoinsTo unitSorter segments
    , test "with 3 segments, highlighted interactive sandwiching a plain unmarked, marks the plain segmnet" <|
        \() ->
            [ init Highlightable.initInteractive [ mark () ]
            , init Highlightable.initStatic []
            , init Highlightable.initInteractive [ mark () ]
            ]
                |> expectJoinsTo unitSorter
                    [ init Highlightable.initInteractive [ mark () ]
                    , init Highlightable.initStatic [ mark () ]
                    , init Highlightable.initInteractive [ mark () ]
                    ]
    , test "with 3 segments with highlights of various types, marks the plain segment correctly" <|
        \() ->
            [ init Highlightable.initInteractive [ mark "A", mark "B", mark "C" ]
            , init Highlightable.initStatic []
            , init Highlightable.initInteractive [ mark "A", mark "C" ]
            ]
                |> expectJoinsTo Sort.alphabetical
                    [ init Highlightable.initInteractive [ mark "A", mark "B", mark "C" ]
                    , init Highlightable.initStatic [ mark "A", mark "C" ]
                    , init Highlightable.initInteractive [ mark "A", mark "C" ]
                    ]
    , test "with many segments, marks plain sandwiched segments" <|
        \() ->
            [ init Highlightable.initStatic []
            , init Highlightable.initInteractive [ mark () ]
            , init Highlightable.initStatic []
            , init Highlightable.initStatic []
            , init Highlightable.initStatic []
            , init Highlightable.initInteractive [ mark () ]
            , init Highlightable.initStatic []
            ]
                |> expectJoinsTo unitSorter
                    [ init Highlightable.initStatic []
                    , init Highlightable.initInteractive [ mark () ]
                    , init Highlightable.initStatic [ mark () ]
                    , init Highlightable.initStatic [ mark () ]
                    , init Highlightable.initStatic [ mark () ]
                    , init Highlightable.initInteractive [ mark () ]
                    , init Highlightable.initStatic []
                    ]
    , test "with multiple highlighted interactive segments not all of the same type, only joins the types that match" <|
        \() ->
            [ init Highlightable.initInteractive [ mark "A", mark "B" ]
            , init Highlightable.initStatic []
            , init Highlightable.initInteractive [ mark "A" ]
            , init Highlightable.initStatic []
            , init Highlightable.initInteractive [ mark "B" ]
            , init Highlightable.initStatic []
            , init Highlightable.initInteractive [ mark "B" ]
            ]
                |> expectJoinsTo Sort.alphabetical
                    [ init Highlightable.initInteractive [ mark "A", mark "B" ]
                    , init Highlightable.initStatic [ mark "A" ]
                    , init Highlightable.initInteractive [ mark "A" ]
                    , init Highlightable.initStatic []
                    , init Highlightable.initInteractive [ mark "B" ]
                    , init Highlightable.initStatic [ mark "B" ]
                    , init Highlightable.initInteractive [ mark "B" ]
                    ]
    ]


usedMarkersSpec : List Test
usedMarkersSpec =
    let
        testUsedMarkers =
            List.indexedMap (\i f -> f i)
                >> Highlightable.usedMarkers Sort.alphabetical
                >> Sort.Set.toList

        init init_ marks index =
            init_ marks index "Content"

        initBlank init_ marks index =
            init_ marks index " "
    in
    [ test "for an empty list of highlightables, returns an empty set" <|
        \() ->
            testUsedMarkers []
                |> Expect.equal []
    , test "for a static blank with marks, returns an empty set" <|
        \() ->
            testUsedMarkers [ initBlank Highlightable.initStatic [ mark "a", mark "b" ] ]
                |> Expect.equal []
    , test "for an interactive blank with marks, returns an empty set" <|
        \() ->
            testUsedMarkers [ initBlank Highlightable.initInteractive [ mark "a", mark "b" ] ]
                |> Expect.equal []
    , test "for a static segment with marks, returns the marks" <|
        \() ->
            testUsedMarkers [ init Highlightable.initStatic [ mark "a", mark "b" ] ]
                |> Expect.equal [ "a", "b" ]
    , test "for an interactive segment with marks, returns the marks" <|
        \() ->
            testUsedMarkers [ init Highlightable.initInteractive [ mark "a", mark "b" ] ]
                |> Expect.equal [ "a", "b" ]
    , test "for a list of segments, returns the correct marks" <|
        \() ->
            testUsedMarkers
                [ init Highlightable.initInteractive [ mark "e", mark "b" ]
                , init Highlightable.initStatic [ mark "a" ]
                , init Highlightable.initInteractive [ mark "c" ]
                , init Highlightable.initInteractive [ mark "c", mark "d", mark "a" ]
                ]
                |> Expect.equal [ "a", "b", "c", "d", "e" ]
    ]


asFragmentTuplesSpec : List Test
asFragmentTuplesSpec =
    [ test "for a size-1 list" <|
        \() ->
            [ { text = "Content"
              , customAttributes = []
              , marked = [ mark "a", mark "b" ]
              , index = 0
              , type_ = Interactive
              }
            ]
                |> Highlightable.asFragmentTuples
                |> Expect.equal [ ( [ "a", "b" ], "Content" ) ]
    , test "for a longer list" <|
        \() ->
            [ { text = "Content"
              , customAttributes = []
              , marked = [ mark "a", mark "b" ]
              , index = 0
              , type_ = Interactive
              }
            , { text = " "
              , customAttributes = []
              , marked = [ mark "a" ]
              , index = 1
              , type_ = Static
              }
            , { text = "Content"
              , customAttributes = []
              , marked = [ mark "a", mark "c" ]
              , index = 2
              , type_ = Interactive
              }
            ]
                |> Highlightable.asFragmentTuples
                |> Expect.equal
                    [ ( [ "a", "b" ], "Content" )
                    , ( [ "a" ], " " )
                    , ( [ "a", "c" ], "Content" )
                    ]
    ]


byIdSpec : List Test
byIdSpec =
    [ test "returns Nothing if a highlightable with the id is not found" <|
        \() ->
            []
                |> Highlightable.byId 0
                |> Expect.equal Nothing
    , test "returns Just the highlightable if a highlightable with the id is found" <|
        \() ->
            let
                highlightable =
                    { text = "Content"
                    , customAttributes = []
                    , marked = []
                    , index = 0
                    , type_ = Interactive
                    }
            in
            [ highlightable ]
                |> Highlightable.byId 0
                |> Expect.equal (Just highlightable)
    ]


mark : a -> Tool.MarkerModel a
mark kind =
    Tool.buildMarker
        { highlightColor = Colors.highlightYellow
        , hoverColor = Colors.highlightYellow
        , hoverHighlightColor = Colors.highlightYellow
        , kind = kind
        , name = Nothing
        }
