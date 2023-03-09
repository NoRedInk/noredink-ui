module Spec.Nri.Ui.Highlightable exposing (spec)

import Expect
import Fuzz exposing (..)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Highlightable.V1 as HighlightableV1
import Nri.Ui.Highlightable.V2 as Highlightable exposing (Type(..))
import Nri.Ui.Highlighter.V2 as Highlighter
import Nri.Ui.HighlighterTool.V1 as Tool
import String.Extra
import Test exposing (..)


spec : Test
spec =
    describe "Highlightable"
        [ describe "roundtrip from initFragment via toTuplesByFragment"
            [ fuzz (list string) "toTuplesByFragment (initFragment [str]) ... == [str]" <|
                \strings ->
                    let
                        fragment =
                            strings
                                |> List.map (Tuple.pair [])
                                |> HighlightableV1.initFragment Nothing 0

                        roundtripText =
                            Highlighter.asFragmentTuples fragment
                                |> List.head
                                |> Maybe.map Tuple.second

                        expected =
                            String.Extra.nonEmpty (String.join "" strings)
                    in
                    Expect.equal roundtripText expected
            ]
        , fuzz Fuzz.string "splitWords" <|
            \a ->
                let
                    expected =
                        String.split " " a
                            |> List.length
                            |> (*) 2
                            |> (\x ->
                                    if x > 1 then
                                        x - 1

                                    else
                                        x
                               )
                            |> max 0
                in
                HighlightableV1.splitWords a
                    |> List.length
                    |> Expect.equal expected
        , describe "HighlightableV1.fromMarkdown" fromMarkdownV1Spec
        , describe "fromMarkdown" fromMarkdownSpec
        , describe "joinAdjacentInteractiveHighlights" joinAdjacentInteractiveHighlightsSpec
        ]


fromMarkdownV1Spec : List Test
fromMarkdownV1Spec =
    let
        testFromMarkdown startingString expected =
            HighlightableV1.fromMarkdown startingString
                |> List.map (\{ text, marked } -> ( text, marked ))
                |> Expect.equal expected

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
                [ ( "A sentence without highlighted content", Nothing ) ]
    , test "does not strip emphasized content" <|
        \() ->
            testFromMarkdown "A *sentence without* **highlighted content**"
                [ ( "A *sentence without* **highlighted content**", Nothing ) ]
    , test "marks a single segment as highlighted" <|
        \() ->
            testFromMarkdown "[fake link]()"
                [ ( "fake link", Just defaultMark ) ]
    , test "does mark content that's intended to be highlighted" <|
        \() ->
            testFromMarkdown "A sentence with [highlighted content]()"
                [ ( "A sentence with ", Nothing )
                , ( "highlighted content", Just defaultMark )
                ]
    , test "marks content that's intended to be highlighted within an emphasis" <|
        \() ->
            testFromMarkdown "A sentence with *emphasized [highlighted content]()!*"
                [ ( "A sentence with *emphasized *", Nothing )
                , ( "*highlighted content*", Just defaultMark )
                , ( "*!*", Nothing )
                ]
    , test "marks content that's intended to be highlighted that also contains an emphasis" <|
        \() ->
            testFromMarkdown "A sentence with [highlighted *and partially emphasized* content]()"
                [ ( "A sentence with ", Nothing )
                , ( "highlighted *and partially emphasized* content", Just defaultMark )
                ]
    , test "does not highlight actual links" <|
        \() ->
            testFromMarkdown "I am a [real link](google.com)"
                [ ( "I am a [real link](google.com)", Nothing )
                ]
    , test "does not get confused by parentheses" <|
        \() ->
            testFromMarkdown "main thought (parenthetical)"
                [ ( "main thought (parenthetical)", Nothing )
                ]
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
        mark kind =
            Tool.buildMarker
                { highlightColor = Colors.highlightYellow
                , hoverColor = Colors.highlightYellow
                , hoverHighlightColor = Colors.highlightYellow
                , kind = kind
                , name = Nothing
                }

        init type_ marks index =
            Highlightable.init type_ marks index ( [], "Content" )
    in
    [ test "without any segments, does nothing" <|
        \() ->
            []
                |> Highlightable.joinAdjacentInteractiveHighlights
                |> Expect.equal []
    , test "with 1 interactive highlighted segment, does nothing" <|
        \() ->
            let
                segment =
                    init Interactive [ mark () ] 0
            in
            [ segment ]
                |> Highlightable.joinAdjacentInteractiveHighlights
                |> Expect.equal [ segment ]
    , test "with 1 interactive unmarked segment, does nothing" <|
        \() ->
            let
                segment =
                    init Interactive [] 0
            in
            [ segment ]
                |> Highlightable.joinAdjacentInteractiveHighlights
                |> Expect.equal [ segment ]
    , test "with 1 static highlighted segment, does nothing" <|
        \() ->
            let
                segment =
                    init Static [ mark () ] 0
            in
            [ segment ]
                |> Highlightable.joinAdjacentInteractiveHighlights
                |> Expect.equal [ segment ]
    , test "with 1 static unmarked segment, does nothing" <|
        \() ->
            let
                segment =
                    init Static [] 0
            in
            [ segment ]
                |> Highlightable.joinAdjacentInteractiveHighlights
                |> Expect.equal [ segment ]
    , test "with only 2 segments [highlighted interactive, plain static], does nothing" <|
        \() ->
            let
                segments =
                    [ init Interactive [ mark () ] 0
                    , init Static [] 1
                    ]
            in
            segments
                |> Highlightable.joinAdjacentInteractiveHighlights
                |> Expect.equal segments
    , test "with only 2 segments [plain static, highlighted interactive], does nothing" <|
        \() ->
            let
                segments =
                    [ init Static [] 0
                    , init Interactive [ mark () ] 1
                    ]
            in
            segments
                |> Highlightable.joinAdjacentInteractiveHighlights
                |> Expect.equal segments
    , test "with 3 segments, highlighted interactive sandwiching a plain unmarked, marks the plain segmnet" <|
        \() ->
            [ init Interactive [ mark () ] 0
            , init Static [] 1
            , init Interactive [ mark () ] 2
            ]
                |> Highlightable.joinAdjacentInteractiveHighlights
                |> Expect.equal
                    [ init Interactive [ mark () ] 0
                    , init Static [ mark () ] 1
                    , init Interactive [ mark () ] 2
                    ]
    , test "with many segments, marks plain sandwiched segments" <|
        \() ->
            [ init Static [] 0
            , init Interactive [ mark () ] 1
            , init Static [] 2
            , init Static [] 3
            , init Static [] 4
            , init Interactive [ mark () ] 5
            , init Static [] 6
            ]
                |> Highlightable.joinAdjacentInteractiveHighlights
                |> Expect.equal
                    [ init Static [] 0
                    , init Interactive [ mark () ] 1
                    , init Static [ mark () ] 2
                    , init Static [ mark () ] 3
                    , init Static [ mark () ] 4
                    , init Interactive [ mark () ] 5
                    , init Static [] 6
                    ]
    , test "with multiple highlighted interactive segments not all of the same type, only joins the types that match" <|
        \() ->
            [ init Interactive [ mark "A", mark "B" ] 0
            , init Static [] 1
            , init Interactive [ mark "A" ] 2
            , init Static [] 3
            , init Interactive [ mark "B" ] 4
            , init Static [] 5
            , init Interactive [ mark "B" ] 6
            ]
                |> Highlightable.joinAdjacentInteractiveHighlights
                |> Expect.equal
                    [ init Interactive [ mark "A", mark "B" ] 0
                    , init Static [ mark "A" ] 1
                    , init Interactive [ mark "A" ] 2
                    , init Static [] 3
                    , init Interactive [ mark "B" ] 4
                    , init Static [ mark "B" ] 5
                    , init Interactive [ mark "B" ] 6
                    ]
    ]
