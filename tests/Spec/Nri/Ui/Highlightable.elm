module Spec.Nri.Ui.Highlightable exposing (spec)

import Expect
import Fuzz exposing (..)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Highlightable.V1 as HighlightableV1
import Nri.Ui.Highlightable.V2 as Highlightable exposing (Type(..))
import Nri.Ui.Highlighter.V2 as Highlighter
import Nri.Ui.HighlighterTool.V1 as Tool
import Sort
import Sort.Set
import String.Extra
import Test exposing (..)


spec : Test
spec =
    describe "Highlightable"
        [ describe "roundtrip from initFragment via asFragmentTuples"
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
        , describe "usedMarkers" usedMarkersSpec
        , describe "blur" blurSpec
        , describe "hover" hoverSpec
        , describe "asFragmentTuples" asFragmentTuplesSpec
        , describe "byId" byIdSpec
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
        init type_ marks index =
            Highlightable.init type_ marks index ( [], "Content" )

        expectJoinsTo expected starting =
            let
                processedExpected =
                    List.indexedMap (\i f -> f i) expected
                        |> Highlightable.asFragmentTuples
            in
            List.indexedMap (\i f -> f i) starting
                |> Highlightable.joinAdjacentInteractiveHighlights
                |> Highlightable.asFragmentTuples
                |> Expect.equal processedExpected
    in
    [ test "without any segments, does nothing" <|
        \() ->
            []
                |> expectJoinsTo []
    , test "with 1 interactive highlighted segment, does nothing" <|
        \() ->
            let
                segment =
                    init Interactive [ mark () ]
            in
            [ segment ]
                |> expectJoinsTo [ segment ]
    , test "with 1 interactive unmarked segment, does nothing" <|
        \() ->
            let
                segment =
                    init Interactive []
            in
            [ segment ]
                |> expectJoinsTo [ segment ]
    , test "with 1 static highlighted segment, does nothing" <|
        \() ->
            let
                segment =
                    init Static [ mark () ]
            in
            [ segment ]
                |> expectJoinsTo [ segment ]
    , test "with 1 static unmarked segment, does nothing" <|
        \() ->
            let
                segment =
                    init Static []
            in
            [ segment ]
                |> expectJoinsTo [ segment ]
    , test "with only 2 segments [highlighted interactive, plain static], does nothing" <|
        \() ->
            let
                segments =
                    [ init Interactive [ mark () ]
                    , init Static []
                    ]
            in
            segments
                |> expectJoinsTo segments
    , test "with only 2 segments [plain static, highlighted interactive], does nothing" <|
        \() ->
            let
                segments =
                    [ init Static []
                    , init Interactive [ mark () ]
                    ]
            in
            segments
                |> expectJoinsTo segments
    , test "with 3 segments, highlighted interactive sandwiching a plain unmarked, marks the plain segmnet" <|
        \() ->
            [ init Interactive [ mark () ]
            , init Static []
            , init Interactive [ mark () ]
            ]
                |> expectJoinsTo
                    [ init Interactive [ mark () ]
                    , init Static [ mark () ]
                    , init Interactive [ mark () ]
                    ]
    , test "with 3 segments with highlights of various types, marks the plain segment correctly" <|
        \() ->
            [ init Interactive [ mark "A", mark "B", mark "C" ]
            , init Static []
            , init Interactive [ mark "A", mark "C" ]
            ]
                |> expectJoinsTo
                    [ init Interactive [ mark "A", mark "B", mark "C" ]
                    , init Static [ mark "A", mark "C" ]
                    , init Interactive [ mark "A", mark "C" ]
                    ]
    , test "with many segments, marks plain sandwiched segments" <|
        \() ->
            [ init Static []
            , init Interactive [ mark () ]
            , init Static []
            , init Static []
            , init Static []
            , init Interactive [ mark () ]
            , init Static []
            ]
                |> expectJoinsTo
                    [ init Static []
                    , init Interactive [ mark () ]
                    , init Static [ mark () ]
                    , init Static [ mark () ]
                    , init Static [ mark () ]
                    , init Interactive [ mark () ]
                    , init Static []
                    ]
    , test "with multiple highlighted interactive segments not all of the same type, only joins the types that match" <|
        \() ->
            [ init Interactive [ mark "A", mark "B" ]
            , init Static []
            , init Interactive [ mark "A" ]
            , init Static []
            , init Interactive [ mark "B" ]
            , init Static []
            , init Interactive [ mark "B" ]
            ]
                |> expectJoinsTo
                    [ init Interactive [ mark "A", mark "B" ]
                    , init Static [ mark "A" ]
                    , init Interactive [ mark "A" ]
                    , init Static []
                    , init Interactive [ mark "B" ]
                    , init Static [ mark "B" ]
                    , init Interactive [ mark "B" ]
                    ]
    ]


usedMarkersSpec : List Test
usedMarkersSpec =
    let
        testUsedMarkers =
            List.indexedMap (\i f -> f i)
                >> Highlightable.usedMarkers Sort.alphabetical
                >> Sort.Set.toList

        init type_ marks index =
            Highlightable.init type_ marks index ( [], "Content" )

        initBlank type_ marks index =
            Highlightable.init type_ marks index ( [], " " )
    in
    [ test "for an empty list of highlightables, returns an empty set" <|
        \() ->
            testUsedMarkers []
                |> Expect.equal []
    , test "for a static blank with marks, returns an empty set" <|
        \() ->
            testUsedMarkers [ initBlank Static [ mark "a", mark "b" ] ]
                |> Expect.equal []
    , test "for an interactive blank with marks, returns an empty set" <|
        \() ->
            testUsedMarkers [ initBlank Interactive [ mark "a", mark "b" ] ]
                |> Expect.equal []
    , test "for a static segment with marks, returns the marks" <|
        \() ->
            testUsedMarkers [ init Static [ mark "a", mark "b" ] ]
                |> Expect.equal [ "a", "b" ]
    , test "for an interactive segment with marks, returns the marks" <|
        \() ->
            testUsedMarkers [ init Interactive [ mark "a", mark "b" ] ]
                |> Expect.equal [ "a", "b" ]
    , test "for a list of segments, returns the correct marks" <|
        \() ->
            testUsedMarkers
                [ init Interactive [ mark "e", mark "b" ]
                , init Static [ mark "a" ]
                , init Interactive [ mark "c" ]
                , init Interactive [ mark "c", mark "d", mark "a" ]
                ]
                |> Expect.equal [ "a", "b", "c", "d", "e" ]
    ]


blurSpec : List Test
blurSpec =
    let
        highlightable state =
            { text = "Content"
            , uiState = state
            , customAttributes = []
            , marked = []
            , index = 0
            , type_ = Interactive
            }
    in
    [ test "when hinted, no change to state" <|
        \() ->
            highlightable Highlightable.Hinted
                |> Highlightable.blur
                |> Expect.equal (highlightable Highlightable.Hinted)
    , test "when hovered, remove hover state" <|
        \() ->
            highlightable Highlightable.Hovered
                |> Highlightable.blur
                |> Expect.equal (highlightable Highlightable.None)
    , test "when no UiState, no change to state" <|
        \() ->
            highlightable Highlightable.None
                |> Highlightable.blur
                |> Expect.equal (highlightable Highlightable.None)
    ]


hoverSpec : List Test
hoverSpec =
    let
        highlightable state =
            { text = "Content"
            , uiState = state
            , customAttributes = []
            , marked = []
            , index = 0
            , type_ = Interactive
            }
    in
    [ test "when hinted, no change to state" <|
        \() ->
            highlightable Highlightable.Hinted
                |> Highlightable.hover
                |> Expect.equal (highlightable Highlightable.Hinted)
    , test "when already hovered, no change to state" <|
        \() ->
            highlightable Highlightable.Hovered
                |> Highlightable.hover
                |> Expect.equal (highlightable Highlightable.Hovered)
    , test "when no UiState, hover" <|
        \() ->
            highlightable Highlightable.None
                |> Highlightable.hover
                |> Expect.equal (highlightable Highlightable.Hovered)
    ]


asFragmentTuplesSpec : List Test
asFragmentTuplesSpec =
    [ test "for a size-1 list" <|
        \() ->
            [ { text = "Content"
              , uiState = Highlightable.None
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
              , uiState = Highlightable.None
              , customAttributes = []
              , marked = [ mark "a", mark "b" ]
              , index = 0
              , type_ = Interactive
              }
            , { text = " "
              , uiState = Highlightable.None
              , customAttributes = []
              , marked = [ mark "a" ]
              , index = 1
              , type_ = Static
              }
            , { text = "Content"
              , uiState = Highlightable.None
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
                    , uiState = Highlightable.None
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
