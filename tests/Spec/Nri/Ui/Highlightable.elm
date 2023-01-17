module Spec.Nri.Ui.Highlightable exposing (spec)

import Expect
import Fuzz exposing (..)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Highlightable.V1 as Highlightable
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
                                |> Highlightable.initFragment Nothing 0

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
                Highlightable.splitWords a
                    |> List.length
                    |> Expect.equal expected
        , describe "fromMarkdown" fromMarkdownSpec
        ]


fromMarkdownSpec : List Test
fromMarkdownSpec =
    let
        testFromMarkdown startingString expected =
            Highlightable.fromMarkdown startingString
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
    , test "does mark content that's intended to be highlighted" <|
        \() ->
            testFromMarkdown "A sentence with [highlighted content]()"
                [ ( "A sentence with ", Nothing )
                , ( "highlighted content", Just defaultMark )
                ]
    , test "marks content that's intended to be highlighted within an emphasis" <|
        \() ->
            testFromMarkdown "A sentence with *emphasized [highlighted content]()!*"
                [ ( "A sentence with ", Nothing )
                , ( "*emphasized *", Nothing )
                , ( "*highlighted content*", Just defaultMark )
                , ( "*!*", Nothing )
                ]
    ]
