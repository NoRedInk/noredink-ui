module Spec.Nri.Ui.Highlightable exposing (spec)

import Expect
import Fuzz exposing (..)
import Nri.Ui.Highlightable.V1 as Highlightable
import Nri.Ui.Highlighter.V2 as Highlighter
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
        ]
