module Spec.Nri.Ui.HighlighterOverlaps exposing (spec)

import Expect exposing (Expectation)
import List.Extra
import Maybe.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Highlightable.V3 as Highlightable
import Nri.Ui.Highlighter.V5 as Highlighter
import Nri.Ui.HighlighterTool.V1 as Tool
import Sort
import Test exposing (..)


spec : Test
spec =
    describe "Highlighter#selectShortest"
        [ test "no overlap" <|
            \_ ->
                init2
                    |> marker "###   "
                    |> marker "   ***"
                    |> cursor "   ^  "
                    |> expect "*"
        , test "shortest at start" <|
            \_ ->
                init2
                    |> marker "#  "
                    |> marker "***"
                    |> cursor "^  "
                    |> expect "#"
        , test "shortest in the middle" <|
            \_ ->
                init2
                    |> marker " # "
                    |> marker "***"
                    |> cursor " ^ "
                    |> expect "#"
        , test "shortest at the end" <|
            \_ ->
                init2
                    |> marker "  #"
                    |> marker "***"
                    |> cursor "  ^"
                    |> expect "#"
        , test "shortest intersects from left" <|
            \_ ->
                init2
                    |> marker "####  "
                    |> marker "   ***"
                    |> cursor "   ^  "
                    |> expect "*"
        , test "shortest intersects from right" <|
            \_ ->
                init2
                    |> marker "  ####"
                    |> marker "***   "
                    |> cursor "  ^   "
                    |> expect "*"
        , test "shortest is non-contiguous" <|
            \_ ->
                init2
                    |> marker "  ####  "
                    |> marker "*  *   *"
                    |> cursor "   ^    "
                    |> expect "*"
        , test "longest is non-contiguous" <|
            \_ ->
                init2
                    |> marker "      ####      "
                    |> marker "*****  *   *****"
                    |> cursor "       ^        "
                    |> expect "#"
        ]


init2 : String -> String -> String -> Highlighter.Model String
init2 marker1 marker2 cursor_ =
    let
        highlightables =
            List.Extra.zip (String.toList marker1) (String.toList marker2)
                |> List.indexedMap
                    (\idx ( a, b ) ->
                        Highlightable.initInteractive
                            (markers a b)
                            idx
                            " "
                    )

        cursorIndex =
            String.toList cursor_
                |> List.Extra.elemIndex '^'
                |> Maybe.withDefault -1
    in
    Highlighter.init
        { id = "test-highlighter-container"
        , highlightables = highlightables
        , marker = Tool.Marker (mkMarker "unused")
        , joinAdjacentInteractiveHighlights = False
        , sorter = Sort.alphabetical
        }
        |> Highlighter.update (Highlighter.Pointer (Highlighter.Over cursorIndex))
        |> (\( model, _, _ ) -> model)


marker : String -> (String -> a) -> a
marker str f =
    f str


cursor : String -> (String -> a) -> a
cursor str f =
    f str


expect : String -> Highlighter.Model String -> Expectation
expect cursorName highlighter =
    Highlighter.selectShortest Highlighter.hoveredHighlightable highlighter
        |> Expect.equal (Just cursorName)


markers : Char -> Char -> List (Tool.MarkerModel String)
markers a b =
    Maybe.Extra.values
        [ if a == ' ' then
            Nothing

          else
            Just (mkMarker (String.fromChar a))
        , if b == ' ' then
            Nothing

          else
            Just (mkMarker (String.fromChar b))
        ]


mkMarker : String -> Tool.MarkerModel String
mkMarker str =
    Tool.buildMarker
        { highlightColor = Colors.magenta
        , hoverColor = Colors.magenta
        , hoverHighlightColor = Colors.magenta
        , kind = str
        , name = Just str
        }
