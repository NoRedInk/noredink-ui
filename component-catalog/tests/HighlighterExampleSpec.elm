module HighlighterExampleSpec exposing (suite)

import Examples.Highlighter exposing (Msg, State, example)
import MouseHelpers
import ProgramTest exposing (..)
import PseudoElements exposing (hasAfter, hasBefore)
import Routes exposing (Route)
import Test exposing (..)
import Test.Html.Selector exposing (..)
import TestApp exposing (app)


route : Route State Msg
route =
    Routes.Doodad example


suite : Test
suite =
    describe "overlapping Highlighter example"
        [ test "it works to add overlaps" <|
            \() ->
                app route
                    |> ensureViewHas [ text "Nri.Ui.Highlighter" ]
                    |> mouseDown "Letter"
                    |> mouseUp "have"
                    |> mouseDown "have"
                    |> mouseUp "variety"
                    |> ensureView (hasBefore "start Comment 1 highlight" "Letter")
                    |> ensureView (hasAfter "end Comment 1" "have")
                    |> ensureView (hasBefore "start Comment 2 highlight" "have")
                    |> ensureView (hasAfter "end Comment 2" "variety")
                    |> done
        ]


mouseDown : String -> ProgramTest a b c -> ProgramTest a b c
mouseDown word =
    MouseHelpers.cancelableMouseDown [ tag "span", containing [ text word ] ]


mouseUp : String -> ProgramTest a b c -> ProgramTest a b c
mouseUp word =
    MouseHelpers.cancelableMouseUp [ tag "span", containing [ text word ] ]
