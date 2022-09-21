module Spec.Nri.Ui.Highlighter exposing (spec)

import Accessibility.Key as Key
import Expect
import Html.Styled exposing (toUnstyled)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Highlightable.V1 as Highlightable exposing (Highlightable)
import Nri.Ui.Highlighter.V1 as Highlighter
import Nri.Ui.HighlighterTool.V1 as Tool exposing (Tool)
import ProgramTest exposing (..)
import Test exposing (..)
import Test.Html.Query exposing (..)
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.Highlighter.V1"
        [ describe "keyboard behavior" keyboardTests
        ]


keyboardTests : List Test
keyboardTests =
    [ test "has a focusable element when there is one" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos thrive in indirect light."
                |> program marker
                |> ensureFocusOn "Pothos"
                |> done
    ]


ensureFocusOn : String -> TestContext marker -> TestContext marker
ensureFocusOn word textContext =
    textContext
        |> ensureViewHas [ text word, attribute (Key.tabbable True) ]
        |> ensureView
            (findAll [ attribute (Key.tabbable True) ]
                >> count (Expect.equal 1)
            )


marker : Tool ()
marker =
    Tool.Marker
        (Tool.buildMarker
            { highlightColor = Colors.magenta
            , hoverColor = Colors.magenta
            , hoverHighlightColor = Colors.magenta
            , kind = ()
            , name = Nothing
            }
        )


type alias TestContext marker =
    ProgramTest (Highlighter.Model marker) (Highlighter.Msg marker) ()


program : Tool marker -> List (Highlightable marker) -> TestContext marker
program tool highlightables =
    ProgramTest.createSandbox
        { init =
            Highlighter.init
                { id = "test-highlighter-container"
                , highlightables = highlightables
                , marker = tool
                }
        , update =
            \msg model ->
                case Highlighter.update msg model of
                    ( newModel, _, _ ) ->
                        newModel
        , view = Highlighter.view >> toUnstyled
        }
        |> ProgramTest.start ()
