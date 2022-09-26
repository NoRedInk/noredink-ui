module Spec.Nri.Ui.Highlighter exposing (spec)

import Accessibility.Key as Key
import Expect
import Html.Styled exposing (toUnstyled)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Highlightable.V1 as Highlightable exposing (Highlightable)
import Nri.Ui.Highlighter.V1 as Highlighter
import Nri.Ui.HighlighterTool.V1 as Tool exposing (Tool)
import ProgramTest exposing (..)
import Spec.KeyboardHelpers as KeyboardHelpers
import Spec.MouseHelpers as MouseHelpers
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.Highlighter.V1"
        [ describe "keyboard behavior" keyboardTests
        ]


keyboardTests : List Test
keyboardTests =
    [ test "has a focusable element when there is one" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program marker
                |> ensureFocusOn "Pothos"
                |> done
    , test "moves focus right on right arrow key" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program marker
                |> ensureFocusOn "Pothos"
                |> rightArrow
                |> ensureFocusOn "indirect"
                |> rightArrow
                |> ensureFocusOn "light"
                -- once we're on the final element, pressing right arrow again should
                -- _not_ wrap the focus. We should stay right where we are!
                |> rightArrow
                |> ensureFocusOn "light"
                |> done
    , test "moves focus left on left arrow key" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program marker
                |> ensureFocusOn "Pothos"
                |> rightArrow
                |> ensureFocusOn "indirect"
                |> leftArrow
                |> ensureFocusOn "Pothos"
                -- once we're on the first element, pressing left arrow again should
                -- _not_ wrap the focus. We should stay right where we are!
                |> leftArrow
                |> ensureFocusOn "Pothos"
                |> done
    , test "moves focus right on shift + right arrow" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program marker
                |> ensureFocusOn "Pothos"
                |> shiftRight
                |> ensureFocusOn "indirect"
                |> shiftRight
                |> ensureFocusOn "light"
                |> shiftRight
                |> ensureFocusOn "light"
                |> done
    , test "moves focus left on shift + left arrow" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program marker
                |> ensureFocusOn "Pothos"
                |> rightArrow
                |> ensureFocusOn "indirect"
                |> shiftLeft
                |> ensureFocusOn "Pothos"
                |> shiftLeft
                |> ensureFocusOn "Pothos"
                |> done
    , test "expands selection one element to the right on shift + right arrow and highlight selected elements" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program marker
                |> shiftRight
                |> releaseShiftRight
                |> ensureMarked [ "Pothos", "", "indirect" ]
                |> shiftRight
                |> releaseShiftRight
                |> ensureMarked [ "Pothos", "", "indirect", "", "light" ]
                |> shiftRight
                |> releaseShiftRight
                |> ensureMarked [ "Pothos", "", "indirect", "", "light" ]
                |> done
    , test "expands selection one element to the left on shift + left arrow and highlight selected elements" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program marker
                |> rightArrow
                |> rightArrow
                |> shiftLeft
                |> releaseShiftLeft
                |> ensureMarked [ "indirect", "", "light" ]
                |> shiftLeft
                |> releaseShiftLeft
                |> ensureMarked [ "Pothos", "", "indirect", "", "light" ]
                |> shiftLeft
                |> releaseShiftLeft
                |> ensureMarked [ "Pothos", "", "indirect", "", "light" ]
                |> done
    , test "merges highlights" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program marker
                |> ensureFocusOn "Pothos"
                |> shiftRight
                |> releaseShiftRight
                |> ensureMarked [ "Pothos", "", "indirect" ]
                |> ensureFocusOn "indirect"
                |> rightArrow
                |> ensureFocusOn "light"
                |> shiftLeft
                |> releaseShiftLeft
                |> ensureMarked [ "Pothos", "", "indirect", "", "light" ]
                |> done
    , test "selects element on MouseDown and highlights selected element on MouseUp" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program marker
                |> ensureFocusOn "Pothos"
                |> mouseDown "Pothos"
                |> mouseUp "Pothos"
                |> ensureMarked [ "Pothos" ]
                |> done
    , test "selects element on MouseDown, expands selection on MouseOver, and highlights selected elements on MouseUp" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program marker
                |> ensureFocusOn "Pothos"
                |> mouseDown "Pothos"
                |> mouseOver "indirect"
                |> mouseUp "Pothos"
                |> ensureMarked [ "Pothos", "", "indirect" ]
                |> done
    , test "Highlights element on Space" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program marker
                |> ensureFocusOn "Pothos"
                |> space
                |> ensureMarked [ "Pothos" ]
                |> done
    , test "Removes highlight from element on MouseUp" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program marker
                |> ensureFocusOn "Pothos"
                |> space
                |> ensureMarked [ "Pothos" ]
                |> ensureFocusOn "Pothos"
                |> mouseDown "Pothos"
                |> mouseUp "Pothos"
                |> expectViewHasNot
                    [ Selector.tag "mark" ]
    , test "Removes entire highlight from a group of elements on MouseUp" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program marker
                |> ensureFocusOn "Pothos"
                |> shiftRight
                |> releaseShiftRight
                |> ensureMarked [ "Pothos", "", "indirect" ]
                |> mouseDown "indirect"
                |> mouseUp "indirect"
                |> expectViewHasNot
                    [ Selector.tag "mark" ]
    , test "Removes highlight from element on Space" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program marker
                |> ensureFocusOn "Pothos"
                |> space
                |> ensureMarked [ "Pothos" ]
                |> ensureFocusOn "Pothos"
                |> space
                |> expectViewHasNot
                    [ Selector.tag "mark" ]
    ]


ensureFocusOn : String -> TestContext marker -> TestContext marker
ensureFocusOn word textContext =
    textContext
        |> ensureView
            (Query.find [ Selector.attribute (Key.tabbable True) ]
                >> Query.has [ Selector.text word ]
            )
        |> ensureView
            (Query.findAll [ Selector.attribute (Key.tabbable True) ]
                >> Query.count (Expect.equal 1)
            )
        -- We only manage focus for interactive elements.
        -- Static elements should not have explicit tabindex -1 as it is redundant,
        -- they are never included in the tab sequence
        |> ensureView
            (Query.findAll [ Selector.attribute (Key.tabbable False) ]
                >> Query.count (Expect.equal 2)
            )


ensureMarked : List String -> TestContext marker -> TestContext marker
ensureMarked words textContext =
    textContext
        |> ensureView
            (Query.find [ Selector.tag "mark" ]
                >> Query.children [ Selector.tag "span" ]
                >> Expect.all (List.indexedMap (\i w -> Query.index i >> Query.has [ Selector.text w ]) words)
            )


space : TestContext marker -> TestContext marker
space =
    KeyboardHelpers.pressSpaceKey { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


rightArrow : TestContext marker -> TestContext marker
rightArrow =
    KeyboardHelpers.pressRightArrow { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


leftArrow : TestContext marker -> TestContext marker
leftArrow =
    KeyboardHelpers.pressLeftArrow { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


shiftRight : TestContext marker -> TestContext marker
shiftRight =
    KeyboardHelpers.pressShiftRight { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


shiftLeft : TestContext marker -> TestContext marker
shiftLeft =
    KeyboardHelpers.pressShiftLeft { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


releaseShiftRight : TestContext marker -> TestContext marker
releaseShiftRight =
    KeyboardHelpers.releaseShiftRight { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


releaseShiftLeft : TestContext marker -> TestContext marker
releaseShiftLeft =
    KeyboardHelpers.releaseShiftLeft { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


mouseDown : String -> TestContext marker -> TestContext marker
mouseDown word =
    MouseHelpers.cancelableMouseDown [ Selector.tag "span", Selector.containing [ Selector.text word ] ]


mouseUp : String -> TestContext marker -> TestContext marker
mouseUp word =
    MouseHelpers.cancelableMouseUp [ Selector.tag "span", Selector.containing [ Selector.text word ] ]


mouseOver : String -> TestContext marker -> TestContext marker
mouseOver word =
    MouseHelpers.cancelableMouseOver [ Selector.tag "span", Selector.containing [ Selector.text word ] ]


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
