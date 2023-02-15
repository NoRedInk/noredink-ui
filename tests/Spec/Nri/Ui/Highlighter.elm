module Spec.Nri.Ui.Highlighter exposing (spec)

import Accessibility.Key as Key
import Expect exposing (Expectation)
import Html.Styled exposing (Html, toUnstyled)
import List.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Highlightable.V1 as Highlightable exposing (Highlightable)
import Nri.Ui.Highlighter.V2 as Highlighter
import Nri.Ui.HighlighterTool.V1 as Tool exposing (Tool)
import ProgramTest exposing (..)
import Regex exposing (Regex)
import Spec.KeyboardHelpers as KeyboardHelpers
import Spec.MouseHelpers as MouseHelpers
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.Highlighter.V2"
        [ describe "keyboard behavior" keyboardTests
        , describe "markdown behavior" markdownTests
        , describe "joinAdjacentInteractiveHighlights" joinAdjacentInteractiveHighlightsTests
        ]


keyboardTests : List Test
keyboardTests =
    [ test "has a focusable element when there is one" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> done
    , test "has only one element included in the tab sequence" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureOnlyOneInTabSequence (String.words "Pothos indirect light")
                |> done
    , test "moves focus right on right arrow key" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> rightArrow
                |> ensureTabbable "indirect"
                |> ensureOnlyOneInTabSequence (String.words "Pothos indirect light")
                |> rightArrow
                |> ensureTabbable "light"
                -- once we're on the final element, pressing right arrow again should
                -- _not_ wrap the focus. We should stay right where we are!
                |> rightArrow
                |> ensureTabbable "light"
                |> done
    , test "moves focus left on left arrow key" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> rightArrow
                |> ensureTabbable "indirect"
                |> leftArrow
                |> ensureTabbable "Pothos"
                |> ensureOnlyOneInTabSequence (String.words "Pothos indirect light")
                -- once we're on the first element, pressing left arrow again should
                -- _not_ wrap the focus. We should stay right where we are!
                |> leftArrow
                |> ensureTabbable "Pothos"
                |> done
    , test "moves focus right on shift + right arrow" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> shiftRight
                |> ensureTabbable "indirect"
                |> shiftRight
                |> ensureTabbable "light"
                |> shiftRight
                |> ensureTabbable "light"
                |> done
    , test "moves focus left on shift + left arrow" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> rightArrow
                |> ensureTabbable "indirect"
                |> shiftLeft
                |> ensureTabbable "Pothos"
                |> shiftLeft
                |> ensureTabbable "Pothos"
                |> done
    , test "expands selection one element to the right on shift + right arrow and highlight selected elements" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                |> shiftRight
                |> releaseShiftRight
                |> ensureMarked [ "Pothos", " ", "indirect" ]
                |> shiftRight
                |> releaseShiftRight
                |> ensureMarked [ "Pothos", " ", "indirect", " ", "light" ]
                |> shiftRight
                |> releaseShiftRight
                |> ensureMarked [ "Pothos", " ", "indirect", " ", "light" ]
                |> done
    , test "expands selection one element to the left on shift + left arrow and highlight selected elements" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                |> rightArrow
                |> rightArrow
                |> shiftLeft
                |> releaseShiftLeft
                |> ensureMarked [ "indirect", " ", "light" ]
                |> shiftLeft
                |> releaseShiftLeft
                |> ensureMarked [ "Pothos", " ", "indirect", " ", "light" ]
                |> shiftLeft
                |> releaseShiftLeft
                |> ensureMarked [ "Pothos", " ", "indirect", " ", "light" ]
                |> done
    , test "merges highlights" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> shiftRight
                |> releaseShiftRight
                |> ensureMarked [ "Pothos", " ", "indirect" ]
                |> ensureTabbable "indirect"
                |> rightArrow
                |> ensureTabbable "light"
                |> shiftLeft
                |> releaseShiftLeft
                |> ensureMarked [ "Pothos", " ", "indirect", " ", "light" ]
                |> done
    , test "selects element on MouseDown and highlights selected element on MouseUp" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> mouseDown "Pothos"
                |> mouseUp "Pothos"
                |> ensureMarked [ "Pothos" ]
                |> done
    , test "selects element on MouseDown, expands selection on MouseOver, and highlights selected elements on MouseUp" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> mouseDown "Pothos"
                |> mouseOver "indirect"
                |> mouseUp "Pothos"
                |> ensureMarked [ "Pothos", " ", "indirect" ]
                |> done
    , test "Highlights element on Space" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> space
                |> ensureMarked [ "Pothos" ]
                |> done
    , test "Removes highlight from element on MouseUp" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> space
                |> ensureMarked [ "Pothos" ]
                |> ensureTabbable "Pothos"
                |> mouseDown "Pothos"
                |> mouseUp "Pothos"
                |> expectViewHasNot [ Selector.tag "mark" ]
    , test "Removes entire highlight from a group of elements on MouseUp" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> shiftRight
                |> releaseShiftRight
                |> ensureMarked [ "Pothos", " ", "indirect" ]
                |> mouseDown "indirect"
                |> mouseUp "indirect"
                |> expectViewHasNot [ Selector.tag "mark" ]
    , test "Removes highlight from element on Space" <|
        \() ->
            Highlightable.initFragments Nothing "Pothos indirect light"
                |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> space
                |> ensureMarked [ "Pothos" ]
                |> ensureTabbable "Pothos"
                |> space
                |> expectViewHasNot [ Selector.tag "mark" ]
    , describe "Regression tests for A11-1767"
        [ test "generic start announcement is made when mark does not include first element" <|
            \() ->
                Highlightable.initFragments Nothing "Pothos indirect light"
                    |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                    |> rightArrow
                    |> shiftRight
                    |> releaseShiftRight
                    |> ensureMarked [ "indirect" ]
                    |> expectView (hasStartHighlightBeforeContent "start highlight" "indirect")
        , test "specific start announcement is made when mark does not include first element" <|
            \() ->
                Highlightable.initFragments Nothing "Pothos indirect light"
                    |> program { name = Just "banana", joinAdjacentInteractiveHighlights = False }
                    |> rightArrow
                    |> ensureTabbable "indirect"
                    |> shiftRight
                    |> releaseShiftRight
                    |> ensureMarked [ "indirect" ]
                    |> expectView (hasStartHighlightBeforeContent "start banana highlight" "indirect")
        ]
    , describe "Regression tests for A11-1769"
        [ -- as far as I can tell, the problem is not in the Elm logic.
          -- However, it still seemed worth adding an explicit check against the buggy behavior.
          test "Focus moves past 3rd element" <|
            \() ->
                Highlightable.initFragments Nothing "Sir Walter Elliot, of Kellynch Hall, in Somersetshire..."
                    |> program { name = Just "Claim", joinAdjacentInteractiveHighlights = False }
                    |> shiftRight
                    |> releaseShiftRight
                    |> ensureMarked [ "Sir", " ", "Walter" ]
                    |> ensureTabbable "Walter"
                    |> rightArrow
                    |> ensureTabbable "Elliot,"
                    |> rightArrow
                    |> ensureTabbable "of"
                    |> rightArrow
                    |> ensureTabbable "Kellynch"
                    |> done
        ]
    ]


markdownTests : List Test
markdownTests =
    [ testRendersRawContent "view" Highlighter.view
    , testRendersMarkdownContent "viewMarkdown" Highlighter.viewMarkdown
    , testRendersRawContent "static" Highlighter.static
    , testRendersMarkdownContent "staticMarkdown" Highlighter.staticMarkdown
    , testRendersRawContent "staticWithTags" Highlighter.staticWithTags
    , testRendersMarkdownContent "staticMarkdownWithTags" Highlighter.staticMarkdownWithTags
    ]


testRendersRawContent : String -> (Highlighter.Model String -> Html msg) -> Test
testRendersRawContent testName view =
    test (testName ++ " does not interpret content as markdown")
        (\() ->
            Highlightable.initFragments Nothing "*Pothos* indirect light"
                |> startWithoutMarker view
                |> expectViewHas [ Selector.text "*Pothos*" ]
        )


testRendersMarkdownContent : String -> (Highlighter.Model String -> Html msg) -> Test
testRendersMarkdownContent testName view =
    test (testName ++ " does interpret content as markdown")
        (\() ->
            Highlightable.initFragments Nothing "*Pothos* indirect light"
                |> startWithoutMarker view
                |> ensureViewHasNot [ Selector.text "*Pothos*" ]
                |> expectViewHas
                    [ Selector.tag "em"
                    , Selector.containing [ Selector.text "Pothos" ]
                    ]
        )


startWithoutMarker : (Highlighter.Model String -> Html msg) -> List (Highlightable String) -> ProgramTest (Highlighter.Model String) msg ()
startWithoutMarker view highlightables =
    ProgramTest.createSandbox
        { init =
            Highlighter.init
                { id = "highlighter-container"
                , highlightables = highlightables
                , marker = markerModel Nothing
                , joinAdjacentInteractiveHighlights = False
                }
        , update = \_ m -> m
        , view = view >> toUnstyled
        }
        |> ProgramTest.start ()


hasStartHighlightBeforeContent : String -> String -> Query.Single msg -> Expectation
hasStartHighlightBeforeContent startHighlightMarker relevantHighlightableText view =
    let
        styles =
            view
                |> Query.find [ Selector.tag "style" ]
                |> Query.children []
                |> Debug.toString

        startHighlightClassRegex : Maybe Regex
        startHighlightClassRegex =
            "\\.(\\_[a-zA-Z0-9]+)::before\\{content:\\\\\"\\s*\\s*"
                ++ startHighlightMarker
                |> Regex.fromString

        maybeClassName : Maybe String
        maybeClassName =
            startHighlightClassRegex
                |> Maybe.andThen
                    (\regex ->
                        Regex.find regex styles
                            |> List.head
                            |> Maybe.andThen (.submatches >> List.head)
                    )
                |> Maybe.withDefault Nothing
    in
    case maybeClassName of
        Just className ->
            Query.has
                [ Selector.tag "mark"
                , Selector.containing
                    [ Selector.class className
                    , Selector.containing [ Selector.text relevantHighlightableText ]
                    ]
                ]
                view

        Nothing ->
            "Expected to find a class defining a ::before element with content: `"
                ++ startHighlightMarker
                ++ "`, but failed to find the class in the styles: \n\n"
                ++ styles
                |> Expect.fail


ensureTabbable : String -> TestContext -> TestContext
ensureTabbable word testContext =
    testContext
        |> ensureView
            (Query.find [ Selector.attribute (Key.tabbable True) ]
                >> Query.has [ Selector.text word ]
            )


ensureOnlyOneInTabSequence : List String -> TestContext -> TestContext
ensureOnlyOneInTabSequence words testContext =
    testContext
        |> ensureView
            (Query.findAll [ Selector.attribute (Key.tabbable True) ]
                >> Query.count (Expect.equal 1)
            )
        |> ensureView
            (Query.findAll [ Selector.attribute (Key.tabbable False) ]
                >> Query.count (Expect.equal (List.length words - 1))
            )


ensureMarked : List String -> TestContext -> TestContext
ensureMarked =
    ensureMarkIndex 0


ensureMarks : List (List String) -> TestContext -> TestContext
ensureMarks marks testContext =
    List.Extra.indexedFoldl (\i words -> ensureMarkIndex i words) testContext marks


ensureMarkIndex : Int -> List String -> TestContext -> TestContext
ensureMarkIndex markI words testContext =
    testContext
        |> ensureView
            (Query.findAll [ Selector.tag "mark" ]
                >> Query.index markI
                >> Query.children [ Selector.class "highlighter-highlightable" ]
                >> Expect.all (List.indexedMap (\i w -> Query.index i >> Query.has [ Selector.text w ]) words)
            )


noneMarked : TestContext -> TestContext
noneMarked =
    ensureView (Query.hasNot [ Selector.tag "mark" ])


space : TestContext -> TestContext
space =
    KeyboardHelpers.pressSpaceKey { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


rightArrow : TestContext -> TestContext
rightArrow =
    KeyboardHelpers.pressRightArrow { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


leftArrow : TestContext -> TestContext
leftArrow =
    KeyboardHelpers.pressLeftArrow { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


shiftRight : TestContext -> TestContext
shiftRight =
    KeyboardHelpers.pressShiftRight { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


shiftLeft : TestContext -> TestContext
shiftLeft =
    KeyboardHelpers.pressShiftLeft { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


releaseShiftRight : TestContext -> TestContext
releaseShiftRight =
    KeyboardHelpers.releaseShiftRight { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


releaseShiftLeft : TestContext -> TestContext
releaseShiftLeft =
    KeyboardHelpers.releaseShiftLeft { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


mouseDown : String -> TestContext -> TestContext
mouseDown word =
    MouseHelpers.cancelableMouseDown [ Selector.tag "span", Selector.containing [ Selector.text word ] ]


mouseUp : String -> TestContext -> TestContext
mouseUp word =
    MouseHelpers.cancelableMouseUp [ Selector.tag "span", Selector.containing [ Selector.text word ] ]


click : String -> TestContext -> TestContext
click word =
    mouseDown word >> mouseUp word


mouseOver : String -> TestContext -> TestContext
mouseOver word =
    MouseHelpers.cancelableMouseOver [ Selector.tag "span", Selector.containing [ Selector.text word ] ]


markerModel : Maybe String -> Tool String
markerModel name =
    Tool.Marker (marker name)


marker : Maybe String -> Tool.MarkerModel String
marker name =
    Tool.buildMarker
        { highlightColor = Colors.magenta
        , hoverColor = Colors.magenta
        , hoverHighlightColor = Colors.magenta
        , kind = Maybe.withDefault "" name
        , name = name
        }


type alias TestContext =
    ProgramTest (Highlighter.Model String) (Highlighter.Msg String) ()


program :
    { name : Maybe String
    , joinAdjacentInteractiveHighlights : Bool
    }
    -> List (Highlightable String)
    -> TestContext
program config highlightables =
    ProgramTest.createSandbox
        { init =
            Highlighter.init
                { id = "test-highlighter-container"
                , highlightables = highlightables
                , marker = markerModel config.name
                , joinAdjacentInteractiveHighlights = config.joinAdjacentInteractiveHighlights
                }
        , update =
            \msg model ->
                case Highlighter.update msg model of
                    ( newModel, _, _ ) ->
                        newModel
        , view = Highlighter.view >> toUnstyled
        }
        |> ProgramTest.start ()


joinAdjacentInteractiveHighlightsTests : List Test
joinAdjacentInteractiveHighlightsTests =
    [ describe "static segments surrounding a single interactive segment" <|
        let
            runTest description { joinAdjacentInteractiveHighlights } =
                test (description ++ ", static elements should not change state") <|
                    \() ->
                        [ Highlightable.init Highlightable.Static Nothing 0 ( [], " " )
                        , Highlightable.init Highlightable.Interactive Nothing 1 ( [], "word" )
                        , Highlightable.init Highlightable.Static Nothing 2 ( [], " " )
                        ]
                            |> program { name = Nothing, joinAdjacentInteractiveHighlights = joinAdjacentInteractiveHighlights }
                            |> click "word"
                            |> ensureMarked [ "word" ]
                            |> click "word"
                            |> noneMarked
                            |> done
        in
        [ runTest "not joining adjacent interactive highlights" { joinAdjacentInteractiveHighlights = False }
        , runTest "joining adjacent interactive highlights" { joinAdjacentInteractiveHighlights = True }
        ]
    , describe "interactive segments surrounding a single static segment" <|
        let
            highlightables =
                [ Highlightable.init Highlightable.Interactive Nothing 0 ( [], "hello" )
                , Highlightable.init Highlightable.Static Nothing 1 ( [], " " )
                , Highlightable.init Highlightable.Interactive Nothing 2 ( [], "world" )
                ]
        in
        [ test "not joining adjacent interactive highlights" <|
            \() ->
                highlightables
                    |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                    |> click "hello"
                    |> ensureMarks [ [ "hello" ] ]
                    |> click "world"
                    |> ensureMarks [ [ "hello" ], [ "world" ] ]
                    |> click "hello"
                    |> ensureMarks [ [ "world" ] ]
                    |> click "world"
                    |> noneMarked
                    |> done
        , test "joining adjacent interactive highlights" <|
            \() ->
                highlightables
                    |> program { name = Nothing, joinAdjacentInteractiveHighlights = True }
                    |> click "hello"
                    |> ensureMarks [ [ "hello" ] ]
                    |> click "world"
                    |> ensureMarks [ [ "hello", " ", "world" ] ]
                    |> click "hello"
                    |> noneMarked
                    |> done
        ]
    , describe "initialization"
        [ test "with matching mark types, not joining adjacent interactive highlights, does not join marks" <|
            \() ->
                [ Highlightable.init Highlightable.Interactive (Just (marker (Just "type-1"))) 0 ( [], "hello" )
                , Highlightable.init Highlightable.Static Nothing 1 ( [], " " )
                , Highlightable.init Highlightable.Interactive (Just (marker (Just "type-1"))) 2 ( [], "world" )
                ]
                    |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                    |> ensureMarks [ [ "hello" ], [ "world" ] ]
                    |> done
        , test "with matching mark types, joining adjacent interactive highlights, joins marks" <|
            \() ->
                [ Highlightable.init Highlightable.Interactive (Just (marker (Just "type-1"))) 0 ( [], "hello" )
                , Highlightable.init Highlightable.Static Nothing 1 ( [], " " )
                , Highlightable.init Highlightable.Interactive (Just (marker (Just "type-1"))) 2 ( [], "world" )
                ]
                    |> program { name = Nothing, joinAdjacentInteractiveHighlights = True }
                    |> ensureMarks [ [ "hello", " ", "world" ] ]
                    |> done
        , test "with differing mark types, not joining adjacent interactive highlights, does not join marks" <|
            \() ->
                [ Highlightable.init Highlightable.Interactive (Just (marker (Just "type-1"))) 0 ( [], "hello" )
                , Highlightable.init Highlightable.Static Nothing 1 ( [], " " )
                , Highlightable.init Highlightable.Interactive (Just (marker (Just "type-2"))) 2 ( [], "world" )
                ]
                    |> program { name = Nothing, joinAdjacentInteractiveHighlights = False }
                    |> ensureMarks [ [ "hello" ], [ "world" ] ]
                    |> done
        , test "with differing mark types, joining adjacent interactive highlights, does not join marks" <|
            \() ->
                [ Highlightable.init Highlightable.Interactive (Just (marker (Just "type-1"))) 0 ( [], "hello" )
                , Highlightable.init Highlightable.Static Nothing 1 ( [], " " )
                , Highlightable.init Highlightable.Interactive (Just (marker (Just "type-2"))) 2 ( [], "world" )
                ]
                    |> program { name = Nothing, joinAdjacentInteractiveHighlights = True }
                    |> ensureMarks [ [ "hello" ], [ "world" ] ]
                    |> done
        ]
    ]
