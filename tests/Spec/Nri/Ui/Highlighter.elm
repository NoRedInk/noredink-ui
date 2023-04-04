module Spec.Nri.Ui.Highlighter exposing (spec)

import Accessibility.Key as Key
import Expect exposing (Expectation)
import Html.Styled exposing (Html, toUnstyled)
import List.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Highlightable.V3 as Highlightable exposing (Highlightable)
import Nri.Ui.Highlighter.V4 as Highlighter
import Nri.Ui.HighlighterTool.V1 as Tool exposing (Tool)
import ProgramTest exposing (..)
import Regex exposing (Regex)
import Sort
import Spec.KeyboardHelpers as KeyboardHelpers
import Spec.MouseHelpers as MouseHelpers
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)


spec : Test
spec =
    describe "Nri.Ui.Highlighter"
        [ describe "keyboard behavior" keyboardTests
        , describe "markdown behavior" markdownTests
        , describe "joinAdjacentInteractiveHighlights" joinAdjacentInteractiveHighlightsTests
        , describe "selectShortest" selectShortestTests
        , describe "overlapping highlights" overlappingHighlightTests
        ]


keyboardTests : List Test
keyboardTests =
    [ test "has a focusable element when there is one" <|
        \() ->
            Highlightable.initFragments [] "Pothos indirect light"
                |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> done
    , test "has only one element included in the tab sequence" <|
        \() ->
            Highlightable.initFragments [] "Pothos indirect light"
                |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureOnlyOneInTabSequence (String.words "Pothos indirect light")
                |> done
    , test "moves focus right on right arrow key" <|
        \() ->
            Highlightable.initFragments [] "Pothos indirect light"
                |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
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
            Highlightable.initFragments [] "Pothos indirect light"
                |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
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
            Highlightable.initFragments [] "Pothos indirect light"
                |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
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
            Highlightable.initFragments [] "Pothos indirect light"
                |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
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
            Highlightable.initFragments [] "Pothos indirect light"
                |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
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
            Highlightable.initFragments [] "Pothos indirect light"
                |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
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
            Highlightable.initFragments [] "Pothos indirect light"
                |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
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
            Highlightable.initFragments [] "Pothos indirect light"
                |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> mouseDown "Pothos"
                |> mouseUp "Pothos"
                |> ensureMarked [ "Pothos" ]
                |> done
    , test "selects element on MouseDown, expands selection on MouseOver, and highlights selected elements on MouseUp" <|
        \() ->
            Highlightable.initFragments [] "Pothos indirect light"
                |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> mouseDown "Pothos"
                |> mouseOver "indirect"
                |> mouseUp "Pothos"
                |> ensureMarked [ "Pothos", " ", "indirect" ]
                |> done
    , test "Highlights element on Space" <|
        \() ->
            Highlightable.initFragments [] "Pothos indirect light"
                |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> space
                |> ensureMarked [ "Pothos" ]
                |> done
    , test "Removes highlight from element on MouseUp" <|
        \() ->
            Highlightable.initFragments [] "Pothos indirect light"
                |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> space
                |> ensureMarked [ "Pothos" ]
                |> ensureTabbable "Pothos"
                |> mouseDown "Pothos"
                |> mouseUp "Pothos"
                |> expectViewHasNot [ Selector.tag "mark" ]
    , test "Removes entire highlight from a group of elements on MouseUp" <|
        \() ->
            Highlightable.initFragments [] "Pothos indirect light"
                |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> shiftRight
                |> releaseShiftRight
                |> ensureMarked [ "Pothos", " ", "indirect" ]
                |> mouseDown "indirect"
                |> mouseUp "indirect"
                |> expectViewHasNot [ Selector.tag "mark" ]
    , test "Removes highlight from element on Space" <|
        \() ->
            Highlightable.initFragments [] "Pothos indirect light"
                |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
                |> ensureTabbable "Pothos"
                |> space
                |> ensureMarked [ "Pothos" ]
                |> ensureTabbable "Pothos"
                |> space
                |> expectViewHasNot [ Selector.tag "mark" ]
    , describe "Regression tests for A11-1767"
        [ test "generic start announcement is made when mark does not include first element" <|
            \() ->
                Highlightable.initFragments [] "Pothos indirect light"
                    |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
                    |> rightArrow
                    |> shiftRight
                    |> releaseShiftRight
                    |> ensureMarked [ "indirect" ]
                    |> expectView (hasBefore "start highlight" "indirect")
        , test "specific start announcement is made when mark does not include first element" <|
            \() ->
                Highlightable.initFragments [] "Pothos indirect light"
                    |> program { markerName = Just "banana", joinAdjacentInteractiveHighlights = False }
                    |> rightArrow
                    |> ensureTabbable "indirect"
                    |> shiftRight
                    |> releaseShiftRight
                    |> ensureMarked [ "indirect" ]
                    |> expectView (hasBefore "start banana highlight" "indirect")
        ]
    , describe "Regression tests for A11-1769"
        [ -- as far as I can tell, the problem is not in the Elm logic.
          -- However, it still seemed worth adding an explicit check against the buggy behavior.
          test "Focus moves past 3rd element" <|
            \() ->
                Highlightable.initFragments [] "Sir Walter Elliot, of Kellynch Hall, in Somersetshire..."
                    |> program { markerName = Just "Claim", joinAdjacentInteractiveHighlights = False }
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


testRendersRawContent : String -> (Highlighter.Model () -> Html msg) -> Test
testRendersRawContent testName view =
    describe (testName ++ " does not interpret content as markdown")
        [ test "using Highlightable.initFragments for initialization" <|
            \() ->
                Highlightable.initFragments [] "*Pothos* prefer indirect [light]() to direct light."
                    |> startWithoutMarker view
                    |> expectView
                        (Expect.all
                            [ [ "*Pothos*", " ", "prefer", " ", "indirect", " ", "[light]()", " ", "to", " ", "direct", " ", "light." ]
                                |> List.indexedMap (\i word -> highlightable i [ Selector.text word ])
                                |> Expect.all
                            , Query.hasNot [ mark ]
                            ]
                        )
        , test "using Highlightable.fromMarkdown for initialization" <|
            \() ->
                Highlightable.fromMarkdown "*Pothos* prefer indirect [light]() to direct light."
                    |> startWithoutMarker view
                    |> expectView
                        (Expect.all
                            [ highlightable 0 [ Selector.text "*Pothos* prefer indirect " ]
                            , highlightable 1 [ Selector.text "light" ]
                            , highlightable 2 [ Selector.text " to direct light." ]
                            , Query.has [ mark, Selector.containing [ Selector.text "light" ] ]
                            ]
                        )
        ]


testRendersMarkdownContent : String -> (Highlighter.Model () -> Html msg) -> Test
testRendersMarkdownContent testName view =
    describe (testName ++ " does interpret content as markdown") <|
        [ test "using Highlightable.initFragments for initialization" <|
            \() ->
                Highlightable.initFragments [] "*Pothos* prefer indirect [light]() to direct light."
                    |> startWithoutMarker view
                    |> ensureViewHasNot [ Selector.text "*Pothos*" ]
                    |> expectView
                        (Expect.all
                            [ [ "Pothos", " ", "prefer", " ", "indirect", " ", "light", " ", "to", " ", "direct", " ", "light." ]
                                |> List.indexedMap (\i word -> highlightable i [ Selector.text word ])
                                |> Expect.all
                            , Query.has
                                [ Selector.tag "em"
                                , Selector.containing [ Selector.text "Pothos" ]
                                ]
                            , Query.has [ Selector.tag "a", Selector.containing [ Selector.text "light" ] ]
                            ]
                        )
        , test "using Highlightable.fromMarkdown for initialization" <|
            \() ->
                Highlightable.fromMarkdown "*Pothos* prefer indirect [light]() to direct light."
                    |> startWithoutMarker view
                    |> ensureViewHasNot [ Selector.text "*Pothos*" ]
                    |> ensureViewHasNot [ Selector.text "[light]()" ]
                    |> ensureViewHasNot [ Selector.tag "a" ]
                    |> expectView
                        (Expect.all
                            [ highlightable 0
                                [ Selector.all
                                    [ Selector.tag "em"
                                    , Selector.containing [ Selector.text "Pothos" ]
                                    ]
                                , Selector.text " "
                                , Selector.text "prefer indirect"
                                , Selector.text " "
                                ]
                            , highlightable 1 [ Selector.text "light" ]
                            , highlightable 2 [ Selector.text " ", Selector.text "to direct light." ]
                            , Query.has
                                [ Selector.tag "em"
                                , Selector.containing [ Selector.text "Pothos" ]
                                ]
                            , Query.has [ mark, Selector.containing [ Selector.text "light" ] ]
                            ]
                        )
        ]


startWithoutMarker : (Highlighter.Model () -> Html msg) -> List (Highlightable ()) -> ProgramTest (Highlighter.Model ()) msg ()
startWithoutMarker view highlightables =
    ProgramTest.createSandbox
        { init =
            Highlighter.init
                { id = "highlighter-container"
                , highlightables = highlightables
                , marker = Tool.Eraser Tool.buildEraser
                , joinAdjacentInteractiveHighlights = False
                , sorter = Sort.custom (\() () -> EQ)
                }
        , update = \_ m -> m
        , view = view >> toUnstyled
        }
        |> ProgramTest.start ()


hasBefore : String -> String -> Query.Single msg -> Expectation
hasBefore =
    hasPseudoElement "::before"


hasAfter : String -> String -> Query.Single msg -> Expectation
hasAfter =
    hasPseudoElement "::after"


hasNotBefore : String -> String -> Query.Single msg -> Expectation
hasNotBefore =
    hasNotPseudoElement "::before"


hasNotAfter : String -> String -> Query.Single msg -> Expectation
hasNotAfter =
    hasNotPseudoElement "::after"


hasPseudoElement : String -> String -> String -> Query.Single msg -> Expectation
hasPseudoElement pseudoElement highlightMarker relevantHighlightableText view =
    case pseudoElementSelector pseudoElement highlightMarker view of
        Just className ->
            Query.has
                [ className
                , Selector.containing [ Selector.text relevantHighlightableText ]
                ]
                view

        Nothing ->
            ("Expected to find a class defining a " ++ pseudoElement ++ " element with content: `")
                ++ highlightMarker
                ++ "`, but failed to find the class in the styles: \n\n"
                ++ rawStyles view
                |> Expect.fail


hasNotPseudoElement : String -> String -> String -> Query.Single msg -> Expectation
hasNotPseudoElement pseudoElement highlightMarker relevantHighlightableText view =
    case pseudoElementSelector pseudoElement highlightMarker view of
        Just className ->
            view
                |> Query.findAll [ className ]
                |> Query.each (Query.hasNot [ Selector.containing [ Selector.text relevantHighlightableText ] ])

        Nothing ->
            Expect.pass


pseudoElementSelector : String -> String -> Query.Single msg -> Maybe Selector
pseudoElementSelector pseudoElement highlightMarker view =
    let
        startHighlightClassRegex : Maybe Regex
        startHighlightClassRegex =
            ("\\.(\\_[a-zA-Z0-9]+)" ++ pseudoElement ++ "\\{content:\\\\\"\\s*\\s*")
                ++ highlightMarker
                |> Regex.fromString

        maybeClassName : Maybe String
        maybeClassName =
            startHighlightClassRegex
                |> Maybe.andThen
                    (\regex ->
                        Regex.find regex (rawStyles view)
                            |> List.head
                            |> Maybe.andThen (.submatches >> List.head)
                    )
                |> Maybe.withDefault Nothing
    in
    Maybe.map Selector.class maybeClassName


rawStyles : Query.Single msg -> String
rawStyles view =
    view
        |> Query.find [ Selector.tag "style" ]
        |> Query.children []
        |> Debug.toString


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
    ensureView (Query.hasNot [ mark ])


mark : Selector
mark =
    Selector.tag "mark"


highlightable : Int -> List Selector -> Query.Single msg -> Expectation
highlightable index selector =
    Query.findAll [ Selector.class "highlighter-highlightable" ]
        >> Query.index index
        >> Query.has selector


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
    { markerName : Maybe String
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
                , marker = markerModel config.markerName
                , joinAdjacentInteractiveHighlights = config.joinAdjacentInteractiveHighlights
                , sorter = Sort.alphabetical
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
                        [ Highlightable.initStatic [] 0 " "
                        , Highlightable.initInteractive [] 1 "word"
                        , Highlightable.initStatic [] 2 " "
                        ]
                            |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = joinAdjacentInteractiveHighlights }
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
                [ Highlightable.initInteractive [] 0 "hello"
                , Highlightable.initStatic [] 1 " "
                , Highlightable.initInteractive [] 2 "world"
                ]
        in
        [ test "not joining adjacent interactive highlights" <|
            \() ->
                highlightables
                    |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
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
                    |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = True }
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
                [ Highlightable.initInteractive [ marker (Just "type-1") ] 0 "hello"
                , Highlightable.initStatic [] 1 " "
                , Highlightable.initInteractive [ marker (Just "type-1") ] 2 "world"
                ]
                    |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
                    |> ensureMarks [ [ "hello" ], [ "world" ] ]
                    |> done
        , test "with matching mark types, joining adjacent interactive highlights, joins marks" <|
            \() ->
                [ Highlightable.initInteractive [ marker (Just "type-1") ] 0 "hello"
                , Highlightable.initStatic [] 1 " "
                , Highlightable.initInteractive [ marker (Just "type-1") ] 2 "world"
                ]
                    |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = True }
                    |> ensureMarks [ [ "hello", " ", "world" ] ]
                    |> done
        , test "with differing mark types, not joining adjacent interactive highlights, does not join marks" <|
            \() ->
                [ Highlightable.initInteractive [ marker (Just "type-1") ] 0 "hello"
                , Highlightable.initStatic [] 1 " "
                , Highlightable.initInteractive [ marker (Just "type-2") ] 2 "world"
                ]
                    |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = False }
                    |> ensureMarks [ [ "hello" ], [ "world" ] ]
                    |> done
        , test "with differing mark types, joining adjacent interactive highlights, does not join marks" <|
            \() ->
                [ Highlightable.initInteractive [ marker (Just "type-1") ] 0 "hello"
                , Highlightable.initStatic [] 1 " "
                , Highlightable.initInteractive [ marker (Just "type-2") ] 2 "world"
                ]
                    |> program { markerName = Nothing, joinAdjacentInteractiveHighlights = True }
                    |> ensureMarks [ [ "hello" ], [ "world" ] ]
                    |> done
        ]
    ]


selectShortestTests : List Test
selectShortestTests =
    let
        init highlightables =
            Highlighter.init
                { id = "test-highlighter-container"
                , highlightables = highlightables
                , marker = markerModel (Just "A")
                , joinAdjacentInteractiveHighlights = False
                , sorter = Sort.alphabetical
                }
    in
    [ test "without any highlightables, returns Nothing" <|
        \() ->
            init []
                |> Highlighter.selectShortest (.highlightables >> List.Extra.getAt 0)
                |> Expect.equal Nothing
    , test "with 1 unmarked highlightable, returns Nothing" <|
        \() ->
            init [ Highlightable.initInteractive [] 0 "hello" ]
                |> Highlighter.selectShortest (.highlightables >> List.Extra.getAt 0)
                |> Expect.equal Nothing
    , test "with 1 marked highlightable, returns the singular mark type" <|
        \() ->
            init [ Highlightable.initInteractive [ marker (Just "B") ] 0 "hello" ]
                |> Highlighter.selectShortest (.highlightables >> List.Extra.getAt 0)
                |> Expect.equal (Just "B")
    , test "with 1 multi-marked highlightable, returns one of the mark types depending on sort order" <|
        \() ->
            init
                [ Highlightable.initInteractive
                    [ marker (Just "C")
                    , marker (Just "B")
                    , marker (Just "D")
                    ]
                    0
                    "hello"
                ]
                |> Highlighter.selectShortest (.highlightables >> List.Extra.getAt 0)
                |> Expect.equal (Just "B")
    , test "with multiple marked highlightables, selecting the first highlightable, returns the shortest mark type" <|
        \() ->
            init
                [ Highlightable.initInteractive
                    [ marker (Just "C")
                    , marker (Just "B")
                    ]
                    0
                    "hello"
                , Highlightable.initInteractive
                    [ marker (Just "B")
                    ]
                    1
                    "world"
                , Highlightable.initInteractive
                    [ marker (Just "C")
                    ]
                    2
                    "!"
                ]
                |> Highlighter.selectShortest (.highlightables >> List.Extra.getAt 0)
                |> Expect.equal (Just "C")
    , test "with multiple marked highlightables, selecting the second highlightable, returns the shortest mark type" <|
        \() ->
            init
                [ Highlightable.initInteractive
                    [ marker (Just "B")
                    ]
                    0
                    "hello"
                , Highlightable.initInteractive
                    [ marker (Just "B")
                    , marker (Just "C")
                    ]
                    1
                    "world"
                , Highlightable.initInteractive
                    [ marker (Just "C")
                    ]
                    2
                    "!"
                ]
                |> Highlighter.selectShortest (.highlightables >> List.Extra.getAt 1)
                |> Expect.equal (Just "C")
    , test "with multiple marked highlightables, selecting the third highlightable, returns the shortest mark type" <|
        \() ->
            init
                [ Highlightable.initInteractive
                    [ marker (Just "B")
                    ]
                    0
                    "hello"
                , Highlightable.initInteractive
                    [ marker (Just "B")
                    , marker (Just "C")
                    ]
                    1
                    "world"
                , Highlightable.initInteractive
                    [ marker (Just "C")
                    , marker (Just "B")
                    ]
                    2
                    "!"
                ]
                |> Highlighter.selectShortest (.highlightables >> List.Extra.getAt 2)
                |> Expect.equal (Just "C")
    ]


overlappingHighlightTests : List Test
overlappingHighlightTests =
    let
        initHighlightables : List ( String, List String ) -> List (Highlightable String)
        initHighlightables =
            List.indexedMap
                (\i ( text, marks ) ->
                    Highlightable.initInteractive (List.map (Just >> marker) marks) i text
                )

        start renderer highlightables =
            ProgramTest.createSandbox
                { init =
                    Highlighter.init
                        { id = "test-highlighter-container"
                        , highlightables = initHighlightables highlightables
                        , marker = markerModel (Just "Comment")
                        , joinAdjacentInteractiveHighlights = False
                        , sorter = Sort.alphabetical
                        }
                , update =
                    \msg model ->
                        case Highlighter.update msg model of
                            ( newModel, _, _ ) ->
                                newModel
                , view = renderer >> toUnstyled
                }
                |> ProgramTest.start ()

        staticAssertions renderer =
            [ describe "without any overlaps"
                [ test "renders a single ::before element" <|
                    \() ->
                        [ ( "Hello", [ "A" ] ), ( "World", [ "A" ] ) ]
                            |> start renderer
                            |> ensureView (hasBefore "start A highlight" "Hello")
                            |> ensureView (hasNotBefore "start A highlight" "World")
                            |> done
                , test "renders a single ::after element" <|
                    \() ->
                        [ ( "Hello", [ "A" ] ), ( "World", [ "A" ] ) ]
                            |> start renderer
                            |> ensureView (hasAfter "end A highlight" "World")
                            |> ensureView (hasNotAfter "end A highlight" "Hello")
                            |> done
                ]
            , describe "existing overlapping highlights with the same start segment"
                [ test "renders a single ::before element for both marks" <|
                    \() ->
                        [ ( "Hello", [ "A", "B" ] ), ( "World", [ "A", "B" ] ), ( "!", [ "B" ] ) ]
                            |> start renderer
                            |> ensureView (hasBefore "start A and B highlights" "Hello")
                            |> ensureView (hasNotBefore "start A and B highlights" "World")
                            |> ensureView (hasNotBefore "start B highlight" "!")
                            |> done
                , test "uses Oxford comma for more-than-2 marks" <|
                    \() ->
                        [ ( "Hello", [ "A", "B", "C" ] ), ( "World", [ "A", "B" ] ), ( "!", [ "B" ] ) ]
                            |> start renderer
                            |> ensureView (hasBefore "start A, B, and C highlights" "Hello")
                            |> done
                ]
            , describe "existing overlapping highlights with the same end segment"
                [ test "renders a single ::after element for both marks" <|
                    \() ->
                        [ ( "Hello", [ "A" ] ), ( "World", [ "A", "B" ] ), ( "!", [ "A", "B" ] ) ]
                            |> start renderer
                            |> ensureView (hasAfter "end A and B highlights" "!")
                            |> ensureView (hasNotAfter "end A highlight" "Hello,")
                            |> ensureView (hasNotAfter "end A and B highlights" "World")
                            |> done
                , test "uses Oxford comma for more-than-2 marks" <|
                    \() ->
                        [ ( "Hello", [ "A" ] ), ( "World", [ "B" ] ), ( "!", [ "A", "B", "C" ] ) ]
                            |> start renderer
                            |> ensureView (hasAfter "end A, B, and C highlights" "!")
                            |> done
                ]
            , describe "existing overlapping highlights with differing start and end segments"
                [ test "renders individual ::before and ::after elements" <|
                    \() ->
                        [ ( "Hello", [ "A" ] )
                        , ( " ", [] )
                        , ( "World!", [ "A", "B" ] )
                        , ( " ", [] )
                        , ( "Hope you're", [ "A", "B" ] )
                        , ( " ", [] )
                        , ( "well", [ "B" ] )
                        ]
                            |> start renderer
                            |> ensureView (hasBefore "start A highlight" "Hello")
                            |> ensureView (hasAfter "end A highlight" "Hope you're")
                            |> ensureView (hasBefore "start B highlight" "World!")
                            |> ensureView (hasAfter "end B highlight" "well")
                            |> done
                ]
            ]
    in
    [ describe "viewWithOverlappingHighlights" (staticAssertions Highlighter.viewWithOverlappingHighlights)
    , describe "staticWithOverlappingHighlights" (staticAssertions Highlighter.staticWithOverlappingHighlights)
    ]
