module Spec.Nri.Ui.Highlighter exposing (spec)

import Accessibility.Aria as Aria
import Accessibility.Key as Key
import Expect exposing (Expectation)
import Html.Styled exposing (Html, toUnstyled)
import Html.Styled.Attributes
import Json.Encode as Encode
import List.Extra
import Nri.Test.KeyboardHelpers.V1 as KeyboardHelpers
import Nri.Test.MouseHelpers.V1 as MouseHelpers
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Highlightable.V3 as Highlightable exposing (Highlightable)
import Nri.Ui.Highlighter.V6 as Highlighter
import Nri.Ui.HighlighterTool.V1 as Tool exposing (Tool)
import ProgramTest exposing (..)
import Sort
import Spec.PseudoElements exposing (..)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)


spec : Test
spec =
    describe "Nri.Ui.Highlighter"
        [ describe "mouse behavior" mouseTests
        , describe "keyboard behavior" keyboardTests
        , describe "markdown highlightable behavior" markdownContentTests
        , describe "markdown highlight name behavior" markdownHighlightNameTests
        , describe "joinAdjacentInteractiveHighlights" joinAdjacentInteractiveHighlightsTests
        , describe "overlapping highlights" overlappingHighlightTests
        , describe "selectShortestMarkerRange" selectShortestMarkerRangeTests
        , describe "scrollFriendly" scrollFriendlyTests
        , describe "intent" intentTests
        ]


mouseTests : List Test
mouseTests =
    [ test "clicking on a static element does nothing" <|
        \() ->
            [ Highlightable.initStatic [] 0 "Pothos"
            , Highlightable.initInteractive [] 1 "Philodendron"
            ]
                |> program []
                |> click "Pothos"
                |> noneMarked
                |> done
    , test "ending a highlight on a static element works" <|
        \() ->
            [ Highlightable.initStatic [] 0 "Pothos"
            , Highlightable.initInteractive [] 1 "Philodendron"
            ]
                |> program []
                |> mouseDown "Philodendron"
                |> mouseUp "Pothos"
                |> ensureNotMarked "Pothos"
                |> ensureMarked [ "Philodendron" ]
                |> done
    , test "static element before a highlight cannot be highlighted" <|
        \() ->
            [ Highlightable.initStatic [] 0 "Pothos"
            , Highlightable.initInteractive [] 1 "Philodendron"
            ]
                |> program []
                |> click "Philodendron"
                |> click "Pothos"
                |> ensureNotMarked "Pothos"
                |> ensureMarked [ "Philodendron" ]
                |> done
    , test "static element after a highlight cannot be highlighted" <|
        \() ->
            [ Highlightable.initInteractive [] 0 "Philodendron"
            , Highlightable.initStatic [] 1 "Pothos"
            ]
                |> program []
                |> click "Philodendron"
                |> ensureMarked [ "Philodendron" ]
                |> click "Pothos"
                |> ensureNotMarked "Pothos"
                |> ensureMarked [ "Philodendron" ]
                |> done
    ]


keyboardTests : List Test
keyboardTests =
    [ test "has a focusable element when there is one" <|
        \() ->
            Highlightable.initFragments "Pothos indirect light"
                |> program []
                |> ensureTabbable "Pothos"
                |> done
    , test "has only one element included in the tab sequence" <|
        \() ->
            Highlightable.initFragments "Pothos indirect light"
                |> program []
                |> ensureOnlyOneInTabSequence (String.words "Pothos indirect light")
                |> done
    , test "moves focus right on right arrow key" <|
        \() ->
            Highlightable.initFragments "Pothos indirect light"
                |> program []
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
            Highlightable.initFragments "Pothos indirect light"
                |> program []
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
            Highlightable.initFragments "Pothos indirect light"
                |> program []
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
            Highlightable.initFragments "Pothos indirect light"
                |> program []
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
            Highlightable.initFragments "Pothos indirect light"
                |> program []
                |> shiftRight
                |> releaseShift
                |> ensureMarked [ "Pothos", " ", "indirect" ]
                |> shiftRight
                |> releaseShift
                |> ensureMarked [ "Pothos", " ", "indirect", " ", "light" ]
                |> shiftRight
                |> releaseShift
                |> ensureMarked [ "Pothos", " ", "indirect", " ", "light" ]
                |> done
    , test "expands selection one element to the left on shift + left arrow and highlight selected elements" <|
        \() ->
            Highlightable.initFragments "Pothos indirect light"
                |> program []
                |> rightArrow
                |> rightArrow
                |> shiftLeft
                |> releaseShift
                |> ensureMarked [ "indirect", " ", "light" ]
                |> shiftLeft
                |> releaseShift
                |> ensureMarked [ "Pothos", " ", "indirect", " ", "light" ]
                |> shiftLeft
                |> releaseShift
                |> ensureMarked [ "Pothos", " ", "indirect", " ", "light" ]
                |> done
    , test "supports hinting multiple segments at a time (right)" <|
        \() ->
            Highlightable.initFragments "Pothos indirect light"
                |> program []
                |> shiftRight
                |> ensureHinted [ "Pothos", " ", "indirect" ]
                |> shiftRight
                |> ensureHinted [ "Pothos", " ", "indirect", " ", "light" ]
                |> shiftRight
                |> ensureHinted [ "Pothos", " ", "indirect", " ", "light" ]
                |> releaseShift
                |> ensureMarked [ "Pothos", " ", "indirect", " ", "light" ]
                |> done
    , test "support hinting multiple elements at a time (left)" <|
        \() ->
            Highlightable.initFragments "Pothos indirect light"
                |> program []
                |> rightArrow
                |> rightArrow
                |> shiftLeft
                |> ensureHinted [ "indirect", " ", "light" ]
                |> shiftLeft
                |> ensureHinted [ "Pothos", " ", "indirect", " ", "light" ]
                |> shiftLeft
                |> ensureHinted [ "Pothos", " ", "indirect", " ", "light" ]
                |> releaseShift
                |> ensureMarked [ "Pothos", " ", "indirect", " ", "light" ]
                |> done
    , test "supports cancelling while hinting" <|
        \() ->
            Highlightable.initFragments "Pothos indirect light"
                |> program []
                |> shiftRight
                |> ensureHinted [ "Pothos", " ", "indirect" ]
                |> releaseShiftEsc
                |> ensureNothingHinted
                |> done
    , test "merges highlights" <|
        \() ->
            Highlightable.initFragments "Pothos indirect light"
                |> program []
                |> ensureTabbable "Pothos"
                |> shiftRight
                |> releaseShift
                |> ensureMarked [ "Pothos", " ", "indirect" ]
                |> ensureTabbable "indirect"
                |> rightArrow
                |> ensureTabbable "light"
                |> shiftLeft
                |> releaseShift
                |> ensureMarked [ "Pothos", " ", "indirect", " ", "light" ]
                |> done
    , test "selects element on MouseDown and highlights selected element on MouseUp" <|
        \() ->
            Highlightable.initFragments "Pothos indirect light"
                |> program []
                |> ensureTabbable "Pothos"
                |> mouseDown "Pothos"
                |> mouseUp "Pothos"
                |> ensureMarked [ "Pothos" ]
                |> done
    , test "selects element on MouseDown, expands selection on MouseOver, and highlights selected elements on MouseUp" <|
        \() ->
            Highlightable.initFragments "Pothos indirect light"
                |> program []
                |> ensureTabbable "Pothos"
                |> mouseDown "Pothos"
                |> mouseOver "indirect"
                |> mouseUp "Pothos"
                |> ensureMarked [ "Pothos", " ", "indirect" ]
                |> done
    , test "Highlights element on Space" <|
        \() ->
            Highlightable.initFragments "Pothos indirect light"
                |> program []
                |> ensureTabbable "Pothos"
                |> space
                |> ensureMarked [ "Pothos" ]
                |> done
    , test "Removes highlight from element on MouseUp" <|
        \() ->
            Highlightable.initFragments "Pothos indirect light"
                |> program []
                |> ensureTabbable "Pothos"
                |> space
                |> ensureMarked [ "Pothos" ]
                |> ensureTabbable "Pothos"
                |> mouseDown "Pothos"
                |> mouseUp "Pothos"
                |> expectViewHasNot [ Selector.tag "mark" ]
    , test "Removes entire highlight from a group of elements on MouseUp" <|
        \() ->
            Highlightable.initFragments "Pothos indirect light"
                |> program []
                |> ensureTabbable "Pothos"
                |> shiftRight
                |> releaseShift
                |> ensureMarked [ "Pothos", " ", "indirect" ]
                |> mouseDown "indirect"
                |> mouseUp "indirect"
                |> expectViewHasNot [ Selector.tag "mark" ]
    , test "Removes highlight from element on Space" <|
        \() ->
            Highlightable.initFragments "Pothos indirect light"
                |> program []
                |> ensureTabbable "Pothos"
                |> space
                |> ensureMarked [ "Pothos" ]
                |> ensureTabbable "Pothos"
                |> space
                |> expectViewHasNot [ Selector.tag "mark" ]
    , test "Adds ::after element with screenreader cues while hinting" <|
        \() ->
            Highlightable.initFragments "Pothos indirect light"
                |> program []
                |> shiftRight
                |> ensureHinted [ "Pothos", " ", "indirect" ]
                |> ensureView (hasAfter "\\(selecting text for highlight\\)" "Pothos")
                |> ensureView (hasAfter "\\(selecting text for highlight\\)" "indirect")
                |> done
    , describe "Regression tests for A11-1767"
        [ test "generic start announcement is made when mark does not include first element" <|
            \() ->
                Highlightable.initFragments "Pothos indirect light"
                    |> program []
                    |> rightArrow
                    |> shiftRight
                    |> releaseShift
                    |> ensureMarked [ "indirect" ]
                    |> expectView (hasBefore "start highlight" "indirect")
        , test "specific start announcement is made when mark does not include first element" <|
            \() ->
                Highlightable.initFragments "Pothos indirect light"
                    |> program [ MarkerName "banana" ]
                    |> rightArrow
                    |> ensureTabbable "indirect"
                    |> shiftRight
                    |> releaseShift
                    |> ensureMarked [ "indirect" ]
                    |> expectView (hasBefore "start banana highlight" "indirect")
        ]
    , describe "Regression tests for A11-1769"
        [ -- as far as I can tell, the problem is not in the Elm logic.
          -- However, it still seemed worth adding an explicit check against the buggy behavior.
          test "Focus moves past 3rd element" <|
            \() ->
                Highlightable.initFragments "Sir Walter Elliot, of Kellynch Hall, in Somersetshire..."
                    |> program [ MarkerName "Claim" ]
                    |> shiftRight
                    |> releaseShift
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


markdownContentTests : List Test
markdownContentTests =
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
                Highlightable.initFragments "*Pothos* prefer indirect [light]() to direct light."
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
                Highlightable.initFragments "*Pothos* prefer indirect [light]() to direct light."
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


markdownHighlightNameTests : List Test
markdownHighlightNameTests =
    let
        model =
            Highlighter.init
                { id = "test-markdown-highlight-names-rendering"
                , highlightables =
                    [ Highlightable.initStatic [ marker (Just "*Markdown label*") ] 0 "Highlightable" ]
                , marker = markerModel Nothing
                , joinAdjacentInteractiveHighlights = False
                , sorter = Sort.alphabetical
                , scrollFriendly = False
                }

        testIt viewName view =
            test viewName <|
                \() ->
                    view model
                        |> toUnstyled
                        |> Query.fromHtml
                        |> Expect.all
                            [ -- The rendered markdown tag should be hidden from SR users, since the label information
                              -- is conveyed another way
                              Query.has
                                [ Selector.attribute (Aria.hidden True)
                                , Selector.containing
                                    [ Selector.tag "em"
                                    , Selector.containing [ Selector.text "Markdown label" ]
                                    ]
                                ]
                            , -- The before and after elements that convey the mark type to AT users
                              -- should not include markdown (e.g., no asterisks)
                              hasBefore "start Markdown label highlight" "Highlightable"
                            ]
    in
    [ testIt "view" Highlighter.view
    , testIt "viewMarkdown" Highlighter.viewMarkdown
    , testIt "static" Highlighter.static
    , testIt "staticMarkdown" Highlighter.staticMarkdown
    , testIt "staticWithTags" Highlighter.staticWithTags
    , testIt "staticMarkdownWithTags" Highlighter.staticMarkdownWithTags
    , testIt "viewWithOverlappingHighlights" Highlighter.viewWithOverlappingHighlights
    , testIt "viewFoldHighlight" renderWithFoldHighlight
    , testIt "viewFoldStatic" renderWithFoldStatic
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
                , scrollFriendly = False
                }
        , update = \_ m -> m
        , view = view >> toUnstyled
        }
        |> ProgramTest.start ()


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


ensureNotMarked : String -> TestContext -> TestContext
ensureNotMarked name =
    ensureViewHasNot
        [ Selector.all
            [ Selector.tag "mark"
            , Selector.containing [ Selector.text name ]
            ]
        ]


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


ensureHinted : List String -> TestContext -> TestContext
ensureHinted words =
    ensureView
        (Query.findAll [ Selector.class "highlighter-hinted" ]
            >> Expect.all (List.indexedMap (\i w -> Query.index i >> Query.has [ Selector.text w ]) words)
        )


ensureNothingHinted : TestContext -> TestContext
ensureNothingHinted =
    ensureViewHasNot [ Selector.class "highlighter-hinted" ]


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
    KeyboardHelpers.pressSpace
        { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


rightArrow : TestContext -> TestContext
rightArrow =
    KeyboardHelpers.pressRightArrow
        { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


leftArrow : TestContext -> TestContext
leftArrow =
    KeyboardHelpers.pressLeftArrow
        { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


shiftRight : TestContext -> TestContext
shiftRight =
    KeyboardHelpers.pressShiftRight
        { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


shiftLeft : TestContext -> TestContext
shiftLeft =
    KeyboardHelpers.pressShiftLeft
        { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


releaseShift : TestContext -> TestContext
releaseShift =
    KeyboardHelpers.releaseShift
        { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


{-| Simulate an escape key release while shift is pressed
-}
releaseShiftEsc : TestContext -> TestContext
releaseShiftEsc =
    KeyboardHelpers.releaseKey
        { targetDetails = [], keyCode = 27, shiftKey = True }
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


type ProgAttr
    = MarkerName String
    | JoinAdjacentInteractiveHighlights
    | ScrollFriendly


program :
    List ProgAttr
    -> List (Highlightable String)
    -> TestContext
program attrs highlightables =
    ProgramTest.createSandbox
        { init =
            Highlighter.init
                { id = "test-highlighter-container"
                , highlightables = highlightables
                , marker =
                    attrs
                        |> List.foldl
                            (\attr acc ->
                                case ( acc, attr ) of
                                    ( Nothing, MarkerName name ) ->
                                        Just (markerModel (Just name))

                                    _ ->
                                        acc
                            )
                            Nothing
                        |> Maybe.withDefault (markerModel Nothing)
                , joinAdjacentInteractiveHighlights = List.member JoinAdjacentInteractiveHighlights attrs
                , sorter = Sort.alphabetical
                , scrollFriendly = List.member ScrollFriendly attrs
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
            runTest description joinAdjacentInteractiveHighlights =
                test (description ++ ", static elements should not change state") <|
                    \() ->
                        [ Highlightable.initStatic [] 0 " "
                        , Highlightable.initInteractive [] 1 "word"
                        , Highlightable.initStatic [] 2 " "
                        ]
                            |> program joinAdjacentInteractiveHighlights
                            |> click "word"
                            |> ensureMarked [ "word" ]
                            |> click "word"
                            |> noneMarked
                            |> done
        in
        [ runTest "not joining adjacent interactive highlights" []
        , runTest "joining adjacent interactive highlights" [ JoinAdjacentInteractiveHighlights ]
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
                    |> program []
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
                    |> program [ JoinAdjacentInteractiveHighlights ]
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
                    |> program []
                    |> ensureMarks [ [ "hello" ], [ "world" ] ]
                    |> done
        , test "with matching mark types, joining adjacent interactive highlights, joins marks" <|
            \() ->
                [ Highlightable.initInteractive [ marker (Just "type-1") ] 0 "hello"
                , Highlightable.initStatic [] 1 " "
                , Highlightable.initInteractive [ marker (Just "type-1") ] 2 "world"
                ]
                    |> program [ JoinAdjacentInteractiveHighlights ]
                    |> ensureMarks [ [ "hello", " ", "world" ] ]
                    |> done
        , test "with differing mark types, not joining adjacent interactive highlights, does not join marks" <|
            \() ->
                [ Highlightable.initInteractive [ marker (Just "type-1") ] 0 "hello"
                , Highlightable.initStatic [] 1 " "
                , Highlightable.initInteractive [ marker (Just "type-2") ] 2 "world"
                ]
                    |> program []
                    |> ensureMarks [ [ "hello" ], [ "world" ] ]
                    |> done
        , test "with differing mark types, joining adjacent interactive highlights, does not join marks" <|
            \() ->
                [ Highlightable.initInteractive [ marker (Just "type-1") ] 0 "hello"
                , Highlightable.initStatic [] 1 " "
                , Highlightable.initInteractive [ marker (Just "type-2") ] 2 "world"
                ]
                    |> program [ JoinAdjacentInteractiveHighlights ]
                    |> ensureMarks [ [ "hello" ], [ "world" ] ]
                    |> done
        ]
    ]


renderWithFoldHighlight : Highlighter.Model marker -> Html (Highlighter.Msg marker)
renderWithFoldHighlight model =
    List.Extra.mapAccuml
        (\state _ -> Highlighter.viewFoldHighlighter [] state)
        (Highlighter.initFoldState model)
        model.highlightables
        |> Tuple.second
        |> List.concat
        |> Html.Styled.p [ Html.Styled.Attributes.id "test-id", Html.Styled.Attributes.class "highlighter-container" ]


renderWithFoldStatic : Highlighter.Model marker -> Html (Highlighter.Msg marker)
renderWithFoldStatic model =
    List.Extra.mapAccuml
        (\state _ -> Highlighter.viewFoldStatic [] state)
        (Highlighter.initFoldState model)
        model.highlightables
        |> Tuple.second
        |> List.concat
        |> Html.Styled.p [ Html.Styled.Attributes.id "test-id", Html.Styled.Attributes.class "highlighter-container" ]


initHighlightables : List ( String, List String ) -> List (Highlightable String)
initHighlightables =
    List.indexedMap
        (\i ( text, marks ) ->
            Highlightable.initInteractive (List.map (Just >> marker) marks) i text
        )


overlappingHighlightTests : List Test
overlappingHighlightTests =
    let
        start renderer highlightables =
            ProgramTest.createSandbox
                { init =
                    Highlighter.init
                        { id = "test-highlighter-container"
                        , highlightables = initHighlightables highlightables
                        , marker = markerModel (Just "Comment")
                        , joinAdjacentInteractiveHighlights = False
                        , sorter = Sort.alphabetical
                        , scrollFriendly = False
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
    , describe "viewFoldHighlight" (staticAssertions renderWithFoldHighlight)
    , describe "viewFoldStatic" (staticAssertions renderWithFoldStatic)
    ]


selectShortestMarkerRangeTests : List Test
selectShortestMarkerRangeTests =
    let
        init highlightables =
            Highlighter.init
                { id = "test-highlighter-container"
                , highlightables =
                    initHighlightables
                        highlightables
                , marker = markerModel (Just "Comment")
                , joinAdjacentInteractiveHighlights = False
                , sorter = Sort.alphabetical
                , scrollFriendly = False
                }
    in
    [ test "No marker" <|
        \_ ->
            init [ ( "Hello", [] ), ( "World", [] ) ]
                |> Highlighter.selectShortestMarkerRange 1
                |> Expect.equal ( Nothing, ( 1, 1 ) )
    , test "Single marker" <|
        \_ ->
            init [ ( "Hello", [] ), ( "World", [ "a" ] ) ]
                |> Highlighter.selectShortestMarkerRange 1
                |> Expect.equal ( Just "a", ( 1, 1 ) )
    , test "Multiple markers" <|
        \_ ->
            init [ ( "Hello", [] ), ( "World", [ "a", "b" ] ) ]
                |> Highlighter.selectShortestMarkerRange 1
                |> Expect.equal ( Just "a", ( 1, 1 ) )
    , test "Longer to the left" <|
        \_ ->
            init [ ( "Hello", [ "a" ] ), ( "World", [ "a", "b" ] ) ]
                |> Highlighter.selectShortestMarkerRange 1
                |> Expect.equal ( Just "b", ( 1, 1 ) )
    , test "Longer to the right" <|
        \_ ->
            init [ ( "Hello", [] ), ( "World", [ "a", "b" ] ), ( "olleH", [ "a" ] ) ]
                |> Highlighter.selectShortestMarkerRange 1
                |> Expect.equal ( Just "b", ( 1, 1 ) )
    , test "Same size different sides" <|
        \_ ->
            init [ ( "Hello", [ "a" ] ), ( "World", [ "a", "b" ] ), ( "olleH", [ "b" ] ) ]
                |> Highlighter.selectShortestMarkerRange 1
                |> Expect.equal ( Just "a", ( 0, 1 ) )
    , test "Non-contiguous" <|
        \_ ->
            init
                [ ( "0", [ "b" ] )
                , ( "1", [ "b" ] )
                , ( "2", [ "a", "b" ] )
                , ( "3", [ "a", "b" ] ) -- index
                , ( "4", [ "a", "b" ] )
                , ( "5", [ "a" ] )
                , ( "6", [ "a", "b" ] )
                , ( "7", [ "a", "b" ] )
                ]
                |> Highlighter.selectShortestMarkerRange 3
                |> Expect.equal ( Just "b", ( 0, 4 ) )
    , test "Word size matters" <|
        \_ ->
            init
                [ ( "1234", [ "a" ] )
                , ( "5|1", [ "a", "b" ] )
                , ( "2", [ "b" ] )
                , ( "3", [ "b" ] )
                , ( "4", [ "b" ] )
                ]
                |> Highlighter.selectShortestMarkerRange 1
                |> Expect.equal ( Just "b", ( 1, 4 ) )
    ]


singleClick : String -> TestContext -> TestContext
singleClick word =
    mouseDown word >> mouseUp word >> clickHighlight 1 word


doubleClick : String -> TestContext -> TestContext
doubleClick word =
    mouseDown word
        >> mouseUp word
        >> clickHighlight 1 word
        >> mouseDown word
        >> mouseUp word
        >> clickHighlight 2 word


clickHighlight : Int -> String -> TestContext -> TestContext
clickHighlight count word =
    ProgramTest.simulateDomEvent
        (Query.find [ Selector.tag "span", Selector.containing [ Selector.text word ] ])
        (Event.custom
            "click"
            (Encode.object [ ( "cancelable", Encode.bool True ), ( "detail", Encode.int count ) ])
        )


scrollFriendlyTests : List Test
scrollFriendlyTests =
    [ test "drag to highlight with scrollFriendly works" <|
        \() ->
            [ Highlightable.initStatic [] 0 "Pothole"
            , Highlightable.initInteractive [] 1 "Philadelphia"
            ]
                |> program [ ScrollFriendly ]
                |> mouseDown "Philadelphia"
                |> mouseOver "Pothole"
                |> mouseUp "Pothole"
                |> ensureNotMarked "Pothole"
                |> ensureMarked [ "Philadelphia" ]
                |> done
    , test "click to highlight with scrollFriendly doesn't work" <|
        \() ->
            [ Highlightable.initInteractive [] 1 "Philadelphia"
            ]
                |> program [ ScrollFriendly ]
                |> singleClick "Philadelphia"
                |> ensureNotMarked "Philadelphia"
                |> done
    , test "double click to highlight with scrollFriendly works" <|
        \() ->
            [ Highlightable.initInteractive [] 1 "Philadelphia"
            ]
                |> program [ ScrollFriendly ]
                |> doubleClick "Philadelphia"
                |> ensureMarked [ "Philadelphia" ]
                |> done
    ]


intentTests =
    [ test "highlighting shows creation intent" <|
        \() ->
            [ Highlightable.initInteractive [] 1 "Philadelphia"
            ]
                |> program []
                |> mouseDown "Philadelphia"
                |> expectModel
                    (\model ->
                        case Highlighter.update (Highlighter.Pointer (Highlighter.Up Nothing)) model of
                            ( _, _, Highlighter.Intent { changed } ) ->
                                case changed of
                                    Highlighter.Changed (Highlighter.HighlightCreated _ _) ->
                                        Expect.pass

                                    _ ->
                                        Expect.fail ("Expected HighlightCreated, but got: " ++ Debug.toString changed)
                    )
    , test "clicking highlight shows removal intent" <|
        \() ->
            [ Highlightable.initInteractive [] 1 "Philadelphia"
            ]
                |> program []
                |> mouseDown "Philadelphia"
                |> mouseUp "Philadelphia"
                |> ensureMarked [ "Philadelphia" ]
                |> mouseDown "Philadelphia"
                |> expectModel
                    (\model ->
                        case Highlighter.update (Highlighter.Pointer (Highlighter.Up Nothing)) model of
                            ( _, _, Highlighter.Intent { changed } ) ->
                                case changed of
                                    Highlighter.Changed (Highlighter.HighlightRemoved _ _) ->
                                        Expect.pass

                                    _ ->
                                        Expect.fail ("Expected HighlightRemoved, but got: " ++ Debug.toString changed)
                    )
    ]
