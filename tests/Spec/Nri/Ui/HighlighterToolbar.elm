module Spec.Nri.Ui.HighlighterToolbar exposing (..)

import Accessibility.Aria as Aria
import Accessibility.Key as Key
import Browser.Dom as Dom
import Css exposing (Color)
import Expect
import Html.Attributes as Attributes
import Html.Styled as Html exposing (..)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.HighlighterToolbar.V2 as HighlighterToolbar
import ProgramTest exposing (..)
import Spec.KeyboardHelpers as KeyboardHelpers
import Task
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.HighlighterToolbar.V1"
        [ describe "tool selection" selectionTests
        , describe "keyboard behavior" keyboardTests
        ]


selectionTests : List Test
selectionTests =
    [ test "sets aria-pressed to true on the active tool only" <|
        \() ->
            program
                |> clickTool "Claim"
                |> ensureActiveHasAriaPressedTrue "Claim"
                |> ensureNotActiveHaveAriaPressedFalse [ "Evidence", "Reasoning", "Remove highlight" ]
                |> clickTool "Evidence"
                |> ensureActiveHasAriaPressedTrue "Evidence"
                |> ensureNotActiveHaveAriaPressedFalse [ "Claim", "Reasoning", "Remove highlight" ]
                |> done
    , test "adds visual indicator to the active tool only" <|
        \() ->
            program
                |> clickTool "Claim"
                |> ensureActiveHasVisualIndicator
                |> ensureNotActiveDoNotHaveVisualIndicator
                |> done
    ]


keyboardTests : List Test
keyboardTests =
    [ test "has a focusable tool" <|
        \() ->
            program
                |> ensureTabbable "Remove highlight"
                |> done
    , test "has only one tool included in the tab sequence" <|
        \() ->
            program
                |> ensureOnlyOneInTabSequence [ "Claim", "Evidence", "Reasoning", "Remove highlight" ]
                |> done
    , test "moves focus right on right arrow key. Should wrap focus on last element." <|
        \() ->
            program
                |> ensureTabbable "Remove highlight"
                |> rightArrow
                |> ensureTabbable "Claim"
                |> ensureOnlyOneInTabSequence [ "Claim", "Evidence", "Reasoning", "Remove highlight" ]
                |> rightArrow
                |> ensureTabbable "Evidence"
                |> rightArrow
                |> ensureTabbable "Reasoning"
                |> rightArrow
                |> ensureTabbable "Remove highlight"
                |> done
    , test "moves focus left on left arrow key. Should wrap focus on first element." <|
        \() ->
            program
                |> ensureTabbable "Remove highlight"
                |> leftArrow
                |> ensureTabbable "Reasoning"
                |> leftArrow
                |> ensureTabbable "Evidence"
                |> leftArrow
                |> ensureTabbable "Claim"
                |> leftArrow
                |> ensureTabbable "Remove highlight"
                |> ensureOnlyOneInTabSequence [ "Claim", "Evidence", "Reasoning", "Remove highlight" ]
                |> done
    ]


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


rightArrow : TestContext -> TestContext
rightArrow =
    KeyboardHelpers.pressRightArrow { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


leftArrow : TestContext -> TestContext
leftArrow =
    KeyboardHelpers.pressLeftArrow { targetDetails = [] }
        [ Selector.attribute (Key.tabbable True) ]


clickTool : String -> ProgramTest model msg effect -> ProgramTest model msg effect
clickTool label =
    ProgramTest.simulateDomEvent
        (Query.find
            [ Selector.tag "button"
            , Selector.containing [ Selector.text label ]
            ]
        )
        Event.click


ensureActiveHasAriaPressedTrue : String -> TestContext -> TestContext
ensureActiveHasAriaPressedTrue label testContext =
    testContext
        |> ensureView
            (Query.find [ Selector.attribute (Aria.pressed (Just True)) ]
                >> Query.has [ Selector.text label ]
            )


ensureNotActiveHaveAriaPressedFalse : List String -> TestContext -> TestContext
ensureNotActiveHaveAriaPressedFalse labels testContext =
    testContext
        |> ensureView
            (Query.findAll [ Selector.attribute (Aria.pressed (Just False)) ]
                >> Expect.all (List.indexedMap (\i label -> Query.index i >> Query.has [ Selector.text label ]) labels)
            )


ensureActiveHasVisualIndicator : TestContext -> TestContext
ensureActiveHasVisualIndicator testContext =
    testContext
        |> ensureView
            (Query.find
                [ Selector.attribute (Aria.pressed (Just True)) ]
                >> Query.has [ Selector.attribute (Attributes.attribute "data-nri-description" "active-tool") ]
            )


ensureNotActiveDoNotHaveVisualIndicator : TestContext -> TestContext
ensureNotActiveDoNotHaveVisualIndicator testContext =
    testContext
        |> ensureView
            (Query.findAll
                [ Selector.attribute (Aria.pressed (Just False)) ]
                >> Query.each
                    (Query.hasNot
                        [ Selector.attribute (Attributes.attribute "data-nri-description" "active-tool") ]
                    )
            )


type Tag
    = Claim
    | Evidence
    | Reasoning


tags : List Tag
tags =
    [ Claim, Evidence, Reasoning ]


getName : Tag -> String
getName tag =
    case tag of
        Claim ->
            "Claim"

        Evidence ->
            "Evidence"

        Reasoning ->
            "Reasoning"


getColor : Tag -> { colorSolid : Color, colorLight : Color }
getColor tag =
    case tag of
        Claim ->
            { colorSolid = Colors.mustard
            , colorLight = Colors.highlightYellow
            }

        Evidence ->
            { colorSolid = Colors.magenta
            , colorLight = Colors.highlightMagenta
            }

        Reasoning ->
            { colorSolid = Colors.cyan
            , colorLight = Colors.highlightCyan
            }


{-| -}
type alias State =
    { currentTool : Maybe Tag
    }


{-| -}
init : State
init =
    { currentTool = Nothing }


{-| -}
type Msg
    = FocusAndSelectTag { select : Maybe Tag, focus : Maybe String }
    | Focused (Result Dom.Error ())


{-| -}
update : Msg -> State -> State
update msg state =
    case msg of
        FocusAndSelectTag { select, focus } ->
            Tuple.first
                ( { state | currentTool = select }
                , focus
                    |> Maybe.map (Dom.focus >> Task.attempt Focused)
                    |> Maybe.withDefault Cmd.none
                )

        Focused _ ->
            Tuple.first ( state, Cmd.none )


view : State -> Html Msg
view model =
    HighlighterToolbar.view
        { focusAndSelect = FocusAndSelectTag
        , getColor = getColor
        , getName = getName
        , highlighterId = "highlighter"
        }
        { currentTool = model.currentTool
        , tags = tags
        }


type alias TestContext =
    ProgramTest State Msg ()


program : TestContext
program =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = view >> Html.toUnstyled
        }
        |> ProgramTest.start ()
