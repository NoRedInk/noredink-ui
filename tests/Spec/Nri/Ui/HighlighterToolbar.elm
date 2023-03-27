module Spec.Nri.Ui.HighlighterToolbar exposing (..)

import Accessibility.Key as Key
import Css exposing (Color)
import Expect
import Html.Attributes as Attributes
import Html.Styled as Html exposing (..)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.HighlighterToolbar.V3 as HighlighterToolbar
import ProgramTest exposing (..)
import Spec.KeyboardHelpers as KeyboardHelpers
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)


spec : Test
spec =
    describe "Nri.Ui.HighlighterToolbar.V3"
        [ test "sets selection status on the active tool" <|
            \() ->
                program
                    |> clickTool "Claim"
                    |> ensureActiveToolIs "Claim"
                    |> clickTool "Evidence"
                    |> ensureActiveToolIs "Evidence"
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


byLabel : String -> List Selector
byLabel label =
    [ Selector.tag "label"
    , Selector.containing [ Selector.text label ]
    ]


clickTool : String -> ProgramTest model msg effect -> ProgramTest model msg effect
clickTool label =
    ProgramTest.within (Query.find (byLabel label))
        (ProgramTest.simulateDomEvent
            (Query.find [ Selector.tag "input" ])
            Event.click
        )


ensureActiveToolIs : String -> ProgramTest model msg effect -> ProgramTest model msg effect
ensureActiveToolIs label testContext =
    testContext
        |> ProgramTest.within (Query.find (byLabel label))
            -- has the attribute showing the radio as checked
            (ProgramTest.ensureViewHas [ Selector.attribute (Attributes.checked True) ]
                -- has a visual indicator of selection
                >> ProgramTest.ensureViewHas activeToolVisualIndicator
            )
        |> ProgramTest.ensureView
            -- has only 1 checked radio
            (Query.findAll [ Selector.attribute (Attributes.checked True) ]
                >> Query.count (Expect.equal 1)
            )
        |> ProgramTest.ensureView
            -- has only 1 visual indicator of selection
            (Query.findAll activeToolVisualIndicator
                >> Query.count (Expect.equal 1)
            )


activeToolVisualIndicator : List Selector
activeToolVisualIndicator =
    [ Selector.attribute (Attributes.attribute "data-nri-description" "active-tool") ]


ensureActiveHasAriaPressedTrue : String -> TestContext -> TestContext
ensureActiveHasAriaPressedTrue label testContext =
    testContext
        |> ensureView
            (Query.find [ Selector.attribute (Attributes.checked True) ]
                >> Query.has [ Selector.text label ]
            )


ensureNotActiveHaveAriaPressedFalse : List String -> TestContext -> TestContext
ensureNotActiveHaveAriaPressedFalse labels testContext =
    testContext
        |> ensureView
            (Query.findAll [ Selector.attribute (Attributes.checked False) ]
                >> Expect.all (List.indexedMap (\i label -> Query.index i >> Query.has [ Selector.text label ]) labels)
            )


ensureActiveHasVisualIndicator : TestContext -> TestContext
ensureActiveHasVisualIndicator testContext =
    testContext
        |> ensureView
            (Query.find
                [ Selector.attribute (Attributes.checked True) ]
                >> Query.has [ Selector.attribute (Attributes.attribute "data-nri-description" "active-tool") ]
            )


ensureNotActiveDoNotHaveVisualIndicator : TestContext -> TestContext
ensureNotActiveDoNotHaveVisualIndicator testContext =
    testContext
        |> ensureView
            (Query.findAll
                [ Selector.attribute (Attributes.checked False) ]
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
    = FocusAndSelectTag (Maybe Tag)


{-| -}
update : Msg -> State -> State
update msg state =
    case msg of
        FocusAndSelectTag select ->
            { state | currentTool = select }


view : State -> Html Msg
view model =
    HighlighterToolbar.view
        { onSelect = FocusAndSelectTag
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
