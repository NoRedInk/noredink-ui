module Spec.Nri.Ui.HighlighterToolbar exposing (..)

import Accessibility.Aria as Aria
import Css exposing (Color)
import Expect
import Html.Attributes as Attributes
import Html.Styled as Html exposing (..)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.HighlighterToolbar.V1 as HighlighterToolbar
import ProgramTest exposing (..)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.HighlighterToolbar.V1"
        [ describe "tool selection" selectionTests
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
    = SetTool (Maybe Tag)


{-| -}
update : Msg -> State -> State
update msg state =
    case msg of
        SetTool tag ->
            { state | currentTool = tag }


view : State -> Html Msg
view model =
    HighlighterToolbar.view
        { onSetEraser = SetTool Nothing
        , onChangeTag = SetTool << Just
        , getColor = getColor
        , getName = getName
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
