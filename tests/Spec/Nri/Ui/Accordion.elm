module Spec.Nri.Ui.Accordion exposing (spec)

import Accessibility.Aria as Aria
import Browser.Dom as Dom
import Html.Styled as Html exposing (..)
import Nri.Ui.Accordion.V4 as Accordion
import ProgramTest exposing (..)
import Set exposing (Set)
import Task
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.Accordion.V4"
        [ describe "panel rendering" panelRenderingTests
        , describe "aria attributes" ariaAttributesTests
        ]


type alias TestContext =
    ProgramTest State Msg ()


panelRenderingTests : List Test
panelRenderingTests =
    [ test "displays the associated panel only when an accordion header is activated" <|
        \() ->
            program
                |> ensurePanelNotDisplayed "header-1" "Content 1"
                |> clickButton "Header 1"
                |> ensurePanelDisplayed "header-1" "Content 1"
                |> ensurePanelNotDisplayed "header-2" "Content 2"
                |> clickButton "Header 2"
                |> ensurePanelDisplayed "header-2" "Content 2"
                |> done
    ]


ariaAttributesTests : List Test
ariaAttributesTests =
    [ test "header has aria-expanded true when the associated panel is expanded and false when the associated panel is collapsed" <|
        \() ->
            program
                |> ensureCorrectAriaExpanded "header-1" False
                |> clickButton "Header 1"
                |> ensureCorrectAriaExpanded "header-1" True
                |> ensureCorrectAriaExpanded "header-2" False
                |> clickButton "Header 2"
                |> ensureCorrectAriaExpanded "header-2" True
                |> done
    , test "header has aria-controls set to the id of the associated panel" <|
        \() ->
            program
                |> ensureCorrectAriaControls "header-1"
                |> ensureCorrectAriaControls "header-2"
                |> done
    , test "panel has aria-labelledby set to the id of the associated header" <|
        \() ->
            program
                |> ensureCorrectAriaLabelledBy "header-1"
                |> ensureCorrectAriaLabelledBy "header-2"
                |> done
    ]


ensureCorrectAriaExpanded : String -> Bool -> TestContext -> TestContext
ensureCorrectAriaExpanded id expanded testContext =
    testContext
        |> ensureView
            (Query.find [ Selector.id id ]
                >> Query.has [ Selector.attribute (Aria.expanded expanded) ]
            )


ensureCorrectAriaControls : String -> TestContext -> TestContext
ensureCorrectAriaControls id testContext =
    testContext
        |> ensureView
            (Query.find [ Selector.id id ]
                >> Query.has [ Selector.attribute (Aria.controls [ "accordion-panel__" ++ id ]) ]
            )


ensureCorrectAriaLabelledBy : String -> TestContext -> TestContext
ensureCorrectAriaLabelledBy id testContext =
    testContext
        |> ensureView
            (Query.find [ Selector.id ("accordion-panel__" ++ id) ]
                >> Query.has [ Selector.attribute (Aria.labelledBy id) ]
            )


ensurePanelDisplayed : String -> String -> TestContext -> TestContext
ensurePanelDisplayed id word testContext =
    testContext
        |> ensureView
            (Query.find [ Selector.attribute (Aria.labeledBy id) ]
                >> Query.has [ Selector.text word ]
            )


ensurePanelNotDisplayed : String -> String -> TestContext -> TestContext
ensurePanelNotDisplayed id word testContext =
    testContext
        |> ensureView
            (Query.find [ Selector.attribute (Aria.labeledBy id) ]
                >> Query.hasNot [ Selector.text word ]
            )


type alias State =
    { expanded : Set Int
    }


type Msg
    = Toggle Int Bool
    | Focus String
    | Focused (Result Dom.Error ())


init : State
init =
    { expanded = Set.empty
    }


update : Msg -> State -> State
update msg model =
    case msg of
        Toggle id expand ->
            Tuple.first
                ( if expand then
                    { model | expanded = Set.insert id model.expanded }

                  else
                    { model | expanded = Set.remove id model.expanded }
                , Cmd.none
                )

        Focus id ->
            Tuple.first ( model, Task.attempt Focused (Dom.focus id) )

        Focused _ ->
            Tuple.first ( model, Cmd.none )


view : State -> Html Msg
view model =
    div []
        [ Accordion.view
            { entries =
                [ Accordion.AccordionEntry
                    { caret = Accordion.defaultCaret
                    , content = \() -> text "Content 1"
                    , entryClass = ""
                    , expansionDirection = Accordion.Downwards
                    , headerContent = text "Header 1"
                    , headerId = "header-1"
                    , headerLevel = Accordion.H4
                    , isExpanded = Set.member 1 model.expanded
                    , toggle = Just (Toggle 1)
                    }
                    []
                , Accordion.AccordionEntry
                    { caret = Accordion.upwardCaret
                    , content = \() -> text "Content 2"
                    , entryClass = ""
                    , expansionDirection = Accordion.Upwards
                    , headerContent = text "Header 2"
                    , headerId = "header-2"
                    , headerLevel = Accordion.H4
                    , isExpanded = Set.member 2 model.expanded
                    , toggle = Just (Toggle 2)
                    }
                    []
                ]
            , focus = Focus
            }
        ]


program : TestContext
program =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = view >> Html.toUnstyled
        }
        |> ProgramTest.start ()
