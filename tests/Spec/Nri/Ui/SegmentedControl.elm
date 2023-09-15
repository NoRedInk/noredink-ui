module Spec.Nri.Ui.SegmentedControl exposing (spec)

import Accessibility.Key as Key
import Accessibility.Role as Role
import Browser.Dom as Dom
import Expect
import Html.Styled as Html exposing (..)
import Nri.Test.KeyboardHelpers.V1 as KeyboardHelpers
import Nri.Ui.SegmentedControl.V14 as SegmentedControl
import ProgramTest exposing (..)
import Task
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.SegmentedControl.V14"
        [ describe "panel rendering" panelRenderingTests
        , describe "keyboard behavior" keyboardTests
        ]


type alias TestContext =
    ProgramTest State Msg ()


panelRenderingTests : List Test
panelRenderingTests =
    [ test "displays the associated panel when a control is activated" <|
        \() ->
            program
                |> ensureTabbable "Control 0"
                |> ensurePanelDisplayed "Page 0"
                |> done
    , test "has only one panel displayed" <|
        \() ->
            program
                |> ensureOnlyOnePanelDisplayed "Page 0" [ "Page 1", "Page 2" ]
                |> done
    ]


keyboardTests : List Test
keyboardTests =
    [ test "has a focusable control" <|
        \() ->
            program
                |> ensureTabbable "Control 0"
                |> done
    , test "all panels are focusable" <|
        \() ->
            program
                |> ensurePanelsFocusable [ "Page 0", "Page 1", "Page 2" ]
                |> done
    , test "has only one control included in the tab sequence" <|
        \() ->
            program
                |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                |> done
    , test "moves focus right on right arrow key" <|
        \() ->
            program
                |> ensureTabbable "Control 0"
                |> releaseRightArrow
                |> ensureTabbable "Control 1"
                |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                |> releaseRightArrow
                |> ensureTabbable "Control 2"
                |> done
    , test "moves focus left on left arrow key" <|
        \() ->
            program
                |> ensureTabbable "Control 0"
                |> releaseRightArrow
                |> ensureTabbable "Control 1"
                |> releaseLeftArrow
                |> ensureTabbable "Control 0"
                |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                |> done
    , test "when the focus is on the first element, move focus to the last element on left arrow key" <|
        \() ->
            program
                |> ensureTabbable "Control 0"
                |> releaseLeftArrow
                |> ensureTabbable "Control 2"
                |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                |> done
    , test "when the focus is on the last element, move focus to the first element on right arrow key" <|
        \() ->
            program
                |> ensureTabbable "Control 0"
                |> releaseLeftArrow
                |> ensureTabbable "Control 2"
                |> releaseRightArrow
                |> ensureTabbable "Control 0"
                |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                |> done
    ]


ensureTabbable : String -> TestContext -> TestContext
ensureTabbable word testContext =
    testContext
        |> ensureView
            (Query.find [ Selector.attribute Role.tab, Selector.attribute (Key.tabbable True) ]
                >> Query.has [ Selector.text word ]
            )


ensurePanelsFocusable : List String -> TestContext -> TestContext
ensurePanelsFocusable words testContext =
    testContext
        |> ensureView
            (Query.findAll [ Selector.attribute Role.tabPanel, Selector.attribute (Key.tabbable True) ]
                >> Expect.all (List.indexedMap (\i w -> Query.index i >> Query.has [ Selector.text w ]) words)
            )


ensurePanelDisplayed : String -> TestContext -> TestContext
ensurePanelDisplayed word testContext =
    testContext
        |> ensureView
            (Query.find [ Selector.attribute Role.tabPanel, Selector.style "display" "block" ]
                >> Query.has [ Selector.text word ]
            )


ensureOnlyOnePanelDisplayed : String -> List String -> TestContext -> TestContext
ensureOnlyOnePanelDisplayed selectedPanel notSelectedPanels testContext =
    testContext
        |> ensureView
            (Query.find [ Selector.attribute Role.tabPanel, Selector.style "display" "block" ]
                >> Query.has [ Selector.text selectedPanel ]
            )
        |> ensureView
            (Query.findAll [ Selector.attribute Role.tabPanel, Selector.style "display" "none" ]
                >> Expect.all (List.indexedMap (\i w -> Query.index i >> Query.has [ Selector.text w ]) notSelectedPanels)
            )


ensureOnlyOneTabInSequence : List String -> TestContext -> TestContext
ensureOnlyOneTabInSequence tabs testContext =
    testContext
        |> ensureView
            (Query.findAll [ Selector.attribute Role.tab, Selector.attribute (Key.tabbable True) ]
                >> Query.count (Expect.equal 1)
            )
        |> ensureView
            (Query.findAll [ Selector.attribute Role.tab, Selector.attribute (Key.tabbable False) ]
                >> Query.count (Expect.equal (List.length tabs - 1))
            )


releaseRightArrow : TestContext -> TestContext
releaseRightArrow =
    KeyboardHelpers.releaseRightArrow { targetDetails = [] }
        [ Selector.attribute Role.tab, Selector.attribute (Key.tabbable True) ]


releaseLeftArrow : TestContext -> TestContext
releaseLeftArrow =
    KeyboardHelpers.releaseLeftArrow { targetDetails = [] }
        [ Selector.attribute Role.tab, Selector.attribute (Key.tabbable True) ]


type Page
    = Page0
    | Page1
    | Page2


type alias State =
    { selected : Page
    }


type Msg
    = FocusAndSelectPage { select : Page, focus : Maybe String }
    | Focused (Result Dom.Error ())


init : State
init =
    { selected = Page0
    }


update : Msg -> State -> State
update msg model =
    case msg of
        FocusAndSelectPage { select, focus } ->
            Tuple.first
                ( { model | selected = select }
                , focus
                    |> Maybe.map (Dom.focus >> Task.attempt Focused)
                    |> Maybe.withDefault Cmd.none
                )

        Focused _ ->
            Tuple.first ( model, Cmd.none )


view : State -> Html Msg
view model =
    SegmentedControl.view
        { focusAndSelect = FocusAndSelectPage
        , options =
            [ { icon = Nothing
              , label = text "Control 0"
              , value = Page0
              , idString = "control-0"
              , attributes = []
              , tabTooltip = []
              , content = text "Page 0"
              }
            , { icon = Nothing
              , label = text "Control 1"
              , value = Page1
              , idString = "control-1"
              , attributes = []
              , tabTooltip = []
              , content = text "Page 1"
              }
            , { icon = Nothing
              , label = text "Control 2"
              , value = Page2
              , idString = "control-2"
              , attributes = []
              , tabTooltip = []
              , content = text "Page 2"
              }
            ]
        , selected = model.selected
        , positioning = SegmentedControl.Left SegmentedControl.FitContent
        , toUrl = Nothing
        }


program : TestContext
program =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = view >> Html.toUnstyled
        }
        |> ProgramTest.start ()
