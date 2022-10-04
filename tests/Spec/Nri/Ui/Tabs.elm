module Spec.Nri.Ui.Tabs exposing (spec)

import Accessibility.Key as Key
import Accessibility.Role as Role
import Browser.Dom as Dom
import Expect
import Html.Styled as Html exposing (..)
import Nri.Ui.Tabs.V7 as Tabs
import ProgramTest exposing (..)
import Spec.KeyboardHelpers as KeyboardHelpers
import Task
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.Tabs.V7"
        [ describe "panel rendering" panelRenderingTests
        , describe "keyboard behavior" keyboardTests
        ]


panelRenderingTests : List Test
panelRenderingTests =
    [ test "displays the associated panel when a tab is activated" <|
        \() ->
            program
                |> ensureTabbable "Tab 0"
                |> ensurePanelDisplayed "Panel 0"
                |> done
    , test "has only one panel displayed" <|
        \() ->
            program
                |> ensureOnlyOnePanelDisplayed [ "Panel 0", "Panel 1", "Panel 2" ]
                |> done
    ]


keyboardTests : List Test
keyboardTests =
    [ test "has a focusable tab" <|
        \() ->
            program
                |> ensureTabbable "Tab 0"
                |> done
    , test "all panels are focusable" <|
        \() ->
            program
                |> ensurePanelsFocusable [ "Panel 0", "Panel 1", "Panel 2" ]
                |> done
    , test "has only one tab included in the tab sequence" <|
        \() ->
            program
                |> ensureOnlyOneTabInSequence [ "Tab 0", "Tab 1", "Tab 2" ]
                |> done
    , test "moves focus right on right arrow key" <|
        \() ->
            program
                |> ensureTabbable "Tab 0"
                |> releaseRightArrow
                |> ensureTabbable "Tab 1"
                |> ensureOnlyOneTabInSequence [ "Tab 0", "Tab 1", "Tab 2" ]
                |> releaseRightArrow
                |> ensureTabbable "Tab 2"
                |> done
    , test "moves focus left on left arrow key" <|
        \() ->
            program
                |> ensureTabbable "Tab 0"
                |> releaseRightArrow
                |> ensureTabbable "Tab 1"
                |> releaseLeftArrow
                |> ensureTabbable "Tab 0"
                |> ensureOnlyOneTabInSequence [ "Tab 0", "Tab 1", "Tab 2" ]
                |> done
    , test "when the focus is on the first element, move focus to the last element on left arrow key" <|
        \() ->
            program
                |> ensureTabbable "Tab 0"
                |> releaseLeftArrow
                |> ensureTabbable "Tab 2"
                |> ensureOnlyOneTabInSequence [ "Tab 0", "Tab 1", "Tab 2" ]
                |> done
    , test "when the focus is on the last element, move focus to the first element on right arrow key" <|
        \() ->
            program
                |> ensureTabbable "Tab 0"
                |> releaseLeftArrow
                |> ensureTabbable "Tab 2"
                |> releaseRightArrow
                |> ensureTabbable "Tab 0"
                |> ensureOnlyOneTabInSequence [ "Tab 0", "Tab 1", "Tab 2" ]
                |> done
    ]


type alias TestContext =
    ProgramTest State Msg ()


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


ensureOnlyOnePanelDisplayed : List String -> TestContext -> TestContext
ensureOnlyOnePanelDisplayed panels testContext =
    testContext
        |> ensureView
            (Query.findAll [ Selector.attribute Role.tabPanel, Selector.style "display" "block" ]
                >> Query.count (Expect.equal 1)
            )
        |> ensureView
            (Query.findAll [ Selector.attribute Role.tabPanel, Selector.style "display" "none" ]
                >> Query.count (Expect.equal (List.length panels - 1))
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


type alias State =
    { selected : Int
    }


init : State
init =
    { selected = 0
    }


type Msg
    = FocusAndSelectTab { select : Int, focus : Maybe String }
    | Focused (Result Dom.Error ())


update : Msg -> State -> State
update msg model =
    case msg of
        FocusAndSelectTab { select, focus } ->
            Tuple.first
                ( { model | selected = select }
                , focus
                    |> Maybe.map (Dom.focus >> Task.attempt Focused)
                    |> Maybe.withDefault Cmd.none
                )

        Focused error ->
            Tuple.first ( model, Cmd.none )


view model =
    Tabs.view
        { title = Nothing
        , alignment = Tabs.Left
        , customSpacing = Nothing
        , focusAndSelect = FocusAndSelectTab
        , selected = model.selected
        , tabs =
            [ Tabs.build { id = 0, idString = "tab-0" } [ Tabs.tabString "Tab 0", Tabs.panelHtml (text "Panel 0") ]
            , Tabs.build { id = 1, idString = "tab-1" } [ Tabs.tabString "Tab 1", Tabs.panelHtml (text "Panel 1") ]
            , Tabs.build { id = 2, idString = "tab-2" } [ Tabs.tabString "Tab 2", Tabs.panelHtml (text "Panel 2") ]
            ]
        }


program : TestContext
program =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = view >> Html.toUnstyled
        }
        |> ProgramTest.start ()
