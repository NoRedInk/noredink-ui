module Spec.Nri.Ui.TabsMinimal exposing (spec)

import Browser.Dom as Dom
import Html.Styled as Html exposing (..)
import Nri.Ui.TabsMinimal.V1 as TabsMinimal
import ProgramTest exposing (..)
import Spec.TabsInternalHelpers exposing (..)
import Task
import Test exposing (..)


spec : Test
spec =
    describe "Nri.Ui.TabsMinimal.V1"
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


view : State -> Html Msg
view model =
    TabsMinimal.view
        { focusAndSelect = FocusAndSelectTab
        , selected = model.selected
        }
        [ TabsMinimal.build { id = 0, idString = "tab-0" } [ TabsMinimal.tabString "Tab 0", TabsMinimal.panelHtml (text "Panel 0") ]
        , TabsMinimal.build { id = 1, idString = "tab-1" } [ TabsMinimal.tabString "Tab 1", TabsMinimal.panelHtml (text "Panel 1") ]
        , TabsMinimal.build { id = 2, idString = "tab-2" } [ TabsMinimal.tabString "Tab 2", TabsMinimal.panelHtml (text "Panel 2") ]
        ]


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
