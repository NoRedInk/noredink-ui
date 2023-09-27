module Spec.Nri.Ui.Tabs exposing (spec)

import Browser.Dom as Dom
import Html.Styled as Html exposing (..)
import Nri.Ui.Tabs.V8 as Tabs
import Nri.Ui.Tooltip.V3 as Tooltip
import ProgramTest exposing (..)
import Spec.Helpers exposing (nriDescription)
import Spec.TabsInternalHelpers exposing (..)
import Task
import Test exposing (..)
import Test.Html.Selector as Selector exposing (Selector, all, containing)


spec : Test
spec =
    describe "Nri.Ui.Tabs.V8"
        [ describe "panel rendering" panelRenderingTests
        , describe "keyboard behavior" keyboardTests
        ]


panelRenderingTests : List Test
panelRenderingTests =
    [ test "displays the associated panel when a tab is activated" <|
        \() ->
            program (\_ -> [])
                |> ensureTabbable "Tab 0"
                |> ensurePanelDisplayed "Panel 0"
                |> done
    , test "has only one panel displayed" <|
        \() ->
            program (\_ -> [])
                |> ensureOnlyOnePanelDisplayed [ "Panel 0", "Panel 1", "Panel 2" ]
                |> done
    , describe "identifying tabs container" <|
        let
            ensureTabContainerHas : List Selector -> TestContext -> TestContext
            ensureTabContainerHas attrs =
                ensureViewHas
                    [ all
                        [ all attrs
                        , containing [ Selector.text "Tab 0" ]
                        , containing [ Selector.text "Tab 1" ]
                        , containing [ Selector.text "Tab 2" ]
                        ]
                    ]
                    >> ensureViewHasNot
                        [ all
                            [ all attrs
                            , containing [ Selector.text "Panel 0" ]
                            , containing [ Selector.text "Panel 1" ]
                            , containing [ Selector.text "Panel 2" ]
                            ]
                        ]
        in
        [ test "uses an attribute to identify the tabs container (without tooltips)" <|
            \() ->
                program (\_ -> [])
                    |> ensureTabContainerHas [ nriDescription "Nri-Ui__tabs" ]
                    |> done
        , test "uses an attribute to identify the tabs container (with tooltips)" <|
            \() ->
                program (\tabId -> [ Tabs.withTooltip [ Tooltip.plaintext (String.fromInt tabId) ] ])
                    |> ensureTabContainerHas [ nriDescription "Nri-Ui__tabs" ]
                    |> done
        ]
    ]


keyboardTests : List Test
keyboardTests =
    [ test "has a focusable tab" <|
        \() ->
            program (\_ -> [])
                |> ensureTabbable "Tab 0"
                |> done
    , test "all panels are focusable" <|
        \() ->
            program (\_ -> [])
                |> ensurePanelsFocusable [ "Panel 0", "Panel 1", "Panel 2" ]
                |> done
    , test "has only one tab included in the tab sequence" <|
        \() ->
            program (\_ -> [])
                |> ensureOnlyOneTabInSequence [ "Tab 0", "Tab 1", "Tab 2" ]
                |> done
    , test "moves focus right on right arrow key" <|
        \() ->
            program (\_ -> [])
                |> ensureTabbable "Tab 0"
                |> releaseRightArrow
                |> ensureTabbable "Tab 1"
                |> ensureOnlyOneTabInSequence [ "Tab 0", "Tab 1", "Tab 2" ]
                |> releaseRightArrow
                |> ensureTabbable "Tab 2"
                |> done
    , test "moves focus left on left arrow key" <|
        \() ->
            program (\_ -> [])
                |> ensureTabbable "Tab 0"
                |> releaseRightArrow
                |> ensureTabbable "Tab 1"
                |> releaseLeftArrow
                |> ensureTabbable "Tab 0"
                |> ensureOnlyOneTabInSequence [ "Tab 0", "Tab 1", "Tab 2" ]
                |> done
    , test "when the focus is on the first element, move focus to the last element on left arrow key" <|
        \() ->
            program (\_ -> [])
                |> ensureTabbable "Tab 0"
                |> releaseLeftArrow
                |> ensureTabbable "Tab 2"
                |> ensureOnlyOneTabInSequence [ "Tab 0", "Tab 1", "Tab 2" ]
                |> done
    , test "when the focus is on the last element, move focus to the first element on right arrow key" <|
        \() ->
            program (\_ -> [])
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

        Focused _ ->
            Tuple.first ( model, Cmd.none )


view : (Int -> List (Tabs.TabAttribute Int Msg)) -> State -> Html Msg
view tabAttributes model =
    Tabs.view
        { focusAndSelect = FocusAndSelectTab
        , selected = model.selected
        }
        []
        [ buildTestTab 0 tabAttributes
        , buildTestTab 1 tabAttributes
        , buildTestTab 2 tabAttributes
        ]


buildTestTab : Int -> (Int -> List (Tabs.TabAttribute Int msg)) -> Tabs.Tab Int msg
buildTestTab id tabAttributes =
    Tabs.build { id = id, idString = "tab-" ++ String.fromInt id }
        (List.append
            [ Tabs.tabString ("Tab " ++ String.fromInt id)
            , Tabs.panelHtml (text ("Panel " ++ String.fromInt id))
            ]
            (tabAttributes id)
        )


type alias TestContext =
    ProgramTest State Msg ()


program : (Int -> List (Tabs.TabAttribute Int Msg)) -> TestContext
program tabAttributes =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = view tabAttributes >> Html.toUnstyled
        }
        |> ProgramTest.start ()
