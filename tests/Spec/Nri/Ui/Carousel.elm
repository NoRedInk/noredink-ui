module Spec.Nri.Ui.Carousel exposing (spec)

import Browser.Dom as Dom
import Html.Styled exposing (..)
import Nri.Ui.Carousel.V1 as Carousel
import ProgramTest exposing (..)
import Spec.TabsInternalHelpers exposing (..)
import Task
import Test exposing (..)


spec : Test
spec =
    describe "Nri.Ui.Carousel.V1"
        [ describe "panel rendering" panelRenderingTests
        , describe "keyboard behavior" keyboardTests
        ]


panelRenderingTests : List Test
panelRenderingTests =
    [ test "displays the associated slide when a control is activated" <|
        \() ->
            program
                |> ensureTabbable "Control 0"
                |> ensurePanelDisplayed "Slide 0"
                |> done
    , test "has only one slide displayed" <|
        \() ->
            program
                |> ensureOnlyOnePanelDisplayed [ "Slide 0", "Slide 1", "Slide 2" ]
                |> done
    ]


keyboardTests : List Test
keyboardTests =
    [ test "has a focusable control" <|
        \() ->
            program
                |> ensureTabbable "Control 0"
                |> done
    , test "all slides are focusable" <|
        \() ->
            program
                |> ensurePanelsFocusable [ "Slide 0", "Slide 1", "Slide 2" ]
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
    Carousel.view
        { focusAndSelect = FocusAndSelectTab
        , selected = model.selected
        , controlListStyles = []
        , controlStyles =
            \isSelected ->
                []
        , items =
            [ Carousel.buildItem
                { id = 0
                , idString = "slide-0"
                , controlHtml = text "Control 0"
                , slideHtml = text "Slide 0"
                }
            , Carousel.buildItem
                { id = 1
                , idString = "slide-1"
                , controlHtml = text "Control 1"
                , slideHtml = text "Slide 1"
                }
            , Carousel.buildItem
                { id = 2
                , idString = "slide-2"
                , controlHtml = text "Control 2"
                , slideHtml = text "Slide 2"
                }
            ]
        }
        |> (\{ controls, slides } -> section [] [ slides, controls ])


program : TestContext
program =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = view >> toUnstyled
        }
        |> ProgramTest.start ()
