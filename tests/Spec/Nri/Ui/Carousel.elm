module Spec.Nri.Ui.Carousel exposing (spec)

import Browser.Dom as Dom
import Html.Styled exposing (..)
import Nri.Ui.Carousel.V2 as Carousel
import ProgramTest exposing (..)
import Spec.TabsInternalHelpers exposing (..)
import Task
import Test exposing (..)


spec : Test
spec =
    describe "Nri.Ui.Carousel.V2"
        [ describe "viewWithTabControls rendering" panelRenderingTests
        , describe "keyboard behavior on viewWithTabControls" keyboardTests
        ]


panelRenderingTests : List Test
panelRenderingTests =
    [ test "displays the associated slide when a control is activated" <|
        \() ->
            program viewWithTabControls
                |> ensureTabbable "Control 0"
                |> ensurePanelDisplayed "Slide 0"
                |> done
    , test "has only one slide displayed" <|
        \() ->
            program viewWithTabControls
                |> ensureOnlyOnePanelDisplayed [ "Slide 0", "Slide 1", "Slide 2" ]
                |> done
    ]


keyboardTests : List Test
keyboardTests =
    [ test "has a focusable control" <|
        \() ->
            program viewWithTabControls
                |> ensureTabbable "Control 0"
                |> done
    , test "all slides are focusable" <|
        \() ->
            program viewWithTabControls
                |> ensurePanelsFocusable [ "Slide 0", "Slide 1", "Slide 2" ]
                |> done
    , test "has only one control included in the tab sequence" <|
        \() ->
            program viewWithTabControls
                |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                |> done
    , test "moves focus right on right arrow key" <|
        \() ->
            program viewWithTabControls
                |> ensureTabbable "Control 0"
                |> releaseRightArrow
                |> ensureTabbable "Control 1"
                |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                |> releaseRightArrow
                |> ensureTabbable "Control 2"
                |> done
    , test "moves focus left on left arrow key" <|
        \() ->
            program viewWithTabControls
                |> ensureTabbable "Control 0"
                |> releaseRightArrow
                |> ensureTabbable "Control 1"
                |> releaseLeftArrow
                |> ensureTabbable "Control 0"
                |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                |> done
    , test "when the focus is on the first element, move focus to the last element on left arrow key" <|
        \() ->
            program viewWithTabControls
                |> ensureTabbable "Control 0"
                |> releaseLeftArrow
                |> ensureTabbable "Control 2"
                |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                |> done
    , test "when the focus is on the last element, move focus to the first element on right arrow key" <|
        \() ->
            program viewWithTabControls
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

        Focused _ ->
            Tuple.first ( model, Cmd.none )


viewWithTabControls : State -> Html Msg
viewWithTabControls model =
    Carousel.viewWithTabControls
        { focusAndSelect = FocusAndSelectTab
        , selected = model.selected
        , tabControlListStyles = []
        , role = Carousel.Group
        , tabControlStyles = \_ -> []
        , labelledBy = Carousel.LabelledByAccessibleLabelOnly "Label"
        , panels =
            [ { id = 0
              , idString = "slide-0"
              , tabControlHtml = text "Control 0"
              , slideHtml = text "Slide 0"
              }
            , { id = 1
              , idString = "slide-1"
              , tabControlHtml = text "Control 1"
              , slideHtml = text "Slide 1"
              }
            , { id = 2
              , idString = "slide-2"
              , tabControlHtml = text "Control 2"
              , slideHtml = text "Slide 2"
              }
            ]
        }
        |> (\{ controls, slides } -> section [] [ slides, controls ])


program : (State -> Html Msg) -> TestContext
program view =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = view >> toUnstyled
        }
        |> ProgramTest.start ()
