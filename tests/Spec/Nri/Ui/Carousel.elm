module Spec.Nri.Ui.Carousel exposing
    ( viewWithCombinedControlsSpec
    , viewWithPreviousAndNextControlsSpec
    , viewWithTabControlsSpec
    )

import Expect
import Html.Styled exposing (..)
import Nri.Ui.Carousel.V2 as Carousel
import Nri.Ui.UiIcon.V1 as UiIcon
import ProgramTest exposing (..)
import Spec.TabsInternalHelpers
    exposing
        ( ensureOnlyOnePanelDisplayed
        , ensureOnlyOneTabInSequence
        , ensurePanelDisplayed
        , ensurePanelsFocusable
        , ensureTabbable
        , releaseLeftArrow
        , releaseRightArrow
        )
import Test exposing (..)
import Test.Html.Selector as Selector


type alias Model =
    { selected : Int
    }


type Msg
    = AnnounceAndSelect { select : Int, announce : String }
    | FocusAndSelect { select : Int, focus : Maybe String }


type Effect
    = NoEffect
    | Announce String
    | Focus String


init : flags -> ( Model, Effect )
init _ =
    ( { selected = 0 }, NoEffect )


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        AnnounceAndSelect { select, announce } ->
            ( { model | selected = select }
            , Announce announce
            )

        FocusAndSelect { select, focus } ->
            ( { model | selected = select }
            , case focus of
                Nothing ->
                    NoEffect

                Just id ->
                    Focus id
            )


viewWithPreviousAndNextControlsSpec : Test
viewWithPreviousAndNextControlsSpec =
    let
        start slideCount =
            ProgramTest.createElement
                { init = init
                , update = update
                , view =
                    \model ->
                        Carousel.viewWithPreviousAndNextControls
                            { announceAndSelect = AnnounceAndSelect
                            , selected = model.selected
                            , role = Carousel.Group
                            , accessibleLabel = "Previous/Next Carousel"
                            , visibleLabelId = Nothing
                            , slides =
                                List.map
                                    (\i ->
                                        { id = i
                                        , idString = "slide-" ++ String.fromInt i
                                        , accessibleLabel = "Control " ++ String.fromInt i
                                        , visibleLabelId = Nothing
                                        , slideHtml = text ("Slide " ++ String.fromInt i)
                                        }
                                    )
                                    (List.range 0 (slideCount - 1))
                            , previousButton = { attributes = [], icon = UiIcon.arrowLeft, name = "Previous" }
                            , nextButton = { attributes = [], icon = UiIcon.arrowRight, name = "Next" }
                            }
                            |> (\{ viewPreviousButton, viewNextButton, slides, containerAttributes } ->
                                    section containerAttributes [ slides, viewPreviousButton, viewNextButton ]
                               )
                            |> toUnstyled
                }
                |> ProgramTest.start ()
    in
    describe "viewWithPreviousAndNextControls"
        [ test "rotate back and forward with 3 slides" <|
            \() ->
                start 3
                    |> ensureSlideIsVisible "slide-0"
                    |> clickButton "Next"
                    |> ensureSlideIsVisible "slide-1"
                    |> clickButton "Next"
                    |> ensureSlideIsVisible "slide-2"
                    |> clickButton "Next"
                    |> ensureSlideIsVisible "slide-0"
                    |> clickButton "Previous"
                    |> ensureSlideIsVisible "slide-2"
                    |> clickButton "Previous"
                    |> ensureSlideIsVisible "slide-1"
                    |> clickButton "Previous"
                    |> ensureSlideIsVisible "slide-0"
                    |> done
        , test "rotate back and forward with 1 slides" <|
            \() ->
                start 1
                    |> ensureSlideIsVisible "slide-0"
                    |> clickButton "Next"
                    |> ensureSlideIsVisible "slide-0"
                    |> clickButton "Previous"
                    |> ensureSlideIsVisible "slide-0"
                    |> done
        , test "Announces card changes for screen reader users" <|
            \_ ->
                start 3
                    |> ensureSlideIsVisible "slide-0"
                    |> clickButton "Next"
                    |> ensureLastEffect (Expect.equal (Announce "Active slide of Previous/Next Carousel changed to Control 1"))
                    |> clickButton "Next"
                    |> ensureLastEffect (Expect.equal (Announce "Active slide of Previous/Next Carousel changed to Control 2"))
                    |> clickButton "Previous"
                    |> ensureLastEffect (Expect.equal (Announce "Active slide of Previous/Next Carousel changed to Control 1"))
                    |> done
        ]


viewWithTabControlsSpec : Test
viewWithTabControlsSpec =
    let
        start =
            ProgramTest.createElement
                { init = init
                , update = update
                , view =
                    \model ->
                        Carousel.viewWithTabControls
                            { selected = model.selected
                            , slides =
                                [ { id = 0
                                  , idString = "slide-0"
                                  , accessibleLabel = "Slide 0"
                                  , visibleLabelId = Nothing
                                  , tabControlHtml = text "Control 0"
                                  , slideHtml = text "Slide 0"
                                  }
                                , { id = 1
                                  , idString = "slide-1"
                                  , accessibleLabel = "Slide 1"
                                  , visibleLabelId = Nothing
                                  , tabControlHtml = text "Control 1"
                                  , slideHtml = text "Slide 1"
                                  }
                                , { id = 2
                                  , idString = "slide-2"
                                  , accessibleLabel = "Slide 2"
                                  , visibleLabelId = Nothing
                                  , tabControlHtml = text "Control 2"
                                  , slideHtml = text "Slide 2"
                                  }
                                ]
                            , tabControlStyles = \_ -> []
                            , tabControlListStyles = []
                            , role = Carousel.Group
                            , accessibleLabel = "Slides"
                            , visibleLabelId = Nothing
                            , focusAndSelect = FocusAndSelect
                            , announceAndSelect = AnnounceAndSelect
                            }
                            |> (\{ controls, slides, containerAttributes } -> section containerAttributes [ slides, controls ])
                            |> toUnstyled
                }
                |> ProgramTest.start ()
    in
    describe "viewWithTabControls"
        [ describe "rendering"
            [ test "displays the associated slide when a control is activated" <|
                \() ->
                    start
                        |> ensureTabbable "Control 0"
                        |> ensurePanelDisplayed "Slide 0"
                        |> done
            , test "has only one slide displayed" <|
                \() ->
                    start
                        |> ensureOnlyOnePanelDisplayed [ "Slide 0", "Slide 1", "Slide 2" ]
                        |> done
            ]
        , describe "keyboard behavior"
            [ test "has a focusable control" <|
                \() ->
                    start
                        |> ensureTabbable "Control 0"
                        |> done
            , test "all slides are focusable" <|
                \() ->
                    start
                        |> ensurePanelsFocusable [ "Slide 0", "Slide 1", "Slide 2" ]
                        |> done
            , test "has only one control included in the tab sequence" <|
                \() ->
                    start
                        |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                        |> done
            , test "moves focus right on right arrow key" <|
                \() ->
                    start
                        |> ensureTabbable "Control 0"
                        |> releaseRightArrow
                        |> ensureTabbable "Control 1"
                        |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                        |> releaseRightArrow
                        |> ensureTabbable "Control 2"
                        |> done
            , test "moves focus left on left arrow key" <|
                \() ->
                    start
                        |> ensureTabbable "Control 0"
                        |> releaseRightArrow
                        |> ensureTabbable "Control 1"
                        |> releaseLeftArrow
                        |> ensureTabbable "Control 0"
                        |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                        |> done
            , test "when the focus is on the first element, move focus to the last element on left arrow key" <|
                \() ->
                    start
                        |> ensureTabbable "Control 0"
                        |> releaseLeftArrow
                        |> ensureTabbable "Control 2"
                        |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                        |> done
            , test "when the focus is on the last element, move focus to the first element on right arrow key" <|
                \() ->
                    start
                        |> ensureTabbable "Control 0"
                        |> releaseLeftArrow
                        |> ensureTabbable "Control 2"
                        |> releaseRightArrow
                        |> ensureTabbable "Control 0"
                        |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                        |> done
            ]
        ]


viewWithCombinedControlsSpec : Test
viewWithCombinedControlsSpec =
    let
        start =
            ProgramTest.createElement
                { init = init
                , update = update
                , view =
                    \model ->
                        Carousel.viewWithCombinedControls
                            { selected = model.selected
                            , slides =
                                [ { id = 0
                                  , idString = "slide-0"
                                  , accessibleLabel = "Slide 0"
                                  , visibleLabelId = Nothing
                                  , tabControlHtml = text "Control 0"
                                  , slideHtml = text "Slide 0"
                                  }
                                , { id = 1
                                  , idString = "slide-1"
                                  , accessibleLabel = "Slide 1"
                                  , visibleLabelId = Nothing
                                  , tabControlHtml = text "Control 1"
                                  , slideHtml = text "Slide 1"
                                  }
                                , { id = 2
                                  , idString = "slide-2"
                                  , accessibleLabel = "Slide 2"
                                  , visibleLabelId = Nothing
                                  , tabControlHtml = text "Control 2"
                                  , slideHtml = text "Slide 2"
                                  }
                                ]
                            , role = Carousel.Group
                            , tabControlStyles = \_ -> []
                            , tabControlListStyles = []
                            , accessibleLabel = "Slides"
                            , visibleLabelId = Nothing
                            , previousButton = { attributes = [], icon = UiIcon.arrowLeft, name = "Previous" }
                            , nextButton = { attributes = [], icon = UiIcon.arrowRight, name = "Next" }
                            , focusAndSelect = FocusAndSelect
                            , announceAndSelect = AnnounceAndSelect
                            }
                            |> (\{ tabControls, slides, containerAttributes, viewNextButton, viewPreviousButton } ->
                                    section containerAttributes [ slides, tabControls, viewNextButton, viewPreviousButton ]
                               )
                            |> toUnstyled
                }
                |> ProgramTest.start ()
    in
    describe "viewWithCombinedControls"
        [ test "rotate back and forward with 3 slides" <|
            \_ ->
                start
                    |> ensurePanelDisplayed "Slide 0"
                    |> clickButton "Next"
                    |> ensurePanelDisplayed "Slide 1"
                    |> clickButton "Next"
                    |> ensurePanelDisplayed "Slide 2"
                    |> clickButton "Next"
                    |> ensurePanelDisplayed "Slide 0"
                    |> clickButton "Previous"
                    |> ensurePanelDisplayed "Slide 2"
                    |> clickButton "Previous"
                    |> ensurePanelDisplayed "Slide 1"
                    |> clickButton "Previous"
                    |> ensurePanelDisplayed "Slide 0"
                    |> done
        ]


ensureSlideIsVisible : String -> ProgramTest.ProgramTest model msg effect -> ProgramTest.ProgramTest model msg effect
ensureSlideIsVisible id =
    ensureViewHas [ Selector.id id, Selector.style "display" "block" ]
