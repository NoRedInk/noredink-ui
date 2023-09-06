module Spec.Nri.Ui.Carousel exposing
    ( viewWithCombinedControlsSpec
    , viewWithPreviousAndNextControlsSpec
    , viewWithTabControlsSpec
    )

import Accessibility.Aria as Aria
import Accessibility.Role as Role
import Expect
import Html.Styled exposing (..)
import Html.Styled.Attributes as StyledAttrs
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
    = Select
        { select : Int
        , announce : Maybe String
        , focus : Maybe String
        }


type Effect
    = Announce String
    | Focus String


init : flags -> ( Model, List Effect )
init _ =
    ( { selected = 0 }, [] )


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        Select { select, announce, focus } ->
            ( { model | selected = select }
            , List.filterMap identity
                [ Maybe.map Announce announce
                , Maybe.map Focus focus
                ]
            )


type LabelWith
    = AccessibleLabel
    | VisibleLabel


viewWithPreviousAndNextControlsSpec : Test
viewWithPreviousAndNextControlsSpec =
    let
        createSlide labelWith index =
            { id = index
            , idString = "slide-" ++ String.fromInt index
            , name = "Slide " ++ String.fromInt index
            , visibleLabelId =
                case labelWith of
                    AccessibleLabel ->
                        Nothing

                    VisibleLabel ->
                        Just ("slide-" ++ String.fromInt index ++ "-label")
            , slideView =
                div [ StyledAttrs.id ("slide-" ++ String.fromInt index ++ "-label") ]
                    [ text ("Slide " ++ String.fromInt index) ]
            }

        start { containerLabel, slideLabels, slideCount } =
            ProgramTest.createElement
                { init = init
                , update = update
                , view =
                    \model ->
                        Carousel.viewWithPreviousAndNextControls
                            { announceAndSelect =
                                \{ select, announce } ->
                                    Select
                                        { select = select
                                        , announce = Just announce
                                        , focus = Nothing
                                        }
                            , selected = model.selected
                            , role = Carousel.Group
                            , name = "Slides"
                            , visibleLabelId =
                                case containerLabel of
                                    AccessibleLabel ->
                                        Nothing

                                    VisibleLabel ->
                                        Just "carousel-label"
                            , slides = List.map (createSlide slideLabels) (List.range 0 (slideCount - 1))
                            , previousButton = { attributes = [], icon = UiIcon.arrowLeft, name = "Previous" }
                            , nextButton = { attributes = [], icon = UiIcon.arrowRight, name = "Next" }
                            }
                            |> (\{ viewPreviousButton, viewNextButton, slides, containerAttributes } ->
                                    section containerAttributes
                                        [ span [ StyledAttrs.id "carousel-label" ] [ text "Slides" ]
                                        , slides
                                        , viewPreviousButton
                                        , viewNextButton
                                        ]
                               )
                            |> toUnstyled
                }
                |> ProgramTest.start ()
    in
    describe "viewWithPreviousAndNextControls"
        [ test "rotate back and forward with 3 slides" <|
            \() ->
                start
                    { slideCount = 3
                    , slideLabels = AccessibleLabel
                    , containerLabel = AccessibleLabel
                    }
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
        , test "does not crash with 0 slides" <|
            \() ->
                start
                    { slideCount = 0
                    , slideLabels = AccessibleLabel
                    , containerLabel = AccessibleLabel
                    }
                    |> ensureViewHas [ Selector.text "Slides" ]
                    |> done
        , test "rotate back and forward with 1 slides" <|
            \() ->
                start
                    { slideCount = 1
                    , slideLabels = AccessibleLabel
                    , containerLabel = AccessibleLabel
                    }
                    |> ensureSlideIsVisible "slide-0"
                    |> clickButton "Next"
                    |> ensureSlideIsVisible "slide-0"
                    |> clickButton "Previous"
                    |> ensureSlideIsVisible "slide-0"
                    |> done
        , test "Announces card changes for screen reader users" <|
            \_ ->
                start
                    { slideCount = 3
                    , slideLabels = AccessibleLabel
                    , containerLabel = AccessibleLabel
                    }
                    |> ensureSlideIsVisible "slide-0"
                    |> clickButton "Next"
                    |> ensureLastEffect (Expect.equal [ Announce "Active slide of Slides changed to Slide 1" ])
                    |> clickButton "Next"
                    |> ensureLastEffect (Expect.equal [ Announce "Active slide of Slides changed to Slide 2" ])
                    |> clickButton "Previous"
                    |> ensureLastEffect (Expect.equal [ Announce "Active slide of Slides changed to Slide 1" ])
                    |> done
        , test "If the visibleLabelId is Nothing the container aria label is set to the name" <|
            \_ ->
                start
                    { slideCount = 3
                    , slideLabels = AccessibleLabel
                    , containerLabel = AccessibleLabel
                    }
                    |> ensureViewHas
                        [ Selector.all
                            [ Selector.attribute (Aria.roleDescription "carousel")
                            , Selector.attribute (Aria.label "Slides")
                            ]
                        ]
                    |> done
        , test "If the visibleLabelId is set the container aria labelledby is set to the visibleLabelId" <|
            \_ ->
                start
                    { slideCount = 3
                    , slideLabels = AccessibleLabel
                    , containerLabel = VisibleLabel
                    }
                    |> ensureViewHas
                        [ Selector.all
                            [ Selector.attribute (Aria.roleDescription "carousel")
                            , Selector.attribute (Aria.labelledBy "carousel-label")
                            ]
                        ]
                    |> done
        , test "If the visibleLabelId is Nothing the slides container aria label is set to the name" <|
            \_ ->
                start
                    { slideCount = 1
                    , slideLabels = AccessibleLabel
                    , containerLabel = AccessibleLabel
                    }
                    |> ensureViewHas
                        [ Selector.all
                            [ Selector.id "slide-0"
                            , Selector.attribute (Aria.label "Slide 0")
                            ]
                        ]
                    |> done
        , test "If the visibleLabelId is set the slides container aria label is set to the visibleLabelId" <|
            \_ ->
                start
                    { slideCount = 1
                    , slideLabels = VisibleLabel
                    , containerLabel = AccessibleLabel
                    }
                    |> ensureViewHas
                        [ Selector.all
                            [ Selector.id "slide-0"
                            , Selector.attribute (Aria.labelledBy "slide-0-label")
                            ]
                        ]
                    |> done
        ]


viewWithTabControlsSpec : Test
viewWithTabControlsSpec =
    let
        allSlides =
            [ { id = 0
              , idString = "slide-0"
              , slideView = text "Slide 0"
              , tabView = text "Control 0"
              , tabAttributes = []
              }
            , { id = 1
              , idString = "slide-1"
              , slideView = text "Slide 1"
              , tabView = text "Control 1"
              , tabAttributes = []
              }
            , { id = 2
              , idString = "slide-2"
              , slideView = text "Slide 2"
              , tabView = text "Control 2"
              , tabAttributes = []
              }
            ]

        start slideCount containerLabel =
            ProgramTest.createElement
                { init = init
                , update = update
                , view =
                    \model ->
                        Carousel.viewWithTabControls
                            { selected = model.selected
                            , slides = List.take slideCount allSlides
                            , tabStyles = \_ -> []
                            , tabListStyles = []
                            , role = Carousel.Group
                            , name = "Slides"
                            , visibleLabelId =
                                case containerLabel of
                                    AccessibleLabel ->
                                        Nothing

                                    VisibleLabel ->
                                        Just "carousel-label"
                            , focusAndSelect =
                                \{ select, focus } ->
                                    Select
                                        { select = select
                                        , announce = Nothing
                                        , focus = focus
                                        }
                            }
                            |> (\{ tabs, slides, containerAttributes } ->
                                    section containerAttributes [ slides, tabs ]
                               )
                            |> toUnstyled
                }
                |> ProgramTest.start ()
    in
    describe "viewWithTabControls"
        [ describe "rendering"
            [ test "displays the associated slide when a control is activated" <|
                \() ->
                    start 3 AccessibleLabel
                        |> ensureTabbable "Control 0"
                        |> ensurePanelDisplayed "Slide 0"
                        |> done
            , test "does not crash with 0 slides" <|
                \() ->
                    start 0 AccessibleLabel
                        |> ensureViewHas [ Selector.attribute Role.tabList ]
                        |> done
            , test "has only one slide displayed" <|
                \() ->
                    start 3 AccessibleLabel
                        |> ensureOnlyOnePanelDisplayed [ "Slide 0", "Slide 1", "Slide 2" ]
                        |> done
            ]
        , describe "keyboard behavior"
            [ test "has a focusable control" <|
                \() ->
                    start 3 AccessibleLabel
                        |> ensureTabbable "Control 0"
                        |> done
            , test "all slides are focusable" <|
                \() ->
                    start 3 AccessibleLabel
                        |> ensurePanelsFocusable [ "Slide 0", "Slide 1", "Slide 2" ]
                        |> done
            , test "has only one control included in the tab sequence" <|
                \() ->
                    start 3 AccessibleLabel
                        |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                        |> done
            , test "moves focus right on right arrow key" <|
                \() ->
                    start 3 AccessibleLabel
                        |> ensureTabbable "Control 0"
                        |> releaseRightArrow
                        |> ensureTabbable "Control 1"
                        |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                        |> releaseRightArrow
                        |> ensureTabbable "Control 2"
                        |> done
            , test "moves focus left on left arrow key" <|
                \() ->
                    start 3 AccessibleLabel
                        |> ensureTabbable "Control 0"
                        |> releaseRightArrow
                        |> ensureTabbable "Control 1"
                        |> releaseLeftArrow
                        |> ensureTabbable "Control 0"
                        |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                        |> done
            , test "when the focus is on the first element, move focus to the last element on left arrow key" <|
                \() ->
                    start 3 AccessibleLabel
                        |> ensureTabbable "Control 0"
                        |> releaseLeftArrow
                        |> ensureTabbable "Control 2"
                        |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                        |> done
            , test "when the focus is on the last element, move focus to the first element on right arrow key" <|
                \() ->
                    start 3 AccessibleLabel
                        |> ensureTabbable "Control 0"
                        |> releaseLeftArrow
                        |> ensureTabbable "Control 2"
                        |> releaseRightArrow
                        |> ensureTabbable "Control 0"
                        |> ensureOnlyOneTabInSequence [ "Control 0", "Control 1", "Control 2" ]
                        |> done
            , test "If the visibleLabelId is Nothing the container aria label is set to the name" <|
                \_ ->
                    start 3 AccessibleLabel
                        |> ensureViewHas
                            [ Selector.all
                                [ Selector.attribute (Aria.roleDescription "carousel")
                                , Selector.attribute (Aria.label "Slides")
                                ]
                            ]
                        |> done
            , test "If the visibleLabelId is set the container aria labelledby is set to the visibleLabelId" <|
                \_ ->
                    start 3 VisibleLabel
                        |> ensureViewHas
                            [ Selector.all
                                [ Selector.attribute (Aria.roleDescription "carousel")
                                , Selector.attribute (Aria.labelledBy "carousel-label")
                                ]
                            ]
                        |> done
            ]
        ]


viewWithCombinedControlsSpec : Test
viewWithCombinedControlsSpec =
    let
        allSlides =
            [ { id = 0
              , idString = "slide-0"
              , name = "Slide 0"
              , visibleLabelId = Nothing
              , slideView = text "Slide 0"
              , tabView = text "Control 0"
              , tabAttributes = []
              }
            , { id = 1
              , idString = "slide-1"
              , name = "Slide 1"
              , visibleLabelId = Nothing
              , slideView = text "Slide 1"
              , tabView = text "Control 1"
              , tabAttributes = []
              }
            , { id = 2
              , idString = "slide-2"
              , name = "Slide 2"
              , visibleLabelId = Nothing
              , slideView = text "Slide 2"
              , tabView = text "Control 2"
              , tabAttributes = []
              }
            ]

        start slideCount =
            ProgramTest.createElement
                { init = init
                , update = update
                , view =
                    \model ->
                        Carousel.viewWithCombinedControls
                            { selected = model.selected
                            , slides = List.take slideCount allSlides
                            , role = Carousel.Group
                            , tabStyles = \_ -> []
                            , tabListStyles = []
                            , name = "Slides"
                            , visibleLabelId = Nothing
                            , previousButton = { attributes = [], icon = UiIcon.arrowLeft, name = "Previous" }
                            , nextButton = { attributes = [], icon = UiIcon.arrowRight, name = "Next" }
                            , select = Select
                            }
                            |> (\{ tabs, slides, containerAttributes, viewNextButton, viewPreviousButton } ->
                                    section containerAttributes [ slides, tabs, viewNextButton, viewPreviousButton ]
                               )
                            |> toUnstyled
                }
                |> ProgramTest.start ()
    in
    describe "viewWithCombinedControls"
        [ test "does not crash with 0 slides" <|
            \() ->
                start 0
                    |> ensureViewHas [ Selector.attribute (Aria.label "Slides") ]
                    |> done
        , test "rotate back and forward with 3 slides" <|
            \_ ->
                start 3
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
    ensureViewHas [ Selector.all [ Selector.id id, Selector.style "display" "block" ] ]
