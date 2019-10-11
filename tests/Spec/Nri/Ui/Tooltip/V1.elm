module Spec.Nri.Ui.Tooltip.V1 exposing (spec)

import Accessibility.Aria as Aria
import Accessibility.Widget as Widget
import Expect
import Html
import Html.Styled as HtmlStyled
import Nri.Ui.Tooltip.V1 as Tooltip
import ProgramTest
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (id, tag)


type alias Model =
    { tooltipOpen : Bool }


init : Model
init =
    { tooltipOpen = False }


type Msg
    = ToggleTooltip Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleTooltip isOpen ->
            { tooltipOpen = isOpen }


spec : Test
spec =
    describe "Nri.Ui.Tooltip.V1"
        [ describe "toggleTip"
            [ test "Toggletip is available on click and hides on blur" <|
                \() ->
                    ProgramTest.createSandbox
                        { init = init
                        , update = update
                        , view = viewToggleTip
                        }
                        |> ProgramTest.start ()
                        |> ProgramTest.simulateDomEvent
                            (Query.find
                                [ Selector.tag "button"
                                , Selector.attribute
                                    (Widget.label "More info")
                                ]
                            )
                            Event.click
                        |> ProgramTest.ensureViewHas
                            [ Selector.text "Toggly"
                            ]
                        |> ProgramTest.simulateDomEvent
                            (Query.find
                                [ Selector.tag "button"
                                , Selector.attribute
                                    (Widget.label "More info")
                                ]
                            )
                            Event.blur
                        |> ProgramTest.ensureViewHasNot
                            [ Selector.text "Toggly"
                            ]
                        |> ProgramTest.done
            ]
        , describe "tooltips"
            ([ Tooltip.OnClick, Tooltip.OnHover ]
                |> List.map
                    (\trigger ->
                        test ("Tooltip with " ++ Debug.toString trigger ++ " trigger is available on focus") <|
                            \() ->
                                ProgramTest.createSandbox
                                    { init = init
                                    , update = update
                                    , view = viewTooltip trigger
                                    }
                                    |> ProgramTest.start ()
                                    |> ProgramTest.simulateDomEvent
                                        (Query.find
                                            [ Selector.tag "button"
                                            , Selector.containing [ Selector.text "label-less icon" ]
                                            ]
                                        )
                                        Event.focus
                                    |> ProgramTest.ensureViewHas
                                        [ tag "button"
                                        , Selector.attribute (Aria.labeledBy "primary-label")
                                        ]
                                    |> ProgramTest.ensureViewHas
                                        [ id "primary-label"
                                        , Selector.text "This will be the primary label"
                                        ]
                                    |> ProgramTest.simulateDomEvent
                                        (Query.find
                                            [ Selector.tag "button"
                                            , Selector.containing [ Selector.text "label-less icon" ]
                                            ]
                                        )
                                        Event.blur
                                    |> ProgramTest.ensureViewHasNot
                                        [ id "primary-label"
                                        , Selector.text "This will be the primary label"
                                        ]
                                    |> ProgramTest.done
                    )
            )
        ]


viewTooltip : Tooltip.Trigger -> Model -> Html.Html Msg
viewTooltip trigger model =
    Tooltip.tooltip [ HtmlStyled.text "This will be the primary label" ]
        |> Tooltip.primaryLabel
            { trigger = trigger
            , triggerHtml = HtmlStyled.text "label-less icon"
            , onTrigger = ToggleTooltip
            , isOpen = model.tooltipOpen
            , extraButtonAttrs = []
            , id = "primary-label"
            }
        |> HtmlStyled.toUnstyled


viewToggleTip : Model -> Html.Html Msg
viewToggleTip model =
    Tooltip.tooltip [ HtmlStyled.text "Toggly!" ]
        |> Tooltip.toggleTip
            { onTrigger = ToggleTooltip
            , isOpen = model.tooltipOpen
            , extraButtonAttrs = []
            , label = "More info"
            }
        |> HtmlStyled.toUnstyled
