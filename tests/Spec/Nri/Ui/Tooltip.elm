module Spec.Nri.Ui.Tooltip exposing (spec)

import Accessibility.Aria as Aria
import Accessibility.Widget as Widget
import Expect
import Html
import Html.Styled as HtmlStyled
import Nri.Ui.Tooltip.V2 as Tooltip
import ProgramTest
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (id, tag, text)


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
    describe "Nri.Ui.Tooltip.V2"
        [ describe "toggleTip"
            [ test "Toggletip is available on hover and hides on blur" <|
                \() ->
                    let
                        tooltipContent =
                            "Toggly!"

                        label =
                            "More info"
                    in
                    ProgramTest.createSandbox
                        { init = init
                        , update = update
                        , view =
                            \model ->
                                Tooltip.toggleTip { label = label }
                                    [ Tooltip.plaintext tooltipContent
                                    , Tooltip.onHover ToggleTooltip
                                    , Tooltip.open model.tooltipOpen
                                    ]
                                    |> HtmlStyled.toUnstyled
                        }
                        |> ProgramTest.start ()
                        |> ProgramTest.simulateDomEvent
                            (Query.find
                                [ Selector.tag "button"
                                , Selector.attribute (Widget.label label)
                                ]
                            )
                            Event.mouseEnter
                        |> ProgramTest.ensureViewHas [ Selector.text tooltipContent ]
                        |> ProgramTest.simulateDomEvent
                            (Query.find
                                [ Selector.tag "button"
                                , Selector.attribute (Widget.label label)
                                ]
                            )
                            Event.blur
                        |> ProgramTest.ensureViewHasNot [ Selector.text tooltipContent ]
                        |> ProgramTest.done
            ]
        , test "tooltip with onClick trigger" <|
            \() ->
                ProgramTest.createSandbox
                    { init = init
                    , update = update
                    , view =
                        \model ->
                            Tooltip.view
                                { trigger = \events -> HtmlStyled.button events [ HtmlStyled.text "label-less icon" ]
                                , id = "primary-label"
                                }
                                [ Tooltip.plaintext "This will be the primary label"
                                , Tooltip.primaryLabel
                                , Tooltip.onClick ToggleTooltip
                                , Tooltip.open model.tooltipOpen
                                ]
                                |> HtmlStyled.toUnstyled
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
        , test "tooltip with onHover trigger" <|
            \() ->
                ProgramTest.createSandbox
                    { init = init
                    , update = update
                    , view =
                        \model ->
                            Tooltip.view
                                { trigger = \events -> HtmlStyled.button events [ HtmlStyled.text "label-less icon" ]
                                , id = "primary-label"
                                }
                                [ Tooltip.plaintext "This will be the primary label"
                                , Tooltip.primaryLabel
                                , Tooltip.onHover ToggleTooltip
                                , Tooltip.open model.tooltipOpen
                                ]
                                |> HtmlStyled.toUnstyled
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
        ]
