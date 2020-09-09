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
        [ test "Tooltip.toggleTip with onHover trigger" <|
            \() ->
                let
                    tooltipContent =
                        "Toggly!"

                    label =
                        "More info"
                in
                program
                    (Tooltip.toggleTip { label = label })
                    [ Tooltip.plaintext tooltipContent
                    , Tooltip.onHover ToggleTooltip
                    ]
                    |> mouseEnter
                        [ Selector.tag "button"
                        , Selector.attribute (Widget.label label)
                        ]
                    |> ProgramTest.ensureViewHas [ Selector.text tooltipContent ]
                    |> blur
                        [ Selector.tag "button"
                        , Selector.attribute (Widget.label label)
                        ]
                    |> ProgramTest.ensureViewHasNot [ Selector.text tooltipContent ]
                    |> ProgramTest.done
        , test "Tooltip.view with onClick trigger" <|
            \() ->
                program
                    (Tooltip.view
                        { trigger = \events -> HtmlStyled.button events [ HtmlStyled.text "label-less icon" ]
                        , id = "primary-label"
                        }
                    )
                    [ Tooltip.plaintext "This will be the primary label"
                    , Tooltip.primaryLabel
                    , Tooltip.onClick ToggleTooltip
                    ]
                    |> focus
                        [ Selector.tag "button"
                        , Selector.containing [ Selector.text "label-less icon" ]
                        ]
                    |> ProgramTest.ensureViewHas
                        [ tag "button"
                        , Selector.attribute (Aria.labeledBy "primary-label")
                        ]
                    |> ProgramTest.ensureViewHas
                        [ id "primary-label"
                        , Selector.text "This will be the primary label"
                        ]
                    |> blur
                        [ Selector.tag "button"
                        , Selector.containing [ Selector.text "label-less icon" ]
                        ]
                    |> ProgramTest.ensureViewHasNot
                        [ id "primary-label"
                        , Selector.text "This will be the primary label"
                        ]
                    |> ProgramTest.done
        , test "Tooltip.view with onHover trigger" <|
            \() ->
                program
                    (Tooltip.view
                        { trigger = \events -> HtmlStyled.button events [ HtmlStyled.text "label-less icon" ]
                        , id = "primary-label"
                        }
                    )
                    [ Tooltip.plaintext "This will be the primary label"
                    , Tooltip.primaryLabel
                    , Tooltip.onHover ToggleTooltip
                    ]
                    |> focus
                        [ Selector.tag "button"
                        , Selector.containing [ Selector.text "label-less icon" ]
                        ]
                    |> ProgramTest.ensureViewHas
                        [ tag "button"
                        , Selector.attribute (Aria.labeledBy "primary-label")
                        ]
                    |> ProgramTest.ensureViewHas
                        [ id "primary-label"
                        , Selector.text "This will be the primary label"
                        ]
                    |> blur
                        [ Selector.tag "button"
                        , Selector.containing [ Selector.text "label-less icon" ]
                        ]
                    |> ProgramTest.ensureViewHasNot
                        [ id "primary-label"
                        , Selector.text "This will be the primary label"
                        ]
                    |> ProgramTest.done
        ]


program : (List (Tooltip.Attribute Msg) -> HtmlStyled.Html Msg) -> List (Tooltip.Attribute Msg) -> ProgramTest.ProgramTest Model Msg ()
program view attributes =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = \model -> HtmlStyled.toUnstyled (view (Tooltip.open model.tooltipOpen :: attributes))
        }
        |> ProgramTest.start ()


mouseEnter : List Selector.Selector -> ProgramTest.ProgramTest model msg effect -> ProgramTest.ProgramTest model msg effect
mouseEnter selectors =
    ProgramTest.simulateDomEvent (Query.find selectors) Event.mouseEnter


mouseLeave : List Selector.Selector -> ProgramTest.ProgramTest model msg effect -> ProgramTest.ProgramTest model msg effect
mouseLeave selectors =
    ProgramTest.simulateDomEvent (Query.find selectors) Event.mouseLeave


blur : List Selector.Selector -> ProgramTest.ProgramTest model msg effect -> ProgramTest.ProgramTest model msg effect
blur selectors =
    ProgramTest.simulateDomEvent (Query.find selectors) Event.blur


focus : List Selector.Selector -> ProgramTest.ProgramTest model msg effect -> ProgramTest.ProgramTest model msg effect
focus selectors =
    ProgramTest.simulateDomEvent (Query.find selectors) Event.focuss
