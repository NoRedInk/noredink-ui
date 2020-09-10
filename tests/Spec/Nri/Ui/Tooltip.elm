module Spec.Nri.Ui.Tooltip exposing (spec)

import Accessibility.Aria as Aria
import Accessibility.Widget as Widget
import Expect
import Html
import Html.Attributes as Attributes
import Html.Styled as HtmlStyled
import Nri.Ui.Tooltip.V2 as Tooltip
import ProgramTest exposing (ProgramTest, clickButton, ensureViewHas, ensureViewHasNot)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (id, tag, text)


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
                program (Tooltip.toggleTip { label = label })
                    [ Tooltip.plaintext tooltipContent
                    , Tooltip.onHover identity
                    ]
                    -- Tooltip opens on mouse enter
                    |> mouseEnter [ nriDescription "Nri-Ui-Tooltip-V2" ]
                    |> ensureViewHas [ text tooltipContent ]
                    -- Tooltip stays open on trigger-html click
                    |> clickButtonByLabel label
                    |> ensureViewHas [ text tooltipContent ]
                    -- Tooltip closes on mouse leave
                    |> mouseLeave [ nriDescription "Nri-Ui-Tooltip-V2" ]
                    |> ensureViewHasNot [ text tooltipContent ]
                    -- Tooltip opens on focus
                    |> focus [ tag "button", Selector.attribute (Widget.label label) ]
                    |> ensureViewHas [ text tooltipContent ]
                    -- Tooltip closes on blur
                    |> blur [ tag "button", Selector.attribute (Widget.label label) ]
                    |> ensureViewHasNot [ text tooltipContent ]
                    |> ProgramTest.done
        , test "Tooltip.view with onClick trigger" <|
            \() ->
                let
                    tooltipContent =
                        "This will be the primary label"

                    triggerContent =
                        "label-less icon"

                    tooltipId =
                        "primary-label"
                in
                program
                    (Tooltip.view
                        { trigger = \events -> HtmlStyled.button events [ HtmlStyled.text triggerContent ]
                        , id = tooltipId
                        }
                    )
                    [ Tooltip.plaintext tooltipContent
                    , Tooltip.primaryLabel
                    , Tooltip.onClick identity
                    ]
                    -- Tooltip opens on click
                    |> clickButton triggerContent
                    |> ensureViewHas
                        [ tag "button"
                        , Selector.attribute (Aria.labeledBy tooltipId)
                        ]
                    |> ensureViewHas [ id tooltipId, text tooltipContent ]
                    -- Tooltip closes on another click
                    |> clickButton triggerContent
                    |> ensureViewHasNot [ id tooltipId, text tooltipContent ]
                    |> ProgramTest.done
        , test "Tooltip.view with onHover trigger" <|
            \() ->
                let
                    tooltipContent =
                        "This will be the primary label"

                    triggerContent =
                        "label-less icon"

                    tooltipId =
                        "primary-label"
                in
                program
                    (Tooltip.view
                        { trigger = \events -> HtmlStyled.button events [ HtmlStyled.text triggerContent ]
                        , id = tooltipId
                        }
                    )
                    [ Tooltip.plaintext tooltipContent
                    , Tooltip.primaryLabel
                    , Tooltip.onHover identity
                    ]
                    -- Tooltip opens on mouse enter
                    |> mouseEnter [ nriDescription "Nri-Ui-Tooltip-V2" ]
                    |> ensureViewHas [ text tooltipContent ]
                    -- Tooltip stays open on trigger-html click
                    |> clickButton triggerContent
                    |> ensureViewHas [ text tooltipContent ]
                    -- Tooltip closes on mouse leave
                    |> mouseLeave [ nriDescription "Nri-Ui-Tooltip-V2" ]
                    |> ensureViewHasNot [ text tooltipContent ]
                    -- Tooltip opens on focus
                    |> focus
                        [ Selector.tag "button"
                        , Selector.containing [ Selector.text triggerContent ]
                        ]
                    |> ProgramTest.ensureViewHas
                        [ tag "button"
                        , Selector.attribute (Aria.labeledBy tooltipId)
                        ]
                    |> ProgramTest.ensureViewHas
                        [ id tooltipId
                        , Selector.text tooltipContent
                        ]
                    -- Tooltip closes on blur
                    |> blur
                        [ Selector.tag "button"
                        , Selector.containing [ Selector.text triggerContent ]
                        ]
                    |> ProgramTest.ensureViewHasNot
                        [ id tooltipId
                        , Selector.text tooltipContent
                        ]
                    |> ProgramTest.done
        ]


program : (List (Tooltip.Attribute Bool) -> HtmlStyled.Html Bool) -> List (Tooltip.Attribute Bool) -> ProgramTest Bool Bool ()
program view attributes =
    ProgramTest.createSandbox
        { init = False
        , update = \msg model -> msg
        , view =
            \isOpen ->
                HtmlStyled.div []
                    [ view (Tooltip.open isOpen :: attributes)
                    ]
                    |> HtmlStyled.toUnstyled
        }
        |> ProgramTest.start ()


nriDescription : String -> Selector.Selector
nriDescription desc =
    Selector.attribute (Attributes.attribute "data-nri-description" desc)


mouseEnter : List Selector.Selector -> ProgramTest model msg effect -> ProgramTest model msg effect
mouseEnter selectors =
    ProgramTest.simulateDomEvent (Query.find selectors) Event.mouseEnter


mouseLeave : List Selector.Selector -> ProgramTest model msg effect -> ProgramTest model msg effect
mouseLeave selectors =
    ProgramTest.simulateDomEvent (Query.find selectors) Event.mouseLeave


blur : List Selector.Selector -> ProgramTest model msg effect -> ProgramTest model msg effect
blur selectors =
    ProgramTest.simulateDomEvent (Query.find selectors) Event.blur


focus : List Selector.Selector -> ProgramTest model msg effect -> ProgramTest model msg effect
focus selectors =
    ProgramTest.simulateDomEvent (Query.find selectors) Event.focus


clickButtonByLabel : String -> ProgramTest model msg effect -> ProgramTest model msg effect
clickButtonByLabel label =
    ProgramTest.simulateDomEvent
        (Query.find
            [ Selector.tag "button"
            , Selector.attribute (Widget.label label)
            ]
        )
        Event.click
