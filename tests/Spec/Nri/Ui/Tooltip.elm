module Spec.Nri.Ui.Tooltip exposing (spec)

import Accessibility.Aria as Aria
import Html.Attributes as Attributes
import Html.Styled as HtmlStyled
import Nri.Ui.Tooltip.V4 as Tooltip
import ProgramTest exposing (ProgramTest, ensureViewHas, ensureViewHasNot)
import Spec.Helpers exposing (nriDescription)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (id, text)


spec : Test
spec =
    describe "Nri.Ui.Tooltip.V3"
        [ test "Tooltip.viewToggleTip" <|
            \() ->
                let
                    tooltipContent =
                        "Toggly!"

                    label =
                        "More info"
                in
                program (Tooltip.viewToggleTip { label = label, lastId = Nothing })
                    [ Tooltip.plaintext tooltipContent
                    , Tooltip.onToggle identity
                    ]
                    -- Tooltip opens on mouse enter
                    |> mouseEnter [ nriDescription "Nri-Ui-Tooltip-V2" ]
                    |> ensureViewHas (tooltipContentSelector tooltipContent)
                    -- Tooltip closes on trigger-html click
                    |> clickButtonByLabel label
                    |> ensureViewHasNot (tooltipContentSelector tooltipContent)
                    -- Tooltip reopens on trigger-html click
                    |> clickButtonByLabel label
                    |> ensureViewHas (tooltipContentSelector tooltipContent)
                    -- Tooltip closes on mouse leave
                    |> mouseLeave [ nriDescription "Nri-Ui-Tooltip-V2" ]
                    |> ensureViewHasNot (tooltipContentSelector tooltipContent)
                    |> ProgramTest.done
        , test "Tooltip.view" <|
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
                    , Tooltip.onToggle identity
                    ]
                    -- Tooltip opens on mouse enter
                    |> mouseEnter [ nriDescription "Nri-Ui-Tooltip-V2" ]
                    |> ensureViewHas (tooltipContentSelector tooltipContent)
                    -- Tooltip closes on mouse leave
                    |> mouseLeave [ nriDescription "Nri-Ui-Tooltip-V2" ]
                    |> ensureViewHasNot (tooltipContentSelector tooltipContent)
                    -- Tooltip opens on focus
                    |> focus
                        [ Selector.tag "button"
                        , Selector.containing [ Selector.text triggerContent ]
                        ]
                    |> ProgramTest.ensureViewHas (tooltipContentSelector tooltipContent)
                    -- Tooltip closes on blur
                    |> blur
                        [ Selector.tag "button"
                        , Selector.containing [ Selector.text triggerContent ]
                        ]
                    |> ProgramTest.ensureViewHasNot (id tooltipId :: tooltipContentSelector tooltipContent)
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


tooltipContentSelector : String -> List Selector.Selector
tooltipContentSelector tooltipContent =
    [ Selector.attribute (Attributes.attribute "data-tooltip-visible" "true")
    , Selector.containing [ text tooltipContent ]
    ]


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
            , Selector.attribute (Aria.label label)
            ]
        )
        Event.click
