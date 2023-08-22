module Spec.Nri.Ui.Tooltip exposing (spec)

import Accessibility.Aria as Aria
import Expect
import Html.Attributes
import Html.Styled as HtmlStyled
import Html.Styled.Events as Events
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Tooltip.V3 as Tooltip
import ProgramTest exposing (ProgramTest, ensureViewHas, ensureViewHasNot)
import Spec.Helpers exposing (nriDescription)
import Spec.Nri.Ui.Checkbox exposing (Msg(..))
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (id, text)


type Msg
    = ParentClicked
    | ToggleTooltip Bool


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
        , test "Prevents default on disclosures" <|
            \() ->
                let
                    tooltipContent =
                        "This will be the primary label"

                    triggerContent =
                        "label-less icon"

                    tooltipId =
                        "primary-label"

                    triggerId =
                        "trigger"

                    program_ =
                        ProgramTest.createSandbox
                            { init = { parentClicked = False, isOpen = False }
                            , update =
                                \msg model ->
                                    case Debug.log "msg" msg of
                                        ParentClicked ->
                                            { model | parentClicked = True }

                                        ToggleTooltip isOpen ->
                                            { model | isOpen = isOpen }
                            , view =
                                \model ->
                                    HtmlStyled.div
                                        [ Events.onClick ParentClicked
                                        ]
                                        [ Tooltip.view
                                            { trigger =
                                                \attributes ->
                                                    ClickableText.button triggerContent
                                                        [ ClickableText.custom attributes
                                                        , ClickableText.id triggerId
                                                        ]
                                            , id = tooltipId
                                            }
                                            [ Tooltip.open model.isOpen
                                            , Tooltip.plaintext tooltipContent
                                            , Tooltip.primaryLabel
                                            , Tooltip.onToggle ToggleTooltip
                                            , Tooltip.disclosure
                                                { triggerId = triggerId
                                                , lastId = Nothing
                                                }
                                            ]
                                        ]
                                        |> HtmlStyled.toUnstyled
                            }
                            |> ProgramTest.start ()
                in
                program_
                    |> ensureViewHasNot (tooltipContentSelector tooltipContent)
                    |> clickById triggerId
                    |> ensureViewHas (tooltipContentSelector tooltipContent)
                    |> ProgramTest.expectModel
                        (\model ->
                            Expect.equal False model.parentClicked
                        )
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
    [ Selector.attribute (Html.Attributes.attribute "data-tooltip-visible" "true")
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


clickById : String -> ProgramTest model msg effect -> ProgramTest model msg effect
clickById id =
    ProgramTest.simulateDomEvent (Query.find [ Selector.id id ]) Event.click


clickButtonByLabel : String -> ProgramTest model msg effect -> ProgramTest model msg effect
clickButtonByLabel label =
    ProgramTest.simulateDomEvent
        (Query.find
            [ Selector.tag "button"
            , Selector.attribute (Aria.label label)
            ]
        )
        Event.click
