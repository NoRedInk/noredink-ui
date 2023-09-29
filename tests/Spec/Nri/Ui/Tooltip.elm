module Spec.Nri.Ui.Tooltip exposing (spec)

import Accessibility.Aria as Aria
import Accessibility.Styled.Key as Key
import Expect
import Html.Attributes
import Html.Styled as HtmlStyled
import Html.Styled.Attributes as Attrs
import Nri.Test.KeyboardHelpers.V1 as KeyboardHelpers
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Tooltip.V3 as Tooltip
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
                    , Tooltip.onToggle Toggle
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
                    , Tooltip.onToggle Toggle
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

                    view model =
                        Tooltip.view
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
                            , Tooltip.onToggle (\_ -> ())
                            , Tooltip.disclosure
                                { triggerId = triggerId
                                , lastId = Nothing
                                }
                            ]
                            |> HtmlStyled.toUnstyled
                in
                view { isOpen = False }
                    |> Query.fromHtml
                    |> Query.find [ Selector.id triggerId ]
                    |> Event.simulate Event.click
                    |> Expect.all [ Event.expectStopPropagation, Event.expectPreventDefault ]
        , test "Adds additional keyDown event handlers with Tooltip.onTriggerKeyDown" <|
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
                in
                program
                    (Tooltip.view
                        { trigger = \events -> HtmlStyled.button (Attrs.id triggerId :: events) [ HtmlStyled.text triggerContent ]
                        , id = tooltipId
                        }
                    )
                    [ Tooltip.plaintext tooltipContent
                    , Tooltip.primaryLabel
                    , Tooltip.onToggle Toggle
                    , Tooltip.onTriggerKeyDown
                        [ Key.space SpaceKeyPressed ]
                    ]
                    |> KeyboardHelpers.pressSpace khConfig
                        { targetDetails = [] }
                        [ Selector.id triggerId
                        ]
                    |> ProgramTest.expectModel (\model -> Expect.equal (Just SpaceKeyPressed) model.lastMsg)
        ]


type alias Model =
    { isOpen : Bool
    , lastMsg : Maybe Msg
    }


type Msg
    = Toggle Bool
    | SpaceKeyPressed


program : (List (Tooltip.Attribute Msg) -> HtmlStyled.Html Msg) -> List (Tooltip.Attribute Msg) -> ProgramTest Model Msg ()
program view attributes =
    ProgramTest.createSandbox
        { init =
            { isOpen = False
            , lastMsg = Nothing
            }
        , update =
            \msg model ->
                let
                    updatedModel =
                        case msg of
                            Toggle isOpen ->
                                { model | isOpen = isOpen }

                            _ ->
                                model
                in
                { updatedModel | lastMsg = Just msg }
        , view =
            \{ isOpen } ->
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


clickButtonByLabel : String -> ProgramTest model msg effect -> ProgramTest model msg effect
clickButtonByLabel label =
    ProgramTest.simulateDomEvent
        (Query.find
            [ Selector.tag "button"
            , Selector.attribute (Aria.label label)
            ]
        )
        Event.click


khConfig : KeyboardHelpers.Config (ProgramTest model msg effect) Selector.Selector (Query.Single msg)
khConfig =
    { programTest_simulateDomEvent = ProgramTest.simulateDomEvent
    , query_find = Query.find
    , event_custom = Event.custom
    }
