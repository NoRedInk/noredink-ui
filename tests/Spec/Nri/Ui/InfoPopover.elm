module Spec.Nri.Ui.InfoPopover exposing (spec)

import Expect
import Html.Attributes as UnstyledAttributes
import Html.Styled as HtmlStyled
import Html.Styled.Attributes as Attrs
import Json.Encode
import Nri.Test.KeyboardHelpers.V1 as KeyboardHelpers
import Nri.Ui.InfoPopover.V1 as InfoPopover
import ProgramTest exposing (ProgramTest)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (id)


spec : Test
spec =
    describe "Nri.Ui.InfoPopover.V1"
        [ test "wires trigger and popover ARIA attributes" <|
            \() ->
                HtmlStyled.div []
                    [ InfoPopover.view
                        { id = "popover"
                        , triggerId = "popover-trigger"
                        , label = "Planet details"
                        , trigger = \events -> HtmlStyled.button events [ HtmlStyled.text "Details" ]
                        }
                        [ InfoPopover.plaintext "Mercury"
                        , InfoPopover.open True
                        ]
                    ]
                    |> HtmlStyled.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has
                            [ Selector.tag "button"
                            , id "popover-trigger"
                            , Selector.attribute (UnstyledAttributes.attribute "aria-haspopup" "dialog")
                            , Selector.attribute (UnstyledAttributes.attribute "aria-expanded" "true")
                            , Selector.attribute (UnstyledAttributes.attribute "aria-controls" "popover")
                            ]
                        , Query.has
                            [ Selector.tag "nri-anchored-overlay-v1"
                            , id "popover"
                            , Selector.attribute (UnstyledAttributes.attribute "role" "dialog")
                            , Selector.attribute (UnstyledAttributes.attribute "aria-label" "Planet details")
                            , Selector.attribute (UnstyledAttributes.attribute "open" "")
                            ]
                        ]
        , test "clicking the trigger requests a toggle" <|
            \() ->
                program
                    [ InfoPopover.plaintext "Mercury"
                    , InfoPopover.onToggle Toggle
                    ]
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ Selector.tag "button", id "popover-trigger" ])
                        Event.click
                    |> ProgramTest.expectModel (\model -> Expect.equal (Just (Toggle True)) model.lastMsg)
        , test "pressing Enter on the trigger requests a toggle" <|
            \() ->
                program
                    [ InfoPopover.plaintext "Mercury"
                    , InfoPopover.onToggle Toggle
                    ]
                    |> KeyboardHelpers.pressKey { targetDetails = [], keyCode = 13, shiftKey = False } [ Selector.tag "button", id "popover-trigger" ]
                    |> ProgramTest.expectModel (\model -> Expect.equal (Just (Toggle True)) model.lastMsg)
        , test "pressing Space on the trigger requests a toggle" <|
            \() ->
                program
                    [ InfoPopover.plaintext "Mercury"
                    , InfoPopover.onToggle Toggle
                    ]
                    |> KeyboardHelpers.pressSpace { targetDetails = [] } [ Selector.tag "button", id "popover-trigger" ]
                    |> ProgramTest.expectModel (\model -> Expect.equal (Just (Toggle True)) model.lastMsg)
        , test "request-close events are decoded back through onToggle False" <|
            \() ->
                let
                    eventPayload =
                        Json.Encode.object []
                in
                program
                    [ InfoPopover.plaintext "Mercury"
                    , InfoPopover.onToggle Toggle
                    ]
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ Selector.tag "nri-anchored-overlay-v1", id "popover" ])
                        (Event.custom "request-close" eventPayload)
                    |> ProgramTest.expectModel (\model -> Expect.equal (Just (Toggle False)) model.lastMsg)
        ]


type alias Model =
    { isOpen : Bool
    , lastMsg : Maybe Msg
    }


type Msg
    = Toggle Bool


program : List (InfoPopover.Attribute Msg) -> ProgramTest Model Msg ()
program attributes =
    ProgramTest.createSandbox
        { init =
            { isOpen = False
            , lastMsg = Nothing
            }
        , update =
            \msg model ->
                let
                    nextModel =
                        case msg of
                            Toggle isOpen ->
                                { model | isOpen = isOpen }
                in
                { nextModel | lastMsg = Just msg }
        , view =
            \model ->
                InfoPopover.view
                    { id = "popover"
                    , triggerId = "popover-trigger"
                    , label = "Planet details"
                    , trigger = \events -> HtmlStyled.button (Attrs.type_ "button" :: events) [ HtmlStyled.text "Details" ]
                    }
                    (InfoPopover.open model.isOpen :: attributes)
                    |> HtmlStyled.toUnstyled
        }
        |> ProgramTest.start ()
