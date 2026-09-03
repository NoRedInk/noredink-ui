module Spec.Nri.Ui.HintTooltip exposing (spec)

import Accessibility.Aria as Aria
import Accessibility.Styled.Key as Key
import Expect
import Html.Attributes as UnstyledAttributes
import Html.Styled as HtmlStyled
import Html.Styled.Attributes as Attrs
import Nri.Test.KeyboardHelpers.V1 as KeyboardHelpers
import Nri.Ui.HintTooltip.V1 as HintTooltip
import ProgramTest exposing (ProgramTest, ensureViewHas)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (id, text)


spec : Test
spec =
    describe "Nri.Ui.HintTooltip.V1"
        [ test "primaryLabel keeps the tooltip hidden from assistive tech" <|
            \() ->
                HtmlStyled.div []
                    [ HintTooltip.view
                        { trigger = \events -> HtmlStyled.button (Attrs.id "hint-primary-trigger" :: events) [ HtmlStyled.text "Print" ]
                        , id = "hint-primary"
                        }
                        [ HintTooltip.primaryLabel
                        , HintTooltip.plaintext "Print"
                        , HintTooltip.open True
                        ]
                    ]
                    |> HtmlStyled.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.hasNot [ Selector.tag "button", Selector.attribute (Aria.describedBy [ "hint-primary" ]) ]
                        , Query.has
                            [ Selector.tag "nri-anchored-overlay-v1"
                            , id "hint-primary"
                            , Selector.attribute (UnstyledAttributes.attribute "aria-hidden" "true")
                            , Selector.attribute (UnstyledAttributes.attribute "role" "tooltip")
                            ]
                        ]
        , test "auxiliaryDescription and helpfullyDisabled wire aria-describedby" <|
            \() ->
                HtmlStyled.div []
                    [ HintTooltip.view
                        { trigger = \events -> HtmlStyled.button (Attrs.id "hint-aux-trigger" :: events) [ HtmlStyled.text "Save" ]
                        , id = "hint-aux"
                        }
                        [ HintTooltip.auxiliaryDescription
                        , HintTooltip.plaintext "Preview only"
                        ]
                    , HintTooltip.view
                        { trigger = \events -> HtmlStyled.button (Attrs.id "hint-disabled-trigger" :: events) [ HtmlStyled.text "Submit" ]
                        , id = "hint-disabled"
                        }
                        [ HintTooltip.helpfullyDisabled
                        , HintTooltip.plaintext "Answer the previous question first"
                        ]
                    ]
                    |> HtmlStyled.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ Selector.tag "button", Selector.containing [ text "Save" ], Selector.attribute (Aria.describedBy [ "hint-aux" ]) ]
                        , Query.has [ Selector.tag "button", Selector.containing [ text "Submit" ], Selector.attribute (Aria.describedBy [ "hint-disabled" ]) ]
                        ]
        , test "mouse enter opens the tooltip and reflects open state in the DOM" <|
            \() ->
                program
                    [ HintTooltip.plaintext "Tooltip content"
                    , HintTooltip.onToggle Toggle
                    ]
                    |> mouseEnter [ Selector.attribute (UnstyledAttributes.attribute "data-nri-description" "Nri-Ui-HintTooltip-V1") ]
                    |> ensureViewHas [ Selector.tag "nri-anchored-overlay-v1", Selector.attribute (UnstyledAttributes.attribute "open" "") ]
                    |> ProgramTest.expectModel (\model -> Expect.equal ( Just (Toggle True), True ) ( model.lastMsg, model.isOpen ))
        , test "blur closes the tooltip after focus opens it" <|
            \() ->
                program
                    [ HintTooltip.plaintext "Tooltip content"
                    , HintTooltip.onToggle Toggle
                    ]
                    |> focus [ Selector.tag "button", Selector.containing [ text "Info" ] ]
                    |> blur [ Selector.tag "button", Selector.containing [ text "Info" ] ]
                    |> ProgramTest.expectModel (\model -> Expect.equal (Just (Toggle False)) model.lastMsg)
        , test "Escape closes the tooltip" <|
            \() ->
                program
                    [ HintTooltip.plaintext "Tooltip content"
                    , HintTooltip.onToggle Toggle
                    ]
                    |> focus [ Selector.tag "button", Selector.containing [ text "Info" ] ]
                    |> KeyboardHelpers.pressEsc { targetDetails = [] } [ Selector.tag "button", Selector.containing [ text "Info" ] ]
                    |> ProgramTest.expectModel (\model -> Expect.equal (Just (Toggle False)) model.lastMsg)
        , test "onTriggerKeyDown adds additional key handlers" <|
            \() ->
                program
                    [ HintTooltip.plaintext "Tooltip content"
                    , HintTooltip.onToggle Toggle
                    , HintTooltip.onTriggerKeyDown [ Key.space SpaceKeyPressed ]
                    ]
                    |> KeyboardHelpers.pressSpace { targetDetails = [] } [ Selector.tag "button", Selector.containing [ text "Info" ] ]
                    |> ProgramTest.expectModel (\model -> Expect.equal (Just SpaceKeyPressed) model.lastMsg)
        ]


type alias Model =
    { isOpen : Bool
    , lastMsg : Maybe Msg
    }


type Msg
    = Toggle Bool
    | SpaceKeyPressed


program : List (HintTooltip.Attribute Msg) -> ProgramTest Model Msg ()
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

                            SpaceKeyPressed ->
                                model
                in
                { nextModel | lastMsg = Just msg }
        , view =
            \model ->
                HintTooltip.view
                    { trigger = \events -> HtmlStyled.button (Attrs.id "interactive-hint-trigger" :: Attrs.type_ "button" :: events) [ HtmlStyled.text "Info" ]
                    , id = "interactive-hint"
                    }
                    (HintTooltip.open model.isOpen :: attributes)
                    |> HtmlStyled.toUnstyled
        }
        |> ProgramTest.start ()


mouseEnter : List Selector.Selector -> ProgramTest model msg effect -> ProgramTest model msg effect
mouseEnter selectors =
    ProgramTest.simulateDomEvent (Query.find selectors) Event.mouseEnter


focus : List Selector.Selector -> ProgramTest model msg effect -> ProgramTest model msg effect
focus selectors =
    ProgramTest.simulateDomEvent (Query.find selectors) Event.focus


blur : List Selector.Selector -> ProgramTest model msg effect -> ProgramTest model msg effect
blur selectors =
    ProgramTest.simulateDomEvent (Query.find selectors) Event.blur
