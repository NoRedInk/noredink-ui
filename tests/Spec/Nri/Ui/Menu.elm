module Spec.Nri.Ui.Menu exposing (spec)

import Html.Attributes as Attributes
import Html.Styled as HtmlStyled
import Json.Encode as Encode
import Nri.Test.KeyboardHelpers.V1 as KeyboardHelpers
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Menu.V4 as Menu
import Nri.Ui.Tooltip.V3 as Tooltip
import ProgramTest exposing (ProgramTest, ensureViewHas, ensureViewHasNot)
import Spec.Helpers exposing (nriDescription)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (text)


spec : Test
spec =
    describe "Nri.Ui.Menu.V4"
        [ test "Opens when mouse enters" <|
            \() ->
                program [ Menu.opensOnHover True ]
                    -- Menu opens on mouse enter
                    |> mouseEnter menuButtonSelector
                    |> ensureViewHas (menuContentSelector menuContent)
                    |> mouseLeave menuInteractiveAreaSelector
                    |> ensureViewHasNot (menuContentSelector menuContent)
                    |> ProgramTest.done
        , test "Toggle when mouse clicks" <|
            \() ->
                program []
                    -- Menu opens on mouse click
                    |> clickMenuButton
                    |> ensureViewHas (menuContentSelector menuContent)
                    |> clickMenuButton
                    |> ensureViewHasNot (menuContentSelector menuContent)
                    |> ProgramTest.done
        , test "Close on tab key" <|
            \() ->
                program []
                    -- Menu opens on mouse click and closes on tab key
                    |> clickMenuButton
                    |> ensureViewHas (menuContentSelector menuContent)
                    |> pressTab { targetId = "some-random-id" }
                    |> ensureViewHasNot (menuContentSelector menuContent)
                    |> ProgramTest.done
        , test "Close on esc key" <|
            \() ->
                program []
                    -- Menu opens on mouse click and closes on tab key
                    |> clickMenuButton
                    |> ensureViewHas (menuContentSelector menuContent)
                    |> pressEsc { targetId = "some-random-id" }
                    |> ensureViewHasNot (menuContentSelector menuContent)
                    |> ProgramTest.done
        , test "Opens on down arrow" <|
            \() ->
                program []
                    |> KeyboardHelpers.pressDownArrow ProgramTest.simulateDomEvent
                        { targetDetails = targetDetails "hello-button" }
                        [ Selector.tag "button", Selector.id "hello-button" ]
                    |> ensureViewHas (menuContentSelector menuContent)
                    |> ProgramTest.done
        , test "Opens on down arrow when there's a tooltip attached" <|
            \() ->
                program [ Menu.withTooltip [ Tooltip.onToggle ToggleTooltip ] ]
                    |> KeyboardHelpers.pressDownArrow ProgramTest.simulateDomEvent
                        { targetDetails = targetDetails "hello-button" }
                        [ Selector.tag "button", Selector.id "hello-button" ]
                    |> ensureViewHas (menuContentSelector menuContent)
                    |> ProgramTest.done
        , describe "disclosure" <|
            [ test "Close on esc key" <|
                \() ->
                    program [ Menu.disclosure { lastId = "last-button" } ]
                        -- Menu opens on mouse click and closes on esc key
                        |> clickMenuButton
                        |> ensureViewHas (menuContentSelector menuContent)
                        |> pressEsc { targetId = "some-random-id" }
                        |> ensureViewHasNot (menuContentSelector menuContent)
                        |> ProgramTest.done
            , test "Closes after tab on lastId" <|
                \() ->
                    program [ Menu.disclosure { lastId = "last-button" } ]
                        |> clickMenuButton
                        |> ensureViewHas (menuContentSelector menuContent)
                        -- NOTE: unable to simulate pressTab with other targetId since those decoders will fail
                        |> pressTab { targetId = "last-button" }
                        |> ensureViewHasNot (menuContentSelector menuContent)
                        |> ProgramTest.done
            ]
        , describe "dialog" <|
            [ test "Close on esc key" <|
                \() ->
                    program [ Menu.dialog { firstId = "hello-button", lastId = "last-button" } ]
                        -- Menu opens on mouse click and closes on esc key
                        |> clickMenuButton
                        |> ensureViewHas (menuDialogContentSelector menuContent)
                        |> pressEsc { targetId = "some-random-id" }
                        |> ensureViewHasNot (menuDialogContentSelector menuContent)
                        |> ProgramTest.done
            , test "Selects firstId after tab on lastId" <|
                \() ->
                    program [ Menu.dialog { firstId = "hello-button", lastId = "last-button" } ]
                        |> clickMenuButton
                        |> ensureViewHas (menuDialogContentSelector menuContent)
                        -- NOTE: unable to simulate pressTab with other targetId since those decoders will fail
                        |> pressTab { targetId = "last-button" }
                        |> ensureViewHas (menuDialogContentSelector menuContent)
                        |> ProgramTest.done
            , test "Selects lastId after back tab on firstId" <|
                \() ->
                    program [ Menu.dialog { firstId = "hello-button", lastId = "last-button" } ]
                        |> clickMenuButton
                        |> ensureViewHas (menuDialogContentSelector menuContent)
                        -- NOTE: unable to simulate pressTab with other targetId since those decoders will fail
                        |> pressTabBack { targetId = "hello-button" }
                        |> ensureViewHas (menuDialogContentSelector menuContent)
                        |> ProgramTest.done
            ]
        ]


type alias Model =
    { isOpen : Bool }


type Msg
    = ToggleMenu Bool
    | ToggleTooltip Bool


program : List (Menu.Attribute Msg) -> ProgramTest Model Msg ()
program attributes =
    ProgramTest.createSandbox
        { init = { isOpen = False }
        , update =
            \msg model ->
                case msg of
                    ToggleMenu m ->
                        { isOpen = m }

                    ToggleTooltip _ ->
                        model
        , view =
            \model ->
                HtmlStyled.div []
                    [ Menu.view (\{ isOpen } -> ToggleMenu isOpen)
                        ([ Menu.defaultTrigger menuButton []
                         , Menu.isOpen model.isOpen
                         ]
                            ++ attributes
                        )
                        [ Menu.entry "hello-button" <|
                            \attrs ->
                                ClickableText.button menuContent [ ClickableText.custom attrs ]
                        , Menu.entry "last-button" <|
                            \attrs ->
                                ClickableText.button menuContent [ ClickableText.custom attrs ]
                        ]
                    ]
                    |> HtmlStyled.toUnstyled
        }
        |> ProgramTest.start ()


menuButton : String
menuButton =
    "Menu toggler"


menuContent : String
menuContent =
    "Hello"


menuButtonSelector : List Selector.Selector
menuButtonSelector =
    [ Selector.tag "button"
    , Selector.containing [ Selector.text menuButton ]
    ]


menuInteractiveAreaSelector : List Selector.Selector
menuInteractiveAreaSelector =
    [ nriDescription "Nri-Ui-Menu-V4"
    , Selector.class "InnerContainer"
    ]


menuContentSelector : String -> List Selector.Selector
menuContentSelector content =
    [ Selector.class "InnerContainer"
    , Selector.attribute (Attributes.attribute "aria-expanded" "true")
    , Selector.containing [ text content ]
    ]


menuDialogContentSelector : String -> List Selector.Selector
menuDialogContentSelector content =
    [ Selector.class "InnerContainer"
    , Selector.classes [ "Content", "ContentVisible" ]
    , Selector.containing [ text content ]
    ]


mouseEnter : List Selector.Selector -> ProgramTest model msg effect -> ProgramTest model msg effect
mouseEnter selectors =
    ProgramTest.simulateDomEvent (Query.find selectors) Event.mouseEnter


mouseLeave : List Selector.Selector -> ProgramTest model msg effect -> ProgramTest model msg effect
mouseLeave selectors =
    ProgramTest.simulateDomEvent (Query.find selectors) Event.mouseLeave


clickMenuButton : ProgramTest model msg effect -> ProgramTest model msg effect
clickMenuButton =
    ProgramTest.clickButton menuButton


targetDetails : String -> List ( String, Encode.Value )
targetDetails targetId =
    [ ( "id", Encode.string targetId ) ]


pressTab : { targetId : String } -> ProgramTest model msg effect -> ProgramTest model msg effect
pressTab { targetId } =
    KeyboardHelpers.pressTab ProgramTest.simulateDomEvent
        { targetDetails = targetDetails targetId }
        [ Selector.class "Container" ]


pressTabBack : { targetId : String } -> ProgramTest model msg effect -> ProgramTest model msg effect
pressTabBack { targetId } =
    KeyboardHelpers.pressTabBack ProgramTest.simulateDomEvent
        { targetDetails = targetDetails targetId }
        [ Selector.class "Container" ]


pressEsc : { targetId : String } -> ProgramTest model msg effect -> ProgramTest model msg effect
pressEsc { targetId } =
    KeyboardHelpers.pressEsc ProgramTest.simulateDomEvent
        { targetDetails = targetDetails targetId }
        [ Selector.class "Container" ]
