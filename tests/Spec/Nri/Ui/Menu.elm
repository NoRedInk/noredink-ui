module Spec.Nri.Ui.Menu exposing (spec)

import Html.Attributes as Attributes
import Html.Styled as HtmlStyled
import Json.Encode as Encode
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Menu.V3 as Menu
import ProgramTest exposing (ProgramTest, ensureViewHas, ensureViewHasNot)
import Spec.KeyboardHelpers as KeyboardHelpers
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (text)


spec : Test
spec =
    describe "Nri.Ui.Menu.V3"
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
                    |> pressTabKey { targetId = Nothing }
                    |> ensureViewHasNot (menuContentSelector menuContent)
                    |> ProgramTest.done
        , test "Close on esc key" <|
            \() ->
                program []
                    -- Menu opens on mouse click and closes on tab key
                    |> clickMenuButton
                    |> ensureViewHas (menuContentSelector menuContent)
                    |> pressEscKey { targetId = Nothing }
                    |> ensureViewHasNot (menuContentSelector menuContent)
                    |> ProgramTest.done
        , describe "disclosure" <|
            [ test "Close on esc key" <|
                \() ->
                    program [ Menu.disclosure { lastId = "last-button" } ]
                        -- Menu opens on mouse click and closes on esc key
                        |> clickMenuButton
                        |> ensureViewHas (menuContentSelector menuContent)
                        |> pressEscKey { targetId = Nothing }
                        |> ensureViewHasNot (menuContentSelector menuContent)
                        |> ProgramTest.done
            , test "Closes after tab on lastId" <|
                \() ->
                    program [ Menu.disclosure { lastId = "last-button" } ]
                        |> clickMenuButton
                        |> ensureViewHas (menuContentSelector menuContent)
                        -- NOTE: unable to simulate pressTabKey with other targetId since those decoders will fail
                        |> pressTabKey { targetId = Just "last-button" }
                        |> ensureViewHasNot (menuContentSelector menuContent)
                        |> ProgramTest.done
            ]
        , describe "dialog" <|
            [ test "Close on esc key" <|
                \() ->
                    program [ Menu.dialog { firstId = "hello-button", lastId = "last-button" } ]
                        -- Menu opens on mouse click and closes on esc key
                        |> clickMenuButton
                        |> ensureViewHas (menuContentSelector menuContent)
                        |> pressEscKey { targetId = Nothing }
                        |> ensureViewHasNot (menuContentSelector menuContent)
                        |> ProgramTest.done
            , test "Selects firstId after tab on lastId" <|
                \() ->
                    program [ Menu.dialog { firstId = "hello-button", lastId = "last-button" } ]
                        |> clickMenuButton
                        |> ensureViewHas (menuContentSelector menuContent)
                        -- NOTE: unable to simulate pressTabKey with other targetId since those decoders will fail
                        |> pressTabKey { targetId = Just "last-button" }
                        |> ensureViewHas (menuContentSelector menuContent)
                        |> ProgramTest.done
            , test "Selects lastId after back tab on firstId" <|
                \() ->
                    program [ Menu.dialog { firstId = "hello-button", lastId = "last-button" } ]
                        |> clickMenuButton
                        |> ensureViewHas (menuContentSelector menuContent)
                        -- NOTE: unable to simulate pressTabKey with other targetId since those decoders will fail
                        |> pressTabBackKey { targetId = Just "hello-button" }
                        |> ensureViewHas (menuContentSelector menuContent)
                        |> ProgramTest.done
            ]
        ]


type alias Model =
    { isOpen : Bool }


type alias Msg =
    Bool


program : List (Menu.Attribute Msg) -> ProgramTest Model Msg ()
program attributes =
    ProgramTest.createSandbox
        { init = { isOpen = False }
        , update = \msg _ -> { isOpen = msg }
        , view =
            \model ->
                HtmlStyled.div []
                    [ Menu.view attributes
                        { button = Menu.button [] menuButton
                        , isOpen = model.isOpen
                        , entries =
                            [ Menu.entry "hello-button" <|
                                \attrs ->
                                    ClickableText.button menuContent [ ClickableText.custom attrs ]
                            , Menu.entry "last-button" <|
                                \attrs ->
                                    ClickableText.button menuContent [ ClickableText.custom attrs ]
                            ]
                        , focusAndToggle = \{ isOpen } -> isOpen
                        }
                    ]
                    |> HtmlStyled.toUnstyled
        }
        |> ProgramTest.start ()


menuButton : String
menuButton =
    "Menu"


menuContent : String
menuContent =
    "Hello"


menuButtonSelector : List Selector.Selector
menuButtonSelector =
    [ nriDescription "Nri-Ui-Menu-V3"
    , Selector.class "ToggleButton"
    ]


menuInteractiveAreaSelector : List Selector.Selector
menuInteractiveAreaSelector =
    [ nriDescription "Nri-Ui-Menu-V3"
    , Selector.class "InnerContainer"
    ]


menuContentSelector : String -> List Selector.Selector
menuContentSelector content =
    [ Selector.class "InnerContainer"
    , Selector.attribute (Attributes.attribute "aria-expanded" "true")
    , Selector.containing [ text content ]
    ]


nriDescription : String -> Selector.Selector
nriDescription desc =
    Selector.attribute (Attributes.attribute "data-nri-description" desc)


mouseEnter : List Selector.Selector -> ProgramTest model msg effect -> ProgramTest model msg effect
mouseEnter selectors =
    ProgramTest.simulateDomEvent (Query.find selectors) Event.mouseEnter


mouseLeave : List Selector.Selector -> ProgramTest model msg effect -> ProgramTest model msg effect
mouseLeave selectors =
    ProgramTest.simulateDomEvent (Query.find selectors) Event.mouseLeave


clickMenuButton : ProgramTest model msg effect -> ProgramTest model msg effect
clickMenuButton =
    ProgramTest.simulateDomEvent
        (Query.find
            [ Selector.class "ToggleButton"
            ]
        )
        Event.click


targetDetails : Maybe String -> List ( String, Encode.Value )
targetDetails targetId =
    List.filterMap identity <|
        [ targetId
            |> Maybe.map (\id -> ( "id", Encode.string id ))
        ]


pressTabKey : { targetId : Maybe String } -> ProgramTest model msg effect -> ProgramTest model msg effect
pressTabKey { targetId } =
    KeyboardHelpers.pressTabKey
        { targetDetails = targetDetails targetId }
        [ Selector.class "Container" ]


pressTabBackKey : { targetId : Maybe String } -> ProgramTest model msg effect -> ProgramTest model msg effect
pressTabBackKey { targetId } =
    KeyboardHelpers.pressTabBackKey
        { targetDetails = targetDetails targetId }
        [ Selector.class "Container" ]


pressEscKey : { targetId : Maybe String } -> ProgramTest model msg effect -> ProgramTest model msg effect
pressEscKey { targetId } =
    KeyboardHelpers.pressEscKey
        { targetDetails = targetDetails targetId }
        [ Selector.class "Container" ]
