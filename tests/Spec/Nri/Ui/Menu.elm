module Spec.Nri.Ui.Menu exposing (spec)

import Html.Attributes as Attributes
import Html.Styled as HtmlStyled
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Menu.V3 as Menu
import ProgramTest exposing (ProgramTest, ensureViewHas, ensureViewHasNot)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (text)


spec : Test
spec =
    describe "Nri.Ui.Menu.V3"
        [ test "Menu.view" <|
            \() ->
                let
                    menuContent =
                        "Hello"
                in
                program [ Menu.opensOnHover True ]
                    -- Menu opens on mouse enter
                    |> mouseEnter menuButtonSelector
                    |> ensureViewHas (menuContentSelector menuContent)
                    |> mouseLeave menuInteractiveAreaSelector
                    |> ensureViewHasNot (menuContentSelector menuContent)
                    |> ProgramTest.done
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
                        { button = Menu.button [] "Menu"
                        , isOpen = model.isOpen
                        , entries =
                            [ Menu.entry "hello-button" <|
                                \attrs ->
                                    ClickableText.button "Hello" [ ClickableText.custom attrs ]
                            ]
                        , focusAndToggle = \{ isOpen } -> isOpen
                        }
                    ]
                    |> HtmlStyled.toUnstyled
        }
        |> ProgramTest.start ()


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
menuContentSelector menuContent =
    [ Selector.class "InnerContainer"
    , Selector.attribute (Attributes.attribute "aria-expanded" "true")
    , Selector.containing [ text menuContent ]
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
