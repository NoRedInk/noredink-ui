module Spec.Nri.Ui.Switch exposing (..)

import Accessibility.Aria as Aria
import Accessibility.Role as Role
import Html.Styled exposing (..)
import Nri.Ui.Switch.V2 as Switch
import ProgramTest exposing (..)
import Test exposing (..)
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.Switch.V2"
        [ describe "'switch' role" hasCorrectRole
        , describe "has aria-disabled=true when disabled" hasCorrectAriaDisabled
        ]


hasCorrectRole : List Test
hasCorrectRole =
    [ test "has role 'switch'" <|
        \() ->
            program
                |> ensureViewHas [ Selector.attribute Role.switch ]
                |> done
    ]


hasCorrectAriaDisabled : List Test
hasCorrectAriaDisabled =
    [ test "has 'aria-disabled=true' when disabled" <|
        \() ->
            programDisabled
                |> ensureViewHas [ Selector.attribute (Aria.disabled True) ]
                |> done
    ]


type alias Model =
    { selected : Bool
    }


init : Model
init =
    { selected = False
    }


type Msg
    = Toggle Bool


update : Msg -> Model -> Model
update msg state =
    case msg of
        Toggle selected ->
            let
                selected_ =
                    if selected then
                        False

                    else
                        True
            in
            { state | selected = selected_ }


view : Model -> Html Msg
view model =
    Switch.view { id = "switch", label = "Switch" }
        [ Switch.selected model.selected
        , Switch.onSwitch Toggle
        ]


viewDisabled : Model -> Html Msg
viewDisabled model =
    Switch.view { id = "switch", label = "Switch" }
        [ Switch.selected model.selected
        , Switch.onSwitch Toggle
        , Switch.disabled True
        ]


type alias TestContext =
    ProgramTest Model Msg ()


program : TestContext
program =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = view >> toUnstyled
        }
        |> ProgramTest.start ()


programDisabled : TestContext
programDisabled =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = viewDisabled >> toUnstyled
        }
        |> ProgramTest.start ()
