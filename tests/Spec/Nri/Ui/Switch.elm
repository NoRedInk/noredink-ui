module Spec.Nri.Ui.Switch exposing (..)

import Accessibility.Aria as Aria
import Accessibility.Role as Role
import Html.Styled exposing (..)
import Nri.Test.KeyboardHelpers.V1 as KeyboardHelpers
import Nri.Test.MouseHelpers.V1 as MouseHelpers
import Nri.Ui.Switch.V3 as Switch
import ProgramTest exposing (..)
import Spec.Helpers exposing (expectFailure)
import Test exposing (..)
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.Switch.V2"
        [ describe "'switch' role" hasCorrectRole
        , describe "helpfully disabled switch" helpfullyDisabledSwitch
        ]


hasCorrectRole : List Test
hasCorrectRole =
    [ test "has role 'switch'" <|
        \() ->
            program []
                |> ensureViewHas [ attribute Role.switch ]
                |> done
    ]


helpfullyDisabledSwitch : List Test
helpfullyDisabledSwitch =
    [ test "does not have `aria-disabled=\"true\" when not disabled" <|
        \() ->
            program []
                |> ensureViewHasNot [ attribute (Aria.disabled True) ]
                |> done
    , test "has `aria-disabled=\"true\" when disabled" <|
        \() ->
            program [ Switch.disabled True ]
                |> ensureViewHas [ attribute (Aria.disabled True) ]
                |> done
    , test "is clickable when not disabled" <|
        \() ->
            program []
                |> click
                |> done
    , test "is not clickable when disabled" <|
        \() ->
            program
                [ Switch.disabled True
                ]
                |> click
                |> done
                |> expectFailure "Event.expectEvent: I found a node, but it does not listen for \"click\" events like I expected it would."
    , test "allows pressing space when not disabled" <|
        \() ->
            program
                []
                |> pressSpace
                |> done
    , test "does not allow pressing space when disabled" <|
        \() ->
            program
                [ Switch.disabled True
                ]
                |> pressSpace
                |> done
                |> expectFailure "Event.expectEvent: I found a node, but it does not listen for \"keydown\" events like I expected it would."
    ]


pressSpace : TestContext -> TestContext
pressSpace =
    KeyboardHelpers.pressSpace ProgramTest.simulateDomEvent { targetDetails = [] } switch


click : TestContext -> TestContext
click =
    MouseHelpers.click ProgramTest.simulateDomEvent switch


switch : List Selector
switch =
    [ attribute Role.switch ]


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
            { state | selected = not selected }


view : List (Switch.Attribute Msg) -> Model -> Html Msg
view attributes state =
    div []
        [ Switch.view
            { id = "switch"
            , label = "Switch"
            }
            (Switch.selected state.selected :: Switch.onSwitch Toggle :: attributes)
        ]


type alias TestContext =
    ProgramTest Model Msg ()


program : List (Switch.Attribute Msg) -> TestContext
program attributes =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = view attributes >> toUnstyled
        }
        |> ProgramTest.start ()
