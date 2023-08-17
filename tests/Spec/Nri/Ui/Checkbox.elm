module Spec.Nri.Ui.Checkbox exposing (..)

import Accessibility.Aria as Aria
import Accessibility.Role as Role
import Html.Styled exposing (..)
import InputErrorAndGuidanceInternal exposing (guidanceId)
import Nri.Ui.Checkbox.V7 as Checkbox
import ProgramTest exposing (..)
import Spec.KeyboardHelpers as KeyboardHelpers
import Test exposing (..)
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.Checkbox.V7"
        [ describe "'checkbox' role" hasCorrectRole
        , describe "aria-checked" hasAriaChecked
        , test "guidance" <|
            \() ->
                let
                    checkboxId =
                        "custom-checkbox-id"
                in
                program
                    [ Checkbox.id checkboxId
                    , Checkbox.guidance "Some guidance"
                    ]
                    |> ensureViewHas
                        [ id checkboxId
                        , attribute (Aria.describedBy [ guidanceId checkboxId ])
                        ]
                    |> done
        ]


hasCorrectRole : List Test
hasCorrectRole =
    [ test "has role checkbox" <|
        \() ->
            program []
                |> ensureViewHas [ attribute Role.checkBox ]
                |> done
    ]


hasAriaChecked : List Test
hasAriaChecked =
    [ test "aria-checked reflects the state of the checkbox" <|
        \() ->
            program []
                |> ensureViewHas [ attribute (Aria.checked (Just False)) ]
                |> pressSpace
                |> ensureViewHas [ attribute (Aria.checked (Just True)) ]
                |> pressSpace
                |> ensureViewHas [ attribute (Aria.checked (Just False)) ]
                |> done
    ]


pressSpace : TestContext -> TestContext
pressSpace =
    KeyboardHelpers.pressSpaceKey { targetDetails = [] }
        [ attribute Role.checkBox ]


type alias Model =
    { checked : Checkbox.IsSelected
    }


init : Model
init =
    { checked = Checkbox.NotSelected
    }


type Msg
    = Toggle Bool


update : Msg -> Model -> Model
update msg state =
    case msg of
        Toggle checked ->
            let
                checked_ =
                    if checked then
                        Checkbox.Selected

                    else
                        Checkbox.NotSelected
            in
            { state | checked = checked_ }


view : List (Checkbox.Attribute Msg) -> Model -> Html Msg
view attributes state =
    Checkbox.view
        { label = "Checkbox"
        , selected = state.checked
        }
        (Checkbox.onCheck Toggle :: attributes)


type alias TestContext =
    ProgramTest Model Msg ()


program : List (Checkbox.Attribute Msg) -> TestContext
program attributes =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = view attributes >> toUnstyled
        }
        |> ProgramTest.start ()
