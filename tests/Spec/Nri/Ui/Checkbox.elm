module Spec.Nri.Ui.Checkbox exposing (..)

import Accessibility.Aria as Aria
import Accessibility.Role as Role
import Expect
import Html.Styled exposing (..)
import Nri.Ui.Checkbox.V7 as Checkbox
import ProgramTest exposing (..)
import Spec.KeyboardHelpers as KeyboardHelpers
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.Checkbox.V7"
        [ describe "'checkbox' role" hasCorrectRole
        , describe "aria-checked" hasAriaChecked
        ]


hasCorrectRole : List Test
hasCorrectRole =
    [ test "has role checkbox" <|
        \() ->
            program
                |> ensureHasRoleCheckbox
                |> done
    ]


hasAriaChecked : List Test
hasAriaChecked =
    [ test "aria-checked reflects the state of the checkbox" <|
        \() ->
            program
                |> ensureAriaCheckedIsFalse
                |> pressSpace
                |> ensureAriaCheckedIsTrue
                |> pressSpace
                |> ensureAriaCheckedIsFalse
                |> done
    ]


ensureHasRoleCheckbox : TestContext -> TestContext
ensureHasRoleCheckbox context =
    context
        |> ensureView
            (Query.findAll [ Selector.attribute Role.checkBox ]
                >> Query.count (Expect.equal 1)
            )


ensureAriaCheckedIsFalse : TestContext -> TestContext
ensureAriaCheckedIsFalse context =
    context
        |> ensureView
            (Query.findAll [ Selector.attribute (Aria.checked (Just False)) ]
                >> Query.count (Expect.equal 1)
            )


ensureAriaCheckedIsTrue : TestContext -> TestContext
ensureAriaCheckedIsTrue context =
    context
        |> ensureView
            (Query.findAll [ Selector.attribute (Aria.checked (Just True)) ]
                >> Query.count (Expect.equal 1)
            )


pressSpace : TestContext -> TestContext
pressSpace =
    KeyboardHelpers.pressSpaceKey { targetDetails = [] }
        [ Selector.attribute Role.checkBox ]


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


view : Model -> Html Msg
view state =
    Checkbox.view
        { label = "Checkbox"
        , selected = state.checked
        }
        [ Checkbox.onCheck Toggle
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
