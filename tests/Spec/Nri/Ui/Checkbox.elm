module Spec.Nri.Ui.Checkbox exposing (..)

import Accessibility.Aria as Aria
import Accessibility.Role as Role
import Html.Styled exposing (..)
import InputErrorAndGuidanceInternal exposing (guidanceId)
import Nri.Test.KeyboardHelpers.V1 as KeyboardHelpers
import Nri.Ui.Checkbox.V7 as Checkbox
import ProgramTest exposing (..)
import Spec.Helpers exposing (expectFailure)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.Checkbox.V7"
        [ describe "'checkbox' role" hasCorrectRole
        , describe "state" stateSpec
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
        , describe "helpfullyDisabledCheckbox" helpfullyDisabledCheckbox
        ]


hasCorrectRole : List Test
hasCorrectRole =
    [ test "has role checkbox" <|
        \() ->
            program []
                |> ensureViewHas [ attribute Role.checkBox ]
                |> done
    , test "has role checkbox when label is hidden" <|
        \() ->
            program [ Checkbox.hiddenLabel ]
                |> ensureViewHas [ attribute Role.checkBox ]
                |> done
    ]


stateSpec : List Test
stateSpec =
    [ test "checkbox works when the label is visible and the keyboard is used to change state" <|
        \() ->
            program []
                |> ensureViewHas [ attribute (Aria.checked (Just False)) ]
                |> pressSpace
                |> ensureViewHas [ attribute (Aria.checked (Just True)) ]
                |> pressSpace
                |> ensureViewHas [ attribute (Aria.checked (Just False)) ]
                |> done
    , test "checkbox works when the label is hidden and the keyboard is used to change state" <|
        \() ->
            program [ Checkbox.hiddenLabel ]
                |> ensureViewHas [ attribute (Aria.checked (Just False)) ]
                |> pressSpace
                |> ensureViewHas [ attribute (Aria.checked (Just True)) ]
                |> pressSpace
                |> ensureViewHas [ attribute (Aria.checked (Just False)) ]
                |> done
    , test "checkbox works when the label is visible and the mouse is used to change state" <|
        \() ->
            program []
                |> ensureViewHas [ attribute (Aria.checked (Just False)) ]
                |> clickIt
                |> ensureViewHas [ attribute (Aria.checked (Just True)) ]
                |> clickIt
                |> ensureViewHas [ attribute (Aria.checked (Just False)) ]
                |> done
    , test "checkbox works when the label is hidden and the mouse is used to change state" <|
        \() ->
            program [ Checkbox.hiddenLabel ]
                |> ensureViewHas [ attribute (Aria.checked (Just False)) ]
                |> clickIt
                |> ensureViewHas [ attribute (Aria.checked (Just True)) ]
                |> clickIt
                |> ensureViewHas [ attribute (Aria.checked (Just False)) ]
                |> done
    ]


helpfullyDisabledCheckbox : List Test
helpfullyDisabledCheckbox =
    [ test "does not have `aria-disabled=\"true\" when not disabled" <|
        \() ->
            program []
                |> ensureViewHasNot [ attribute (Aria.disabled True) ]
                |> done
    , test "has `aria-disabled=\"true\" when disabled" <|
        \() ->
            program [ Checkbox.disabled ]
                |> ensureViewHas [ attribute (Aria.disabled True) ]
                |> done
    , test "is clickable when not disabled" <|
        \() ->
            program []
                |> clickIt
                |> done
    , test "is not clickable when disabled" <|
        \() ->
            program
                [ Checkbox.disabled
                ]
                |> clickIt
                |> done
                |> expectFailure "Event.expectEvent: I found a node, but it does not listen for \"click\" events like I expected it would."
    ]


pressSpace : TestContext -> TestContext
pressSpace =
    KeyboardHelpers.pressSpace { targetDetails = [] } checkbox


clickIt : TestContext -> TestContext
clickIt =
    simulateDomEvent (Query.find checkbox) Event.click


checkbox : List Selector
checkbox =
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
