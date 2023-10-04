module Spec.Nri.Ui.RadioButton exposing (..)

import Accessibility.Aria as Aria
import Html.Attributes exposing (type_)
import Html.Styled exposing (..)
import Nri.Test.KeyboardHelpers.V1 as KeyboardHelpers
import Nri.Test.MouseHelpers.V1 as MouseHelpers
import Nri.Ui.RadioButton.V4 as RadioButton
import ProgramTest exposing (..)
import Spec.Helpers exposing (expectFailure)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.RadioButton.V4"
        [ describe "'role' type when enabled" hasCorrectTypeWhenEnabled
        , describe "helpfully disabled RadioButton" helpfullyDisabledRadioButton
        ]


hasCorrectTypeWhenEnabled : List Test
hasCorrectTypeWhenEnabled =
    [ test "has type 'radio'" <|
        \() ->
            program []
                |> ensureViewHas [ attribute (type_ "radio") ]
                |> done
    ]


helpfullyDisabledRadioButton : List Test
helpfullyDisabledRadioButton =
    [ test "does not have `aria-disabled=\"true\" when not disabled" <|
        \() ->
            program []
                |> ensureViewHasNot [ attribute (Aria.disabled True) ]
                |> done
    , test "does not have `aria-disabled=\"false\" when not disabled" <|
        \() ->
            program []
                |> ensureViewHasNot [ attribute (Aria.disabled False) ]
                |> done
    , test "has `aria-disabled=\"true\" when disabled" <|
        \() ->
            program [ RadioButton.disabled ]
                |> ensureViewHas [ attribute (Aria.disabled True) ]
                |> done
    , test "is clickable when not disabled" <|
        \() ->
            program []
                |> click
                |> done
    , test "is not clickable when disabled" <|
        \() ->
            program [ RadioButton.disabled ]
                |> click
                |> done
                |> expectFailure "Event.expectEvent: I found a node, but it does not listen for \"click\" events like I expected it would."
    ]


pressSpace : TestContext -> TestContext
pressSpace =
    KeyboardHelpers.pressSpace khConfig { targetDetails = [] } radio


click : TestContext -> TestContext
click =
    MouseHelpers.click mhConfig radio


radio : List Selector
radio =
    [ id "id-pets-Dogs" ]


type Selection
    = Dogs


selectionToString : Selection -> String
selectionToString selection =
    case selection of
        Dogs ->
            "Dogs"


type alias Model =
    { selectedValue : Maybe Selection
    }


init : Model
init =
    { selectedValue = Nothing
    }


type Msg
    = Select Selection


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select value ->
            { model | selectedValue = Just value }


view : List (RadioButton.Attribute Selection Msg) -> Model -> Html Msg
view attributes state =
    div []
        [ RadioButton.view
            { label = "Dogs"
            , name = "pets"
            , value = Dogs
            , selectedValue = state.selectedValue
            , valueToString = selectionToString
            }
            (RadioButton.onSelect Select :: attributes)
        ]


type alias TestContext =
    ProgramTest Model Msg ()


program : List (RadioButton.Attribute Selection Msg) -> ProgramTest Model Msg ()
program attributes =
    ProgramTest.createSandbox
        { init = init
        , update = update
        , view = view attributes >> toUnstyled
        }
        |> ProgramTest.start ()


khConfig : KeyboardHelpers.Config (ProgramTest model msg effect) Selector.Selector (Query.Single msg)
khConfig =
    { programTest_simulateDomEvent = ProgramTest.simulateDomEvent
    , query_find = Query.find
    , event_custom = Event.custom
    }


mhConfig : MouseHelpers.Config (ProgramTest model msg effect) Selector.Selector (Query.Single msg)
mhConfig =
    { programTest_simulateDomEvent = ProgramTest.simulateDomEvent
    , query_find = Query.find
    , event_click = Event.click
    , event_mouseDown = Event.mouseDown
    , event_mouseUp = Event.mouseUp
    , event_mouseOver = Event.mouseOver
    , event_custom = Event.custom
    }
