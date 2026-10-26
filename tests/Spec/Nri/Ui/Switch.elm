module Spec.Nri.Ui.Switch exposing (..)

import Accessibility.Aria as Aria
import Accessibility.Role as Role
import Html.Styled exposing (..)
import InputErrorAndGuidanceInternal exposing (guidanceId)
import Nri.Test.KeyboardHelpers.V1 as KeyboardHelpers
import Nri.Test.MouseHelpers.V1 as MouseHelpers
import Nri.Ui.Switch.V4 as Switch
import ProgramTest exposing (..)
import Spec.Helpers exposing (expectFailure)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.Switch.V4"
        [ describe "'switch' role" hasCorrectRole
        , describe "helpfully disabled switch" helpfullyDisabledSwitch
        , describe "guidance" guidanceSpec
        ]


guidanceSpec : List Test
guidanceSpec =
    [ test "does not render guidance or aria-describedby when there's no guidance" <|
        \() ->
            program []
                |> ensureViewHasNot [ id (guidanceId switchId) ]
                |> ensureViewHasNot [ attribute (Aria.describedBy [ guidanceId switchId ]) ]
                |> done
    , test "renders guidance and describes the switch with it" <|
        \() ->
            program [ Switch.guidance "Some guidance" ]
                |> ensureViewHas
                    [ id (guidanceId switchId)
                    , containing [ Test.Html.Selector.text "Some guidance" ]
                    ]
                |> ensureViewHas
                    [ id switchId
                    , attribute Role.switch
                    , attribute (Aria.describedBy [ guidanceId switchId ])
                    ]
                |> done
    , test "renders html guidance and describes the switch with it" <|
        \() ->
            program [ Switch.guidanceHtml [ b [] [ Html.Styled.text "Bold guidance" ] ] ]
                |> ensureViewHas
                    [ id (guidanceId switchId)
                    , containing [ tag "b", containing [ Test.Html.Selector.text "Bold guidance" ] ]
                    ]
                |> ensureViewHas
                    [ id switchId
                    , attribute (Aria.describedBy [ guidanceId switchId ])
                    ]
                |> done
    , test "does not render guidance inside the switch, so it is not part of the accessible name" <|
        \() ->
            program [ Switch.guidance "Some guidance" ]
                |> ensureView
                    (Query.find [ attribute Role.switch ]
                        >> Query.hasNot [ id (guidanceId switchId) ]
                    )
                |> done
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
    KeyboardHelpers.pressSpace { targetDetails = [] } switch


click : TestContext -> TestContext
click =
    MouseHelpers.click switch


switch : List Selector
switch =
    [ attribute Role.switch ]


switchId : String
switchId =
    "switch"


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
            { id = switchId
            , label = Html.Styled.text "Switch"
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
