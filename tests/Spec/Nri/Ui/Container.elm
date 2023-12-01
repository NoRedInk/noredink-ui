module Spec.Nri.Ui.Container exposing (spec)

import Html.Styled as Html
import Nri.Ui.AssignmentIcon.V2 as AssignmentIcon
import Nri.Ui.Container.V2 as Container
import ProgramTest exposing (..)
import Spec.Helpers exposing (nriDescription)
import Test exposing (..)
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.Container.V2"
        [ describe "rendering" renderingTests
        ]


renderingTests : List Test
renderingTests =
    [ test "renders a container with text content" <|
        \() ->
            program [ Container.plaintext "hi" ]
                |> ensureViewHas [ Selector.text "hi" ]
                |> ensureViewHasNot [ Selector.tag "svg", nriDescription "top-left-icon" ]
                |> ensureViewHasNot [ Selector.tag "svg", nriDescription "top-left-icon-shadow" ]
                |> done
    , test "renders a container with a top left icon content" <|
        \() ->
            program [ Container.topLeftIcon AssignmentIcon.dailyWritingCircled ]
                |> ensureViewHas [ Selector.tag "svg", nriDescription "top-left-icon" ]
                |> ensureViewHas [ Selector.tag "svg", nriDescription "top-left-icon-shadow" ]
                |> done
    ]


type alias TestContext msg =
    ProgramTest () msg ()


program : List (Container.Attribute msg) -> TestContext msg
program attrs =
    ProgramTest.createSandbox
        { init = ()
        , update = \_ _ -> ()
        , view = \() -> Container.view attrs |> Html.toUnstyled
        }
        |> ProgramTest.start ()
