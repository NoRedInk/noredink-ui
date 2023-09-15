module Spec.Nri.Ui.ClickableSvg exposing (spec)

import Accessibility.Aria as Aria
import Html.Styled exposing (Html, toUnstyled)
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.UiIcon.V1 as UiIcon
import ProgramTest exposing (..)
import Spec.Helpers exposing (expectFailure)
import Test exposing (..)
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.ClickableSvg.V2"
        [ describe "helpfullyDisabledClickableSvg" helpfullyDisabledClickableSvg
        ]


helpfullyDisabledClickableSvg : List Test
helpfullyDisabledClickableSvg =
    [ test "does not have `aria-disabled=\"true\" when not disabled" <|
        \() ->
            program ()
                (\_ ->
                    ClickableSvg.button "Next"
                        UiIcon.arrowRight
                        []
                )
                |> ensureViewHasNot [ attribute (Aria.disabled True) ]
                |> done
    , test "has `aria-disabled=\"true\" when disabled" <|
        \() ->
            program ()
                (\_ ->
                    ClickableSvg.button "Next"
                        UiIcon.arrowRight
                        [ ClickableSvg.disabled True
                        ]
                )
                |> ensureViewHas [ attribute (Aria.disabled True) ]
                |> done
    , test "is clickable when not disabled" <|
        \() ->
            program ()
                (\_ ->
                    ClickableSvg.button "Next"
                        UiIcon.arrowRight
                        [ ClickableSvg.onClick ()
                        ]
                )
                |> clickButton "Next"
                |> done
    , test "is not clickable when disabled" <|
        \() ->
            program ()
                (\_ ->
                    ClickableSvg.button "Next"
                        UiIcon.arrowRight
                        [ ClickableSvg.onClick ()
                        , ClickableSvg.disabled True
                        ]
                )
                |> clickButton "Next"
                |> done
                |> expectFailure "Event.expectEvent: I found a node, but it does not listen for \"click\" events like I expected it would."
    ]


program : model -> (model -> Html model) -> ProgramTest model model ()
program init view =
    ProgramTest.createSandbox
        { init = init
        , update = \msg model -> msg
        , view = \model -> Html.Styled.div [] [ view model ] |> toUnstyled
        }
        |> ProgramTest.start ()
