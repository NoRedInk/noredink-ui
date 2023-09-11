module Spec.Nri.Ui.Button exposing (spec)

import Accessibility.Aria as Aria
import Expect exposing (Expectation)
import Html.Styled exposing (Html, toUnstyled)
import Nri.Ui.Button.V10 as Button
import ProgramTest exposing (..)
import Test exposing (..)
import Test.Html.Selector exposing (..)
import Test.Runner


spec : Test
spec =
    describe "Nri.Ui.Button.V10"
        [ describe "toggleButtonPressed" toggleButtonPressed
        , describe "helpfullyDisabledButton" helpfullyDisabledButton
        ]


toggleButtonPressed : List Test
toggleButtonPressed =
    [ test "can be pressed" <|
        \() ->
            program { pressed = False }
                (\{ pressed } ->
                    Button.button "Italic"
                        [ Button.toggleButtonPressed pressed
                        , Button.onClick { pressed = not pressed }
                        ]
                )
                |> ensureViewHas [ attribute (Aria.pressed (Just False)) ]
                |> clickButton "Italic"
                |> ensureViewHas [ attribute (Aria.pressed (Just True)) ]
                |> clickButton "Italic"
                |> ensureViewHas [ attribute (Aria.pressed (Just False)) ]
                |> done
    ]


expectFailure : String -> Expectation -> Expectation
expectFailure expectedDescriptionSubstring expectation =
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            Expect.fail "Expected a failure, but there was none."

        Just reason ->
            String.contains expectedDescriptionSubstring reason.description
                |> Expect.equal True


helpfullyDisabledButton : List Test
helpfullyDisabledButton =
    [ test "does not have `aria-disabled=\"true\" when not disabled" <|
        \() ->
            program ()
                (\_ ->
                    Button.button "Italic"
                        []
                )
                |> ensureViewHasNot [ attribute (Aria.disabled True) ]
                |> done
    , test "has `aria-disabled=\"true\" when disabled" <|
        \() ->
            program ()
                (\_ ->
                    Button.button "Italic"
                        [ Button.disabled
                        ]
                )
                |> ensureViewHas [ attribute (Aria.disabled True) ]
                |> done
    , test "is clickable when not disabled" <|
        \() ->
            program ()
                (\_ ->
                    Button.button "Italic"
                        [ Button.onClick ()
                        ]
                )
                |> clickButton "Italic"
                |> done
    , test "is not clickable when disabled" <|
        \() ->
            program ()
                (\_ ->
                    Button.button "Italic"
                        [ Button.onClick ()
                        , Button.disabled
                        ]
                )
                |> clickButton "Italic"
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
