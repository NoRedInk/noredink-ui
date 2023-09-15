module Spec.Nri.Ui.ClickableText exposing (spec)

import Accessibility.Aria as Aria
import Html.Styled exposing (Html, toUnstyled)
import Nri.Ui.ClickableText.V3 as ClickableText
import ProgramTest exposing (..)
import Spec.Helpers exposing (expectFailure)
import Test exposing (..)
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.ClickableText.V3"
        [ describe "helpfullyDisabledClickableText" helpfullyDisabledClickableText
        ]


helpfullyDisabledClickableText : List Test
helpfullyDisabledClickableText =
    [ test "does not have `aria-disabled=\"true\" when not disabled" <|
        \() ->
            program ()
                (\_ ->
                    ClickableText.button "Text"
                        []
                )
                |> ensureViewHasNot [ attribute (Aria.disabled True) ]
                |> done
    , test "has `aria-disabled=\"true\" when disabled" <|
        \() ->
            program ()
                (\_ ->
                    ClickableText.button "Text"
                        [ ClickableText.disabled True
                        ]
                )
                |> ensureViewHas [ attribute (Aria.disabled True) ]
                |> done
    , test "is clickable when not disabled" <|
        \() ->
            program ()
                (\_ ->
                    ClickableText.button "Text"
                        [ ClickableText.onClick ()
                        ]
                )
                |> clickButton "Text"
                |> done
    , test "is not clickable when disabled" <|
        \() ->
            program ()
                (\_ ->
                    ClickableText.button "Text"
                        [ ClickableText.onClick ()
                        , ClickableText.disabled True
                        ]
                )
                |> clickButton "Text"
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
