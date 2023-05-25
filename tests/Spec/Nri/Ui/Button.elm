module Spec.Nri.Ui.Button exposing (spec)

import Accessibility.Aria as Aria
import Html.Styled exposing (Html, toUnstyled)
import Nri.Ui.Button.V10 as Button
import ProgramTest exposing (..)
import Test exposing (..)
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.Button.V10"
        [ describe "toggleButtonPressed" toggleButtonPressed
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


program : model -> (model -> Html model) -> ProgramTest model model ()
program init view =
    ProgramTest.createSandbox
        { init = init
        , update = \msg model -> msg
        , view = \model -> Html.Styled.div [] [ view model ] |> toUnstyled
        }
        |> ProgramTest.start ()
