module Spec.Nri.Ui.Checkbox.V4 exposing (all)

import Expect exposing (Expectation)
import Nri.Ui.Checkbox.V4 as Checkbox
import Test exposing (..)


all : Test
all =
    describe "Nri.Ui.Checkbox.V3"
        [ describe "selectedFromBool"
            [ test "it handles Selected" <|
                \() ->
                    Checkbox.selectedFromBool True
                        |> Expect.equal Checkbox.Selected
            , test "it handles NotSelected" <|
                \() ->
                    Checkbox.selectedFromBool False
                        |> Expect.equal Checkbox.NotSelected
            ]
        ]
