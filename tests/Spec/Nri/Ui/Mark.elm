module Spec.Nri.Ui.Mark exposing (..)

import Expect
import Nri.Ui.Mark.V6 as Mark
import Test exposing (..)


tests : Test
tests =
    describe "Nri.Ui.Mark"
        [ describe "overlappingStyles"
            [ test "with no marks" <|
                \_ ->
                    [ ( 1, [] )
                    , ( 2, [] )
                    , ( 3, [] )
                    ]
                        |> Mark.overlappingStyles
                        |> Expect.equalLists
                            [ ( 1, Nothing, [] )
                            , ( 2, Nothing, [] )
                            , ( 3, Nothing, [] )
                            ]
            ]
        ]
