module Spec.List.Zipper.Extra exposing (fromSpec)

import Expect exposing (Expectation)
import List.Zipper
import List.Zipper.Extra
import Test exposing (..)


fromSpec : Test
fromSpec =
    let
        before =
            [ 1, 2 ]

        current =
            3

        after =
            [ 4, 5 ]

        zipper =
            List.Zipper.Extra.from before current after
    in
    describe "List.Zipper.Extra.from before current after"
        [ test "before" <|
            \() ->
                Expect.equal (List.Zipper.before zipper) before
        , test "current" <|
            \() ->
                Expect.equal (List.Zipper.current zipper) current
        , test "after" <|
            \() -> Expect.equal (List.Zipper.after zipper) after
        ]
