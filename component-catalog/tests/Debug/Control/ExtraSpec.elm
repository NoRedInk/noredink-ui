module Debug.Control.ExtraSpec exposing (..)

import Debug.Control.Extra exposing (rotate, rotateN)
import Expect
import List.Nonempty exposing (Nonempty(..))
import Test exposing (Test, describe, test)


spec : Test
spec =
    describe "helper functions"
        [ describe "rotate"
            [ test "works as expected" <|
                \() ->
                    rotate (Nonempty 1 [ 2, 3 ]) |> Expect.equal (Nonempty 2 [ 3, 1 ])
            , test "singleton" <|
                \() ->
                    rotate (Nonempty 1 []) |> Expect.equal (Nonempty 1 [])
            ]
        , describe "rotateN"
            [ test "1" <|
                \() ->
                    rotateN 1 (Nonempty 1 [ 2, 3, 4, 5 ]) |> Expect.equal (Nonempty 2 [ 3, 4, 5, 1 ])
            , test "2" <|
                \() ->
                    rotateN 2 (Nonempty 1 [ 2, 3, 4, 5 ]) |> Expect.equal (Nonempty 3 [ 4, 5, 1, 2 ])
            , test "5" <|
                \() ->
                    rotateN 5 (Nonempty 1 [ 2, 3, 4, 5 ]) |> Expect.equal (Nonempty 1 [ 2, 3, 4, 5 ])
            , test "7" <|
                \() ->
                    rotateN 7 (Nonempty 1 [ 2, 3, 4, 5 ]) |> Expect.equal (Nonempty 3 [ 4, 5, 1, 2 ])
            ]
        ]
