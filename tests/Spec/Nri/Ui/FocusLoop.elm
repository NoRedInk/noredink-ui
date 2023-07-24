module Spec.Nri.Ui.FocusLoop exposing (spec)

import Expect
import Nri.Ui.FocusLoop.V1 as FocusLoop
import Test exposing (..)


spec : Test
spec =
    describe "Nri.Ui.FocusLoop"
        [ forAnEmptyList
        ]


forAnEmptyList : Test
forAnEmptyList =
    describe "for an empty list"
        [ test "without left/right or up/down events" <|
            \() ->
                FocusLoop.addEvents
                    { toId = identity
                    , leftRight = False
                    , upDown = False
                    }
                    []
                    |> Expect.equal []
        , test "with left/right and without up/down events" <|
            \() ->
                FocusLoop.addEvents
                    { toId = identity
                    , leftRight = True
                    , upDown = False
                    }
                    []
                    |> Expect.equal []
        , test "without left/right and with up/down events" <|
            \() ->
                FocusLoop.addEvents
                    { toId = identity
                    , leftRight = False
                    , upDown = True
                    }
                    []
                    |> Expect.equal []
        , test "with left/right and up/down events" <|
            \() ->
                FocusLoop.addEvents
                    { toId = identity
                    , leftRight = True
                    , upDown = True
                    }
                    []
                    |> Expect.equal []
        ]
