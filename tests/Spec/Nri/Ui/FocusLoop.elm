module Spec.Nri.Ui.FocusLoop exposing (spec)

import Accessibility.Styled.Key exposing (Event)
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
        (allCases []
            { noEvents = []
            , justLeftRight = []
            , justUpDown = []
            , allEvents = []
            }
        )


allCases :
    List String
    ->
        { noEvents : List ( String, List (Event msg) )
        , justLeftRight : List ( String, List (Event msg) )
        , justUpDown : List ( String, List (Event msg) )
        , allEvents : List ( String, List (Event msg) )
        }
    -> List Test
allCases startingList expected =
    [ test "without left/right or up/down events" <|
        \() ->
            FocusLoop.addEvents
                { toId = identity
                , leftRight = False
                , upDown = False
                }
                startingList
                |> Expect.equal expected.noEvents
    , test "with left/right and without up/down events" <|
        \() ->
            FocusLoop.addEvents
                { toId = identity
                , leftRight = True
                , upDown = False
                }
                startingList
                |> Expect.equal expected.justLeftRight
    , test "without left/right and with up/down events" <|
        \() ->
            FocusLoop.addEvents
                { toId = identity
                , leftRight = False
                , upDown = True
                }
                startingList
                |> Expect.equal expected.justUpDown
    , test "with left/right and up/down events" <|
        \() ->
            FocusLoop.addEvents
                { toId = identity
                , leftRight = True
                , upDown = True
                }
                startingList
                |> Expect.equal expected.allEvents
    ]
