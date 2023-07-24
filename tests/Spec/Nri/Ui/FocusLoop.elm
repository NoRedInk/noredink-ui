module Spec.Nri.Ui.FocusLoop exposing (spec)

import Accessibility.Styled.Key as Key exposing (Event)
import Expect
import Nri.Ui.FocusLoop.V1 as FocusLoop
import Test exposing (..)


spec : Test
spec =
    describe "Nri.Ui.FocusLoop"
        [ forAnEmptyList
        , forASingletonList
        , forATwoElementList
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


forASingletonList : Test
forASingletonList =
    describe "for a singleton list"
        (allCases [ "a" ]
            { noEvents = [ ( "a", [] ) ]
            , justLeftRight = [ ( "a", [] ) ]
            , justUpDown = [ ( "a", [] ) ]
            , allEvents = [ ( "a", [] ) ]
            }
        )


forATwoElementList : Test
forATwoElementList =
    describe "for a two-element list"
        (allCases [ "a", "b" ]
            { noEvents = [ ( "a", [] ), ( "b", [] ) ]
            , justLeftRight =
                [ ( "a", [ Key.right "b", Key.left "b" ] )
                , ( "b", [ Key.right "a", Key.left "a" ] )
                ]
            , justUpDown =
                [ ( "a", [ Key.down "b", Key.up "b" ] )
                , ( "b", [ Key.down "a", Key.up "a" ] )
                ]
            , allEvents =
                [ ( "a"
                  , [ Key.right "b"
                    , Key.left "b"
                    , Key.down "b"
                    , Key.up "b"
                    ]
                  )
                , ( "b"
                  , [ Key.right "a"
                    , Key.left "a"
                    , Key.down "a"
                    , Key.up "a"
                    ]
                  )
                ]
            }
        )


allCases :
    List String
    ->
        { noEvents : List ( String, List (Event String) )
        , justLeftRight : List ( String, List (Event String) )
        , justUpDown : List ( String, List (Event String) )
        , allEvents : List ( String, List (Event String) )
        }
    -> List Test
allCases startingList expected =
    [ test "without left/right or up/down events" <|
        \() ->
            FocusLoop.addEvents
                { toId = identity
                , focus = identity
                , leftRight = False
                , upDown = False
                }
                startingList
                |> Expect.equal expected.noEvents
    , test "with left/right and without up/down events" <|
        \() ->
            FocusLoop.addEvents
                { toId = identity
                , focus = identity
                , leftRight = True
                , upDown = False
                }
                startingList
                |> Expect.equal expected.justLeftRight
    , test "without left/right and with up/down events" <|
        \() ->
            FocusLoop.addEvents
                { toId = identity
                , focus = identity
                , leftRight = False
                , upDown = True
                }
                startingList
                |> Expect.equal expected.justUpDown
    , test "with left/right and up/down events" <|
        \() ->
            FocusLoop.addEvents
                { toId = identity
                , focus = identity
                , leftRight = True
                , upDown = True
                }
                startingList
                |> Expect.equal expected.allEvents
    ]
