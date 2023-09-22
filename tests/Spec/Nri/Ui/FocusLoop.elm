module Spec.Nri.Ui.FocusLoop exposing (spec)

import Accessibility.Styled.Key as Key exposing (Event)
import Expect
import Html.Styled as Html exposing (Html)
import Nri.Test.KeyboardHelpers.V1 exposing (pressLeftArrow, pressRightArrow)
import Nri.Ui.FocusLoop.V1 as FocusLoop
import ProgramTest exposing (..)
import Test exposing (..)
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.FocusLoop"
        [ addEventsSpec
        , viewSpec
        ]


addEventsSpec : Test
addEventsSpec =
    describe "addEvents"
        [ forAnEmptyList
        , forASingletonList
        , forATwoElementList
        , forAThreeElementList
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


forAThreeElementList : Test
forAThreeElementList =
    describe "for a three-element list"
        (allCases [ "a", "b", "c" ]
            { noEvents = [ ( "a", [] ), ( "b", [] ), ( "c", [] ) ]
            , justLeftRight =
                [ ( "a", [ Key.right "b", Key.left "c" ] )
                , ( "b", [ Key.right "c", Key.left "a" ] )
                , ( "c", [ Key.right "a", Key.left "b" ] )
                ]
            , justUpDown =
                [ ( "a", [ Key.down "b", Key.up "c" ] )
                , ( "b", [ Key.down "c", Key.up "a" ] )
                , ( "c", [ Key.down "a", Key.up "b" ] )
                ]
            , allEvents =
                [ ( "a"
                  , [ Key.right "b"
                    , Key.left "c"
                    , Key.down "b"
                    , Key.up "c"
                    ]
                  )
                , ( "b"
                  , [ Key.right "c"
                    , Key.left "a"
                    , Key.down "c"
                    , Key.up "a"
                    ]
                  )
                , ( "c"
                  , [ Key.right "a"
                    , Key.left "b"
                    , Key.down "a"
                    , Key.up "b"
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
                { focus = identity
                , leftRight = False
                , upDown = False
                }
                startingList
                |> Expect.equal expected.noEvents
    , test "with left/right and without up/down events" <|
        \() ->
            FocusLoop.addEvents
                { focus = identity
                , leftRight = True
                , upDown = False
                }
                startingList
                |> Expect.equal expected.justLeftRight
    , test "without left/right and with up/down events" <|
        \() ->
            FocusLoop.addEvents
                { focus = identity
                , leftRight = False
                , upDown = True
                }
                startingList
                |> Expect.equal expected.justUpDown
    , test "with left/right and up/down events" <|
        \() ->
            FocusLoop.addEvents
                { focus = identity
                , leftRight = True
                , upDown = True
                }
                startingList
                |> Expect.equal expected.allEvents
    ]


viewSpec : Test
viewSpec =
    describe "view"
        [ test "moves focus right on right arrow" <|
            \() ->
                program
                    |> pressRightArrow { targetDetails = [] }
                        [ Selector.all
                            [ Selector.tag "button"
                            , Selector.containing
                                [ Selector.text "foo"
                                ]
                            ]
                        ]
                    |> ProgramTest.expectModel (\model -> Expect.equal (Just "bar") model.focused)
        , test "moves focus left on left arrow" <|
            \() ->
                program
                    |> pressLeftArrow { targetDetails = [] }
                        [ Selector.all
                            [ Selector.tag "button"
                            , Selector.containing
                                [ Selector.text "baz"
                                ]
                            ]
                        ]
                    |> ProgramTest.expectModel (\model -> Expect.equal (Just "bar") model.focused)
        , test "loops to end" <|
            \() ->
                program
                    |> pressLeftArrow { targetDetails = [] }
                        [ Selector.all
                            [ Selector.tag "button"
                            , Selector.containing
                                [ Selector.text "foo"
                                ]
                            ]
                        ]
                    |> ProgramTest.expectModel (\model -> Expect.equal (Just "baz") model.focused)
        , test "loops to beginning" <|
            \() ->
                program
                    |> pressRightArrow { targetDetails = [] }
                        [ Selector.all
                            [ Selector.tag "button"
                            , Selector.containing
                                [ Selector.text "baz"
                                ]
                            ]
                        ]
                    |> ProgramTest.expectModel (\model -> Expect.equal (Just "foo") model.focused)
        ]


type alias State =
    { foos : List String, focused : Maybe String }


type Msg
    = Focus String


type alias TestContext =
    ProgramTest State Msg ()


update : Msg -> State -> State
update msg model =
    case msg of
        Focus item ->
            { model
                | focused = Just item
            }


view : State -> List (Html Msg)
view state =
    FocusLoop.view
        { focus = Focus
        , toId = identity
        , leftRight = True
        , upDown = True
        , view = \arrowKeyHandlers item -> Html.button [ Key.onKeyDownPreventDefault arrowKeyHandlers ] [ Html.text item ]
        }
        state.foos


program : TestContext
program =
    ProgramTest.createSandbox
        { init =
            { foos = [ "foo", "bar", "baz" ]
            , focused = Nothing
            }
        , update = update
        , view = view >> Html.div [] >> Html.toUnstyled
        }
        |> ProgramTest.start ()
