module Spec.Nri.Ui.FocusLoopLazy exposing (spec)

{-|

    This spec is the same as the one in `Nri.Ui.FocusLoop.V1` except that it uses the lazy wrapper

-}

import Accessibility.Styled.Key as Key
import Expect
import Html.Styled as Html exposing (Html)
import Nri.Test.KeyboardHelpers.V1 exposing (pressLeftArrow, pressRightArrow)
import Nri.Ui.FocusLoop.Lazy.V1 as FocusLoop
import ProgramTest exposing (..)
import Test exposing (..)
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.FocusLoop.Lazy"
        [ viewSpec
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
    FocusLoop.lazy
        { focus = Focus
        , toId = identity
        , leftRight = True
        , upDown = True
        , view = \arrowKeyHandlers item -> Html.button [ Key.onKeyDownPreventDefault arrowKeyHandlers ] [ Html.text item ]
        }
        state.foos
        |> List.map Tuple.second


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
