module SwitchExampleSpec exposing (suite)

import Examples.Switch exposing (Msg, State, example)
import ProgramTest exposing (..)
import Routes exposing (Route)
import Test exposing (..)
import Test.Html.Selector exposing (..)
import TestApp exposing (app)


route : Route State Msg
route =
    Routes.Doodad example


suite : Test
suite =
    describe "Switch"
        [ test "it works" <|
            \() ->
                app route
                    |> ensureViewHas [ text "Nri.Ui.Switch" ]
                    -- switch starts with checked=true
                    |> ensureViewHas [ checked True ]
                    -- user can click the first switch
                    |> check "view-switch-example" "Show pandas in results" False
                    -- the switch now has checked=false
                    |> ensureViewHas [ checked False ]
                    |> done
        ]
