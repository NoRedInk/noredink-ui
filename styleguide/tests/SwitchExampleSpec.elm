module SwitchExampleSpec exposing (suite)

import ProgramTest exposing (..)
import Routes exposing (Route)
import Test exposing (..)
import Test.Html.Selector exposing (..)
import TestApp exposing (app)


route : Route
route =
    Routes.Doodad "Switch"


suite : Test
suite =
    describe "Switch"
        [ test "it works" <|
            \() ->
                app route
                    |> ensureViewHas [ text "Nri.Ui.Switch" ]
                    |> done
        ]
