module SwitchExampleSpec exposing (suite)

import Accessibility.Aria as Aria
import Accessibility.Role as Role
import Examples.Switch exposing (example)
import Nri.Test.MouseHelpers.V1 as MouseHelpers
import ProgramTest exposing (..)
import Routes exposing (Route)
import Test exposing (..)
import Test.Html.Selector exposing (..)
import TestApp exposing (app)


route : Route
route =
    Routes.exampleRoute example


suite : Test
suite =
    describe "Switch"
        [ test "it works" <|
            \() ->
                app route
                    |> ensureViewHas [ text "Nri.Ui.Switch" ]
                    -- switch starts with aria-checked=true
                    |> ensureViewHas [ attribute (Aria.checked (Just True)) ]
                    -- user can click the first switch
                    |> switchIt "Show pandas in results"
                    -- the switch now has aria-checked=false
                    |> ensureViewHas [ attribute (Aria.checked (Just False)) ]
                    |> done
        ]


switchIt : String -> ProgramTest a b c -> ProgramTest a b c
switchIt name =
    MouseHelpers.click
        [ attribute Role.switch
        , containing [ text name ]
        , id "view-switch-example"
        ]
