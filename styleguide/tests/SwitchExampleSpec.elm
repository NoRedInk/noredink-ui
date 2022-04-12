module SwitchExampleSpec exposing (suite)

import Accessibility.Aria as Aria
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
                    -- switch starts with aria-checked=true and text "On"
                    |> ensureViewHas
                        [ attribute (Aria.checked (Just True))
                        , text "On"
                        ]
                    -- user can click the first switch
                    |> check "switch-interactive" "On" False
                    -- the switch now has aria-checked=false and text "Off"
                    |> ensureViewHas
                        [ attribute (Aria.checked (Just False))
                        , text "Off"
                        ]
                    |> done
        ]
