module SwitchExampleSpec exposing (suite)

import Accessibility.Aria as Aria
import Accessibility.Role as Role
import Examples.Switch exposing (Msg, State, example)
import ProgramTest exposing (..)
import Routes exposing (Route)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
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
    simulateDomEvent
        (Query.find
            [ attribute Role.switch
            , containing [ text name ]
            ]
        )
        Event.click
