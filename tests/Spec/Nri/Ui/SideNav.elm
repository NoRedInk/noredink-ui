module Spec.Nri.Ui.SideNav exposing (spec)

import Expect
import Html.Styled exposing (toUnstyled)
import Nri.Ui.SideNav.V4 as SideNav exposing (Entry, NavAttribute)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "SideNav"
        [ test "without entries" <|
            \() ->
                { currentRoute = "some-route" }
                    |> viewQuery [] []
                    |> Query.hasNot [ tag "nav" ]
        ]


viewQuery :
    List (NavAttribute ())
    -> List (Entry String ())
    -> { currentRoute : String }
    -> Query.Single ()
viewQuery navAttributes entries { currentRoute } =
    SideNav.view
        { isCurrentRoute = (==) currentRoute
        , routeToString = identity
        , onSkipNav = ()
        }
        navAttributes
        entries
        |> toUnstyled
        |> Query.fromHtml
