module Spec.Nri.Ui.SideNav exposing (spec)

import Accessibility.Aria as Aria
import Expect exposing (Expectation)
import Html.Attributes as Attributes
import Html.Styled exposing (toUnstyled)
import Nri.Ui.SideNav.V4 as SideNav exposing (Entry, NavAttribute)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "SideNav"
        [ describe "tags the current page correctly" currentPageTests
        ]


currentPageTests : List Test
currentPageTests =
    [ test "without entries" <|
        \() ->
            []
                |> viewQuery { currentRoute = "cactus" }
                |> Query.hasNot [ tag "nav" ]
    , test "with 1 partial entry" <|
        \() ->
            [ SideNav.entry "Cactus" [] ]
                |> viewQuery { currentRoute = "a-different-route" }
                |> expectNoCurrentPage
    , test "with 1 complete but not current entry" <|
        \() ->
            [ SideNav.entry "Cactus" [ SideNav.href "cactus" ] ]
                |> viewQuery { currentRoute = "a-different-route" }
                |> expectNoCurrentPage
    , test "with 1 current entry" <|
        \() ->
            [ SideNav.entry "Cactus" [ SideNav.href "cactus" ] ]
                |> viewQuery { currentRoute = "cactus" }
                |> expectCurrentPage "Cactus" "cactus"
    , test "with multiple entries, one of which is current" <|
        \() ->
            [ SideNav.entry "Cactus" [ SideNav.href "cactus" ]
            , SideNav.entry "Epiphyllum" [ SideNav.href "epiphyllum" ]
            ]
                |> viewQuery { currentRoute = "cactus" }
                |> expectCurrentPage "Cactus" "cactus"
    , test "with a currently-selected entry with children" <|
        \() ->
            [ SideNav.entryWithChildren "Cactus"
                [ SideNav.href "cactus" ]
                [ SideNav.entry "Epiphyllum" [ SideNav.href "epiphyllum" ] ]
            ]
                |> viewQuery { currentRoute = "cactus" }
                |> expectCurrentPage "Cactus" "cactus"
    , test "with a currently-selected child entry" <|
        \() ->
            [ SideNav.entryWithChildren "Cactus"
                [ SideNav.href "cactus" ]
                [ SideNav.entry "Epiphyllum" [ SideNav.href "epiphyllum" ] ]
            ]
                |> viewQuery { currentRoute = "epiphyllum" }
                |> expectCurrentPage "Epiphyllum" "epiphyllum"
    ]


expectNoCurrentPage : Query.Single msg -> Expectation
expectNoCurrentPage =
    Expect.all
        [ Query.hasNot [ attribute Aria.currentPage ]
        , Query.hasNot [ mobilePageName ]
        ]


expectCurrentPage : String -> String -> Query.Single msg -> Expectation
expectCurrentPage name href_ =
    Expect.all
        [ Query.findAll [ attribute Aria.currentPage ]
            >> Query.count (Expect.equal 1)
        , Query.has [ currentPage name href_ ]
        , -- for mobile, shows the currently-selected route in text
          Query.has
            [ mobilePageName
            , containing [ text name ]
            ]
        ]


currentPage : String -> String -> Selector
currentPage name href_ =
    all
        [ tag "a"
        , attribute (Attributes.href href_)
        , attribute Aria.currentPage
        , text name
        ]


mobilePageName : Selector
mobilePageName =
    attribute (Attributes.attribute "data-nri-description" "mobile-current-page-name")


viewQuery :
    { currentRoute : String }
    -> List (Entry String ())
    -> Query.Single ()
viewQuery { currentRoute } entries =
    SideNav.view
        { isCurrentRoute = (==) currentRoute
        , routeToString = identity
        , onSkipNav = ()
        }
        [ SideNav.collapsible
            { isOpen = True
            , toggle = \_ -> ()
            , isTooltipOpen = False
            , toggleTooltip = \_ -> ()
            }
        ]
        entries
        |> toUnstyled
        |> Query.fromHtml
