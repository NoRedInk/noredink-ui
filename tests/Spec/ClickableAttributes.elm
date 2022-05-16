module Spec.ClickableAttributes exposing (suite)

import ClickableAttributes exposing (ClickableAttributes)
import Expect
import Html.Attributes exposing (href)
import Html.Styled exposing (a, text, toUnstyled)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test
suite =
    describe "ClickableAttributes"
        [ validLinkAttributes ]


validLinkAttributes : Test
validLinkAttributes =
    describe "link attributes"
        [ test "with an href" <|
            \() ->
                ClickableAttributes.href "some-route"
                    |> setupLinkTest
                    |> Expect.all
                        [ Query.has [ Selector.attribute (href "some-route") ]
                        ]
        , test "with an external href" <|
            \() ->
                ClickableAttributes.linkExternal "some-route"
                    |> setupLinkTest
                    |> Expect.all
                        [ Query.has [ Selector.attribute (href "some-route") ]
                        ]
        , test "with an external href that also supports tracking" <|
            \() ->
                ClickableAttributes.linkExternalWithTracking { track = "track it!", url = "some-route" }
                    |> setupLinkTest
                    |> Expect.all
                        [ Query.has [ Selector.attribute (href "some-route") ]
                        ]
        , test "with a SPA link" <|
            \() ->
                ClickableAttributes.linkSpa "some-route"
                    |> setupLinkTest
                    |> Expect.all
                        [ Query.has [ Selector.attribute (href "some-route") ]
                        ]
        , test "with a link with a method" <|
            \() ->
                ClickableAttributes.linkWithMethod { method = "the right way", url = "some-route" }
                    |> setupLinkTest
                    |> Expect.all
                        [ Query.has [ Selector.attribute (href "some-route") ]
                        ]
        , test "with a link with tracking" <|
            \() ->
                ClickableAttributes.linkWithTracking { track = "track it!", url = "some-route" }
                    |> setupLinkTest
                    |> Expect.all
                        [ Query.has [ Selector.attribute (href "some-route") ]
                        ]
        ]


setupLinkTest : (ClickableAttributes String msg -> ClickableAttributes String msg) -> Query.Single msg
setupLinkTest withLink =
    let
        renderTestAnchorTag ( _, attributes ) =
            a attributes [ text "Test link" ]
                |> toUnstyled
                |> Query.fromHtml
    in
    ClickableAttributes.init
        |> withLink
        |> ClickableAttributes.toLinkAttributes
            { routeToString = identity, isDisabled = False }
        |> renderTestAnchorTag
