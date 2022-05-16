module Spec.ClickableAttributes exposing (suite)

import ClickableAttributes
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
    let
        renderTestAnchorTag ( _, attributes ) =
            a attributes [ text "Test link" ]
                |> toUnstyled
                |> Query.fromHtml
    in
    describe "link attributes"
        [ test "with an href" <|
            \() ->
                ClickableAttributes.init
                    |> ClickableAttributes.href "some-route"
                    |> ClickableAttributes.toLinkAttributes
                        { routeToString = identity, isDisabled = False }
                    |> renderTestAnchorTag
                    |> Expect.all
                        [ Query.has [ Selector.attribute (href "some-route") ]
                        ]
        , test "with an external href" <|
            \() ->
                ClickableAttributes.init
                    |> ClickableAttributes.linkExternal "some-route"
                    |> ClickableAttributes.toLinkAttributes
                        { routeToString = identity, isDisabled = False }
                    |> renderTestAnchorTag
                    |> Expect.all
                        [ Query.has [ Selector.attribute (href "some-route") ]
                        ]
        , test "with an external href that also supports tracking" <|
            \() ->
                ClickableAttributes.init
                    |> ClickableAttributes.linkExternalWithTracking
                        { track = "track it!", url = "some-route" }
                    |> ClickableAttributes.toLinkAttributes
                        { routeToString = identity, isDisabled = False }
                    |> renderTestAnchorTag
                    |> Expect.all
                        [ Query.has [ Selector.attribute (href "some-route") ]
                        ]
        , test "with a SPA link" <|
            \() ->
                ClickableAttributes.init
                    |> ClickableAttributes.linkSpa "some-route"
                    |> ClickableAttributes.toLinkAttributes
                        { routeToString = identity, isDisabled = False }
                    |> renderTestAnchorTag
                    |> Expect.all
                        [ Query.has [ Selector.attribute (href "some-route") ]
                        ]
        , test "with a link with a method" <|
            \() ->
                ClickableAttributes.init
                    |> ClickableAttributes.linkWithMethod { method = "the right way", url = "some-route" }
                    |> ClickableAttributes.toLinkAttributes
                        { routeToString = identity, isDisabled = False }
                    |> renderTestAnchorTag
                    |> Expect.all
                        [ Query.has [ Selector.attribute (href "some-route") ]
                        ]
        , test "with a link with tracking" <|
            \() ->
                ClickableAttributes.init
                    |> ClickableAttributes.linkWithTracking
                        { track = "track it!", url = "some-route" }
                    |> ClickableAttributes.toLinkAttributes
                        { routeToString = identity, isDisabled = False }
                    |> renderTestAnchorTag
                    |> Expect.all
                        [ Query.has [ Selector.attribute (href "some-route") ]
                        ]
        ]
