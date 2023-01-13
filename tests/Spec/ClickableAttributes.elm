module Spec.ClickableAttributes exposing (suite)

import Accessibility.Aria as Aria
import Accessibility.Role as Role
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
        [ linkAttributes, disabledLinkAttributes ]


linkAttributes : Test
linkAttributes =
    describe "link attributes"
        [ test "with an href" <|
            \() ->
                ClickableAttributes.href "some-route"
                    |> setupLinkTest
                    |> Expect.all
                        [ Query.has [ Selector.attribute (href "some-route") ]
                        , -- should not include a redundant role
                          Query.hasNot [ Selector.attribute Role.link ]
                        , Query.hasNot [ Selector.attribute (Aria.disabled True) ]
                        ]
        , test "with an external href" <|
            \() ->
                ClickableAttributes.linkExternal "some-route"
                    |> setupLinkTest
                    |> Expect.all
                        [ Query.has [ Selector.attribute (href "some-route") ]
                        , -- should not include a redundant role
                          Query.hasNot [ Selector.attribute Role.link ]
                        , Query.hasNot [ Selector.attribute (Aria.disabled True) ]
                        ]
        , test "with an external href that also supports tracking" <|
            \() ->
                ClickableAttributes.linkExternalWithTracking { track = "track it!", url = "some-route" }
                    |> setupLinkTest
                    |> Expect.all
                        [ Query.has [ Selector.attribute (href "some-route") ]
                        , -- should not include a redundant role
                          Query.hasNot [ Selector.attribute Role.link ]
                        , Query.hasNot [ Selector.attribute (Aria.disabled True) ]
                        ]
        , test "with a SPA link" <|
            \() ->
                ClickableAttributes.linkSpa "some-route"
                    |> setupLinkTest
                    |> Expect.all
                        [ Query.has [ Selector.attribute (href "some-route") ]
                        , -- should not include a redundant role
                          Query.hasNot [ Selector.attribute Role.link ]
                        , Query.hasNot [ Selector.attribute (Aria.disabled True) ]
                        ]
        , test "with a link with a method" <|
            \() ->
                ClickableAttributes.linkWithMethod { method = "the right way", url = "some-route" }
                    |> setupLinkTest
                    |> Expect.all
                        [ Query.has [ Selector.attribute (href "some-route") ]
                        , -- should not include a redundant role
                          Query.hasNot [ Selector.attribute Role.link ]
                        , Query.hasNot [ Selector.attribute (Aria.disabled True) ]
                        ]
        , test "with a link with tracking" <|
            \() ->
                ClickableAttributes.linkWithTracking { track = "track it!", url = "some-route" }
                    |> setupLinkTest
                    |> Expect.all
                        [ Query.has [ Selector.attribute (href "some-route") ]
                        , -- should not include a redundant role
                          Query.hasNot [ Selector.attribute Role.link ]
                        , Query.hasNot [ Selector.attribute (Aria.disabled True) ]
                        ]
        ]


disabledLinkAttributes : Test
disabledLinkAttributes =
    describe "disabled link attributes"
        [ test "with an href" <|
            \() ->
                ClickableAttributes.href "some-route"
                    |> setupDisabledLinkTest
                    |> Expect.all
                        [ Query.hasNot [ Selector.attribute (href "some-route") ]
                        , -- should explicitly be tagged as having the link role
                          Query.has [ Selector.attribute Role.link ]
                        , -- should be marked as disabled for screenreader users
                          Query.has [ Selector.attribute (Aria.disabled True) ]
                        ]
        , test "with an external href" <|
            \() ->
                ClickableAttributes.linkExternal "some-route"
                    |> setupDisabledLinkTest
                    |> Expect.all
                        [ Query.hasNot [ Selector.attribute (href "some-route") ]
                        , -- should explicitly be tagged as having the link role
                          Query.has [ Selector.attribute Role.link ]
                        , -- should be marked as disabled for screenreader users
                          Query.has [ Selector.attribute (Aria.disabled True) ]
                        ]
        , test "with an external href that also supports tracking" <|
            \() ->
                ClickableAttributes.linkExternalWithTracking { track = "track it!", url = "some-route" }
                    |> setupDisabledLinkTest
                    |> Expect.all
                        [ Query.hasNot [ Selector.attribute (href "some-route") ]
                        , -- should explicitly be tagged as having the link role
                          Query.has [ Selector.attribute Role.link ]
                        , -- should be marked as disabled for screenreader users
                          Query.has [ Selector.attribute (Aria.disabled True) ]
                        ]
        , test "with a SPA link" <|
            \() ->
                ClickableAttributes.linkSpa "some-route"
                    |> setupDisabledLinkTest
                    |> Expect.all
                        [ Query.hasNot [ Selector.attribute (href "some-route") ]
                        , -- should explicitly be tagged as having the link role
                          Query.has [ Selector.attribute Role.link ]
                        , -- should be marked as disabled for screenreader users
                          Query.has [ Selector.attribute (Aria.disabled True) ]
                        ]
        , test "with a link with a method" <|
            \() ->
                ClickableAttributes.linkWithMethod { method = "the right way", url = "some-route" }
                    |> setupDisabledLinkTest
                    |> Expect.all
                        [ Query.hasNot [ Selector.attribute (href "some-route") ]
                        , -- should explicitly be tagged as having the link role
                          Query.has [ Selector.attribute Role.link ]
                        , -- should be marked as disabled for screenreader users
                          Query.has [ Selector.attribute (Aria.disabled True) ]
                        ]
        , test "with a link with tracking" <|
            \() ->
                ClickableAttributes.linkWithTracking { track = "track it!", url = "some-route" }
                    |> setupDisabledLinkTest
                    |> Expect.all
                        [ Query.hasNot [ Selector.attribute (href "some-route") ]
                        , -- should explicitly be tagged as having the link role
                          Query.has [ Selector.attribute Role.link ]
                        , -- should be marked as disabled for screenreader users
                          Query.has [ Selector.attribute (Aria.disabled True) ]
                        ]
        ]


setupLinkTest : (ClickableAttributes.Config {} String msg -> ClickableAttributes.Config {} String msg) -> Query.Single msg
setupLinkTest withLink =
    { clickableAttributes = ClickableAttributes.init, rightIcon = Nothing }
        |> withLink
        |> .clickableAttributes
        |> ClickableAttributes.toLinkAttributes { routeToString = identity, isDisabled = False }
        |> renderTestAnchorTag


setupDisabledLinkTest : (ClickableAttributes.Config {} String msg -> ClickableAttributes.Config {} String msg) -> Query.Single msg
setupDisabledLinkTest withLink =
    { clickableAttributes = ClickableAttributes.init, rightIcon = Nothing }
        |> withLink
        |> .clickableAttributes
        |> ClickableAttributes.toLinkAttributes
            { routeToString = identity, isDisabled = True }
        |> renderTestAnchorTag


renderTestAnchorTag : ( a, List (Html.Styled.Attribute msg) ) -> Query.Single msg
renderTestAnchorTag ( _, attributes ) =
    a attributes [ text "Test link" ]
        |> toUnstyled
        |> Query.fromHtml
