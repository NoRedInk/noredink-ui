module Spec.Nri.Ui.Pagination exposing (spec)

import Accessibility.Aria as Aria
import Expect
import Html.Attributes as Attributes
import Html.Styled
import Nri.Ui.Pagination.V1 as Pagination
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.Pagination.V1"
        [ test "Without any pages, does not render an empty nav" <|
            \() ->
                Pagination.view (always ()) 0 []
                    |> Html.Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.hasNot [ Selector.tag "nav" ]
        , test "With 1 page, does not render a nav without active links" <|
            \() ->
                Pagination.view (always ()) 0 [ "#" ]
                    |> Html.Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.hasNot [ Selector.tag "nav" ]
        , test "With more than 1 page, renders a nav with the current page indicated" <|
            \() ->
                Pagination.view (always ()) 0 [ "#", "#" ]
                    |> Html.Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.has
                        [ Selector.tag "nav"
                        , Selector.containing
                            [ Selector.all
                                [ Selector.attribute (Aria.label "Page 1")
                                , Selector.attribute Aria.currentPage
                                ]
                            ]
                        , Selector.containing
                            [ Selector.attribute (Aria.label "Page 2") ]
                        ]
        , test "Uses anchor tags rather than buttons" <|
            \() ->
                Pagination.view (always ()) 0 [ "#", "#" ]
                    |> Html.Styled.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ Selector.tag "a" ]
                        , Query.hasNot [ Selector.tag "button" ]
                        ]
        , test "Marks 'previous' link as disabled when on the first page" <|
            \() ->
                Pagination.view (always ()) 0 [ "#first", "#second" ]
                    |> Html.Styled.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has
                            [ Selector.all
                                [ Selector.tag "a"
                                , Selector.containing [ Selector.text "Previous" ]
                                , Selector.attribute (Aria.disabled True)
                                ]
                            ]
                        , Query.has
                            [ Selector.all
                                [ Selector.tag "a"
                                , Selector.containing [ Selector.text "Next" ]
                                , Selector.attribute (Attributes.href "#second")
                                ]
                            ]
                        ]
        , test "Marks 'next' link as disabled when on the last page" <|
            \() ->
                Pagination.view (always ()) 1 [ "#first", "#second" ]
                    |> Html.Styled.toUnstyled
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has
                            [ Selector.all
                                [ Selector.tag "a"
                                , Selector.containing [ Selector.text "Previous" ]
                                , Selector.attribute (Attributes.href "#first")
                                ]
                            ]
                        , Query.has
                            [ Selector.all
                                [ Selector.tag "a"
                                , Selector.containing [ Selector.text "Next" ]
                                , Selector.attribute (Aria.disabled True)
                                ]
                            ]
                        ]
        ]
