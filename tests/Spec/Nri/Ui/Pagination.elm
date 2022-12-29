module Spec.Nri.Ui.Pagination exposing (spec)

import Expect
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
                Pagination.view (always ()) 0 [ () ]
                    |> Html.Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.hasNot [ Selector.tag "nav" ]
        , test "With more than 1 page, renders a nav" <|
            \() ->
                Pagination.view (always ()) 0 [ (), () ]
                    |> Html.Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.has [ Selector.tag "nav" ]
        ]
