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
        ]
