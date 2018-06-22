module Headings exposing (h1, h2, h3, h4, h5)

import HeadingsStyled
import Html
import Html.Styled


h1 : List (Html.Html msg) -> Html.Html msg
h1 =
    toRootHtml HeadingsStyled.h1


h2 : List (Html.Html msg) -> Html.Html msg
h2 =
    toRootHtml HeadingsStyled.h2


h3 : List (Html.Html msg) -> Html.Html msg
h3 =
    toRootHtml HeadingsStyled.h3


h4 : List (Html.Html msg) -> Html.Html msg
h4 =
    toRootHtml HeadingsStyled.h4


h5 : List (Html.Html msg) -> Html.Html msg
h5 =
    toRootHtml HeadingsStyled.h5


toRootHtml : (List (Html.Styled.Html msg) -> Html.Styled.Html msg) -> (List (Html.Html msg) -> Html.Html msg)
toRootHtml node =
    \children ->
        List.map Html.Styled.fromUnstyled children
            |> node
            |> Html.Styled.toUnstyled
