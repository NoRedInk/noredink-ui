module Spec.Nri.Ui.Panel exposing (..)

import Html
import Html.Styled exposing (..)
import Nri.Ui.Panel.V1 as Panel
import Test exposing (..)
import Test.Html.Query as Query


spec : Test
spec =
    describe "Nri.Ui.Panel.V1"
        [ describe "renders content" rendersContent
        ]


rendersContent : List Test
rendersContent =
    [ test "renders markdown correctly" <|
        \() ->
            Panel.view
                [ Panel.header "Header"
                , Panel.markdown "_italic_ **bold**"
                ]
                |> toUnstyled
                |> Query.fromHtml
                |> Query.contains
                    [ Html.em [] [ Html.text "italic" ]
                    , Html.strong [] [ Html.text "bold" ]
                    ]
    , test "renders html correctly" <|
        \() ->
            Panel.view
                [ Panel.header "Header"
                , Panel.html [ em [] [ text "italic" ], strong [] [ text "bold" ] ]
                ]
                |> toUnstyled
                |> Query.fromHtml
                |> Query.contains
                    [ Html.em [] [ Html.text "italic" ], Html.strong [] [ Html.text "bold" ] ]
    ]
