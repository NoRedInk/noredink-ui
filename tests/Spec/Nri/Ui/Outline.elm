module Spec.Nri.Ui.Outline exposing (spec)

import Expect exposing (Expectation)
import Html.Styled as Html exposing (Html, toUnstyled)
import Nri.Ui.Outline.V1 as Outline
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


spec =
    describe "Nri.Ui.Outline"
        [ test "view without rows does not render anything" <|
            \() ->
                Outline.view []
                    |> hasNoUl
        , test "viewKeyed without rows does not render anything" <|
            \() ->
                Outline.viewKeyed []
                    |> hasNoUl
        , test "view with rows renders ul with lis" <|
            \() ->
                Outline.view
                    [ Outline.row
                        { title = Nothing
                        , content = Html.text ""
                        , palette = Outline.gray
                        , rows = []
                        }
                    ]
                    |> hasOneUlWithOneLi
        , test "viewKeyed with rows renders ul with lis" <|
            \() ->
                Outline.viewKeyed
                    [ Outline.keyedRow "key"
                        { title = Nothing
                        , content = Html.text ""
                        , palette = Outline.gray
                        , rows = []
                        }
                    ]
                    |> hasOneUlWithOneLi
        ]


hasNoUl : Html msg -> Expectation
hasNoUl content =
    Html.div [] [ content ]
        |> toUnstyled
        |> Query.fromHtml
        |> Query.hasNot [ tag "ul" ]


hasOneUlWithOneLi : Html msg -> Expectation
hasOneUlWithOneLi content =
    Html.div [] [ content ]
        |> toUnstyled
        |> Query.fromHtml
        |> Expect.all
            [ Query.count (Expect.equal 1) << Query.findAll [ tag "ul" ]
            , Query.count (Expect.equal 1) << Query.findAll [ tag "li" ]
            ]
