module Spec.Nri.Ui.Block exposing (spec)

import Expect
import Html.Styled
import Nri.Ui.Block.V1 as Block
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.Block.V1"
        [ describe "content" contentSpec
        ]


contentSpec : List Test
contentSpec =
    [ test "blank" <|
        \() ->
            []
                |> toQuery
                |> Query.has [ Selector.text "blank" ]
    , test "plaintext" <|
        \() ->
            [ Block.plaintext "Yo" ]
                |> toQuery
                |> Expect.all
                    [ Query.hasNot [ Selector.text "blank" ]
                    , Query.has [ Selector.text "Yo" ]
                    ]
    , test "content with string and blank" <|
        \() ->
            [ Block.content [ Block.string "Yo", Block.blank ] ]
                |> toQuery
                |> Query.has [ Selector.text "Yo", Selector.text "blank" ]
    ]


toQuery : List Block.Attribute -> Query.Single a
toQuery block =
    Block.view block
        |> Html.Styled.p []
        |> Html.Styled.toUnstyled
        |> Query.fromHtml
