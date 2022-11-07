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
            Block.view []
                |> Html.Styled.toUnstyled
                |> Query.fromHtml
                |> Query.has [ Selector.text "blank" ]
    , test "plaintext" <|
        \() ->
            Block.view [ Block.plaintext "Yo" ]
                |> Html.Styled.toUnstyled
                |> Query.fromHtml
                |> Expect.all
                    [ Query.hasNot [ Selector.text "blank" ]
                    , Query.has [ Selector.text "Yo" ]
                    ]
    , test "content with string and blank" <|
        \() ->
            Block.view [ Block.content [ Block.string "Yo", Block.blank ] ]
                |> Html.Styled.toUnstyled
                |> Query.fromHtml
                |> Query.has [ Selector.text "Yo", Selector.text "blank" ]
    ]
