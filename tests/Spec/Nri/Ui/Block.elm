module Spec.Nri.Ui.Block exposing (spec)

import Browser.Dom exposing (Element)
import Dict
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
        , describe "getLabelHeights" getLabelHeightsSpec
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


getLabelHeightsSpec : List Test
getLabelHeightsSpec =
    [ test "without any ids or measurements, does not specify heights" <|
        \() ->
            Block.getLabelHeights [] Dict.empty
                |> Expect.equal Dict.empty
    , test "without any measurements, does not specify heights" <|
        \() ->
            Block.getLabelHeights [ "a" ] Dict.empty
                |> Expect.equal Dict.empty
    , test "without any ids, does not specify heights" <|
        \() ->
            Block.getLabelHeights []
                (Dict.singleton "a"
                    { label = dummyElement { x = 0, y = 0, width = 100, height = 100 }
                    , labelContent = dummyElement { x = 0, y = 0, width = 100, height = 20 }
                    }
                )
                |> Expect.equal Dict.empty
    , test "with 1 id and 1 measurement, specifies the default heights" <|
        \() ->
            let
                startingHeight =
                    20
            in
            Block.getLabelHeights [ "a" ]
                (Dict.singleton "a"
                    { label = dummyElement { x = 0, y = 0, width = 100, height = 100 }
                    , labelContent = dummyElement { x = 0, y = 0, width = 100, height = 20 }
                    }
                )
                |> Expect.equal
                    (Dict.singleton "a"
                        { totalHeight = startingHeight + defaultArrowHeight + balloonOffset
                        , arrowHeight = defaultArrowHeight
                        }
                    )
    , test "with multiple ids and measurements, positions wider elements above narrower elements" <|
        \() ->
            let
                startingHeight =
                    20
            in
            Block.getLabelHeights [ "a", "b", "c" ]
                ([ --  A is the second-widest element
                   ( "a"
                   , { label = dummyElement { x = 0, y = 0, width = 200, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 200, height = 20 }
                     }
                   )
                 , -- B is the narrowest element
                   ( "b"
                   , { label = dummyElement { x = 0, y = 0, width = 100, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 100, height = 20 }
                     }
                   )
                 , -- C is the widest element
                   ( "c"
                   , { label = dummyElement { x = 0, y = 0, width = 300, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 300, height = 20 }
                     }
                   )
                 ]
                    |> Dict.fromList
                )
                |> Expect.equal
                    ([ ( "a"
                       , { totalHeight = 2 * startingHeight + defaultArrowHeight + balloonOffset
                         , arrowHeight =
                            -- A is positioned on top of B.
                            -- So its arrow height is the total height of b minus the positioning offset
                            startingHeight + defaultArrowHeight
                         }
                       )
                     , ( "b"
                       , { totalHeight = 1 * startingHeight + defaultArrowHeight + balloonOffset
                         , arrowHeight = defaultArrowHeight
                         }
                       )
                     , ( "c"
                       , { totalHeight = 3 * startingHeight + defaultArrowHeight + balloonOffset
                         , arrowHeight =
                            -- C is positioned on top of A.
                            -- So its arrow height is the total height of A minus the positioning offset
                            2 * startingHeight + defaultArrowHeight
                         }
                       )
                     ]
                        |> Dict.fromList
                    )
    ]


balloonOffset : Float
balloonOffset =
    8


defaultArrowHeight : Float
defaultArrowHeight =
    8


dummyElement : { x : Float, y : Float, width : Float, height : Float } -> Element
dummyElement element =
    { scene = { width = 1000, height = 1000 }
    , viewport = { x = 0, y = 0, width = 1000, height = 500 }
    , element = element
    }
