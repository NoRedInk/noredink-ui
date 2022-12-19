module Spec.Nri.Ui.Block exposing (spec)

import Browser.Dom exposing (Element)
import Dict
import Expect
import Html.Attributes as Attributes
import Html.Styled
import Nri.Ui.Block.V2 as Block
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.Block.V2"
        [ describe "content" contentSpec
        , describe "labelId" labelIdSpec
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
    , test "content with phrase and blank" <|
        \() ->
            [ Block.content (Block.phrase "Yo hello" ++ [ Block.blank ]) ]
                |> toQuery
                |> Query.has [ Selector.text "Yo", Selector.text "blank" ]
    ]


labelIdSpec : List Test
labelIdSpec =
    [ test "an unemphasized word with a labelId" <|
        \() ->
            [ Block.plaintext "Yo"
            , Block.labelId "yo-id"
            ]
                |> toQuery
                |> Query.findAll [ Selector.attribute (Attributes.id "yo-id") ]
                |> Query.count (Expect.equal 0)
    , test "a single emphasized word with a labelId" <|
        \() ->
            [ Block.plaintext "Yo"
            , Block.labelId "yo-id"
            , Block.label "label content"
            ]
                |> toQuery
                |> Query.findAll [ Selector.attribute (Attributes.id "yo-id") ]
                |> Query.count (Expect.equal 1)
    , test "emphasized phrase with an id" <|
        \() ->
            [ Block.plaintext "Yo yo yo"
            , Block.labelId "yo-id"
            , Block.label "label content"
            ]
                |> toQuery
                |> Query.findAll [ Selector.attribute (Attributes.id "yo-id") ]
                |> Query.count (Expect.equal 1)
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
                    , labelContent = dummyElement { x = 0, y = 0, width = 100, height = startingHeight }
                    }
                )
                |> Expect.equal
                    (Dict.singleton "a"
                        { totalHeight = startingHeight + defaultArrowHeight + balloonOffset
                        , arrowHeight = defaultArrowHeight
                        }
                    )
    , test "with different height measurements, prevents overlaps" <|
        \() ->
            let
                aStartingHeight =
                    40

                bStartingHeight =
                    30
            in
            Block.getLabelHeights [ "a", "b", "c" ]
                ([ --  A has taller content
                   ( "a"
                   , { label = dummyElement { x = 0, y = 0, width = 100, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 100, height = aStartingHeight }
                     }
                   )
                 , -- B has shorter content
                   ( "b"
                   , { label = dummyElement { x = 0, y = 0, width = 200, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 100, height = bStartingHeight }
                     }
                   )
                 ]
                    |> Dict.fromList
                )
                |> Expect.equal
                    ([ ( "a"
                       , { totalHeight = aStartingHeight + defaultArrowHeight + balloonOffset
                         , arrowHeight = defaultArrowHeight
                         }
                       )
                     , ( "b"
                       , { totalHeight = aStartingHeight + bStartingHeight + defaultArrowHeight + balloonOffset
                         , arrowHeight = aStartingHeight + defaultArrowHeight
                         }
                       )
                     ]
                        |> Dict.fromList
                    )
    , test "with different height measurements but no overlaps, keeps labels in normal position" <|
        \() ->
            let
                aStartingHeight =
                    40

                bStartingHeight =
                    30

                aWidth =
                    100

                bX =
                    aWidth + 1
            in
            Block.getLabelHeights [ "a", "b", "c" ]
                ([ --  A has taller content
                   ( "a"
                   , { label = dummyElement { x = 0, y = 0, width = aWidth, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = aWidth, height = aStartingHeight }
                     }
                   )
                 , -- B has shorter content
                   ( "b"
                   , { label = dummyElement { x = bX, y = 0, width = 200, height = 100 }
                     , labelContent = dummyElement { x = bX, y = 0, width = 100, height = bStartingHeight }
                     }
                   )
                 ]
                    |> Dict.fromList
                )
                |> Expect.equal
                    ([ ( "a"
                       , { totalHeight = aStartingHeight + defaultArrowHeight + balloonOffset
                         , arrowHeight = defaultArrowHeight
                         }
                       )
                     , ( "b"
                       , { totalHeight = bStartingHeight + defaultArrowHeight + balloonOffset
                         , arrowHeight = defaultArrowHeight
                         }
                       )
                     ]
                        |> Dict.fromList
                    )
    , test "with multiple ids and measurements, positions overlapping wider elements above narrower elements" <|
        \() ->
            let
                startingHeight =
                    20
            in
            Block.getLabelHeights [ "a", "b", "c" ]
                ([ --  A is the second-widest element
                   ( "a"
                   , { label = dummyElement { x = 0, y = 0, width = 200, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 200, height = startingHeight }
                     }
                   )
                 , -- B is the narrowest element
                   ( "b"
                   , { label = dummyElement { x = 0, y = 0, width = 100, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 100, height = startingHeight }
                     }
                   )
                 , -- C is the widest element
                   ( "c"
                   , { label = dummyElement { x = 0, y = 0, width = 300, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 300, height = startingHeight }
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
    , test "with multiple ids and measurements, positions non-overlapping elements in default positions" <|
        \() ->
            let
                startingHeight =
                    20
            in
            Block.getLabelHeights [ "a", "b", "c" ]
                ([ --  A is the second-widest element, but doesn't overlap any other labels
                   ( "a"
                   , { label = dummyElement { x = 500, y = 0, width = 200, height = 100 }
                     , labelContent = dummyElement { x = 500, y = 0, width = 200, height = startingHeight }
                     }
                   )
                 , -- B is the narrowest element and it overlaps C
                   ( "b"
                   , { label = dummyElement { x = 10, y = 0, width = 100, height = 100 }
                     , labelContent = dummyElement { x = 10, y = 0, width = 100, height = startingHeight }
                     }
                   )
                 , -- C is the widest element and it overlaps B
                   ( "c"
                   , { label = dummyElement { x = 0, y = 0, width = 300, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 300, height = startingHeight }
                     }
                   )
                 ]
                    |> Dict.fromList
                )
                |> Expect.equal
                    ([ ( "a"
                       , -- A is positioned in its default position.
                         { totalHeight = 1 * startingHeight + defaultArrowHeight + balloonOffset
                         , arrowHeight = defaultArrowHeight
                         }
                       )
                     , -- B is positioned under C
                       ( "b"
                       , { totalHeight = 1 * startingHeight + defaultArrowHeight + balloonOffset
                         , arrowHeight = defaultArrowHeight
                         }
                       )
                     , -- C is positioned over B
                       ( "c"
                       , { totalHeight = 2 * startingHeight + defaultArrowHeight + balloonOffset
                         , arrowHeight = 1 * startingHeight + defaultArrowHeight
                         }
                       )
                     ]
                        |> Dict.fromList
                    )
    , test "styleguide example regression test: 3 labels on 1 line, with 1 overlap to resolve" <|
        \() ->
            let
                withElement element =
                    { scene = { width = 497, height = 1974 }
                    , viewport = { x = 0, y = 733, width = 497, height = 373 }
                    , element = element
                    }
            in
            Block.getLabelHeights [ "subjectId", "directObjectId", "prepositionId", "editorsNoteId" ]
                ([ ( "subjectId"
                   , { label = withElement { x = 56.8828125, y = 883, width = 63.515625, height = 32 }
                     , labelContent = withElement { x = 56.8828125, y = 883, width = 63.515625, height = 24 }
                     }
                   )
                 , ( "directObjectId"
                   , { label = withElement { x = 241.03515625, y = 883, width = 98.6171875, height = 32 }
                     , labelContent = withElement { x = 241.03515625, y = 883, width = 98.6171875, height = 24 }
                     }
                   )
                 , ( "prepositionId"
                   , { label = withElement { x = 330.55859375, y = 883, width = 91.2109375, height = 32 }
                     , labelContent = withElement { x = 330.55859375, y = 883, width = 91.2109375, height = 24 }
                     }
                   )
                 , ( "editorsNoteId"
                   , { label = withElement { x = 269.20703125, y = 973.5, width = 100.0859375, height = 32 }
                     , labelContent = withElement { x = 269.20703125, y = 973.5, width = 100.0859375, height = 24 }
                     }
                   )
                 ]
                    |> Dict.fromList
                )
                |> Expect.equal
                    ([ ( "prepositionId", { arrowHeight = 8, totalHeight = 40 } )
                     , ( "directObjectId", { arrowHeight = 32, totalHeight = 64 } )
                     , ( "subjectId", { arrowHeight = 8, totalHeight = 40 } )
                     , ( "editorsNoteId", { arrowHeight = 8, totalHeight = 40 } )
                     ]
                        |> Dict.fromList
                    )
    , test "with overlapping labels on different lines, specifies the default heights" <|
        \() ->
            let
                startingHeight =
                    20
            in
            Block.getLabelHeights [ "a", "b" ]
                ([ ( "a"
                   , { label = dummyElement { x = 0, y = 0, width = 100, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 100, height = startingHeight }
                     }
                   )
                 , ( "b"
                   , { label = dummyElement { x = 0, y = 20, width = 100, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 100, height = startingHeight }
                     }
                   )
                 ]
                    |> Dict.fromList
                )
                |> Expect.equal
                    ([ ( "a"
                       , { totalHeight = startingHeight + defaultArrowHeight + balloonOffset
                         , arrowHeight = defaultArrowHeight
                         }
                       )
                     , ( "b"
                       , { totalHeight = startingHeight + defaultArrowHeight + balloonOffset
                         , arrowHeight = defaultArrowHeight
                         }
                       )
                     ]
                        |> Dict.fromList
                    )
    , test "with multiple overlapping labels on different lines, positions elements correctly" <|
        \() ->
            let
                startingHeight =
                    20
            in
            Block.getLabelHeights [ "a", "b", "c" ]
                ([ --  A is the second-widest element
                   ( "a"
                   , { label = dummyElement { x = 0, y = 0, width = 200, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 200, height = startingHeight }
                     }
                   )
                 , -- B is the narrowest element
                   ( "b"
                   , { label = dummyElement { x = 0, y = 0, width = 100, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 100, height = startingHeight }
                     }
                   )
                 , -- C is the widest element and it is also on a new line by itself
                   ( "c"
                   , { label = dummyElement { x = 0, y = 20, width = 300, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 300, height = startingHeight }
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
                       , { totalHeight = 1 * startingHeight + defaultArrowHeight + balloonOffset
                         , arrowHeight =
                            -- C is on a new line, so it goes back to default positioning.
                            defaultArrowHeight
                         }
                       )
                     ]
                        |> Dict.fromList
                    )
    , test "with multiple non-overlapping labels on different lines, positions elements correctly" <|
        \() ->
            let
                startingHeight =
                    20
            in
            Block.getLabelHeights [ "a", "b", "c" ]
                ([ --  A is the second-widest element
                   ( "a"
                   , { label = dummyElement { x = 0, y = 0, width = 200, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 200, height = startingHeight }
                     }
                   )
                 , -- B is the narrowest element
                   ( "b"
                   , { label = dummyElement { x = 201, y = 0, width = 100, height = 100 }
                     , labelContent = dummyElement { x = 201, y = 0, width = 100, height = startingHeight }
                     }
                   )
                 , -- C is the widest element and it is also on a new line by itself
                   ( "c"
                   , { label = dummyElement { x = 0, y = 20, width = 300, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 300, height = startingHeight }
                     }
                   )
                 ]
                    |> Dict.fromList
                )
                |> Expect.equal
                    ([ ( "a"
                       , { totalHeight = 1 * startingHeight + defaultArrowHeight + balloonOffset
                         , arrowHeight = defaultArrowHeight
                         }
                       )
                     , ( "b"
                       , { totalHeight = 1 * startingHeight + defaultArrowHeight + balloonOffset
                         , arrowHeight = defaultArrowHeight
                         }
                       )
                     , ( "c"
                       , { totalHeight = 1 * startingHeight + defaultArrowHeight + balloonOffset
                         , arrowHeight = defaultArrowHeight
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
