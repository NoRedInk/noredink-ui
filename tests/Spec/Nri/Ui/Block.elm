module Spec.Nri.Ui.Block exposing (spec)

import Accessibility.Aria as Aria
import Browser.Dom exposing (Element)
import Dict
import Expect
import Html.Attributes as Attributes
import Html.Styled
import Nri.Ui.Block.V6 as Block
import Spec.PseudoElements exposing (hasAfter, hasBefore)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "Nri.Ui.Block"
        [ describe "content" contentSpec
        , describe "labelId" labelIdSpec
        , describe "labelMarkdown" labelMarkdownSpec
        , describe "getLabelPositions" getLabelPositionsSpec
        , describe "renderReadAloud" renderReadAloudTests
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
            [ Block.content (Block.phrase "Yo hello" ++ [ Block.blank { widthInChars = 8 } ]) ]
                |> toQuery
                |> Query.has [ Selector.text "Yo", Selector.text "blank" ]
    , test "content with blankWithQuestionBox" <|
        \() ->
            [ Block.content [ Block.blank { widthInChars = 8 } ] ]
                |> toQuery
                |> Query.has
                    [ Selector.all
                        [ Selector.containing [ Selector.text "blank" ]
                        ]
                    ]
    , test "content with wordWithId" <|
        \() ->
            [ Block.content (Block.phrase "word") ]
                |> toQuery
                |> Query.has
                    [ Selector.all
                        [ Selector.containing [ Selector.text "word" ]
                        ]
                    ]
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


labelMarkdownSpec : List Test
labelMarkdownSpec =
    [ test "An emphasized word with a markdown label" <|
        \_ ->
            [ Block.plaintext "Hello there"
            , Block.emphasize
            , Block.labelId "yo-id"
            , Block.label "This **is** markdown"
            ]
                |> toQuery
                |> Expect.all
                    [ -- The rendered markdown label content should be in a paragraph tag.
                      -- It should be hidden from SR users, since the label information
                      -- is conveyed another way.
                      Query.has
                        [ Selector.attribute (Aria.hidden True)
                        , Selector.containing
                            [ Selector.text "This "
                            ]
                        , Selector.containing
                            [ Selector.tag "strong"
                            , Selector.text "is"
                            ]
                        , Selector.containing
                            [ Selector.text " markdown"
                            ]
                        ]

                    -- The before and after elements that convey the mark type to AT users
                    -- should not include markdown (e.g., no asterisks)
                    , hasBefore "start This is markdown" "Hello"
                    , hasAfter "end This is markdown" "there"
                    ]
    ]


toQuery : List (Block.Attribute a) -> Query.Single a
toQuery block =
    Block.view block
        |> Html.Styled.toUnstyled
        |> Query.fromHtml


getLabelPositionsSpec : List Test
getLabelPositionsSpec =
    [ test "without any measurements, does not specify heights" <|
        \() ->
            Block.getLabelPositions Dict.empty
                |> Expect.equal Dict.empty
    , test "with a measured label, specifies the default heights" <|
        \() ->
            let
                startingHeight =
                    20
            in
            Block.getLabelPositions
                (Dict.singleton "a"
                    { label = dummyElement { x = 0, y = 0, width = 100, height = 100 }
                    , labelContent = dummyElement { x = 0, y = 0, width = 100, height = startingHeight }
                    }
                )
                |> Expect.equal
                    (Dict.singleton "a"
                        { totalHeight = startingHeight + defaultArrowHeight
                        , arrowHeight = defaultArrowHeight
                        , zIndex = 0
                        , xOffset = 0
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
            Block.getLabelPositions
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
                       , { totalHeight = aStartingHeight + defaultArrowHeight
                         , arrowHeight = defaultArrowHeight
                         , zIndex = 1
                         , xOffset = 0
                         }
                       )
                     , ( "b"
                       , { totalHeight = aStartingHeight + bStartingHeight + defaultArrowHeight + balloonOffset
                         , arrowHeight = aStartingHeight + defaultArrowHeight + balloonOffset
                         , zIndex = 0
                         , xOffset = 0
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
            Block.getLabelPositions
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
                       , { totalHeight = aStartingHeight + defaultArrowHeight
                         , arrowHeight = defaultArrowHeight
                         , zIndex = 0
                         , xOffset = 0
                         }
                       )
                     , ( "b"
                       , { totalHeight = bStartingHeight + defaultArrowHeight
                         , arrowHeight = defaultArrowHeight
                         , zIndex = 0
                         , xOffset = 0
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
            Block.getLabelPositions
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
                            startingHeight + defaultArrowHeight + balloonOffset
                         , zIndex = 1
                         , xOffset = 0
                         }
                       )
                     , ( "b"
                       , { totalHeight = 1 * startingHeight + defaultArrowHeight
                         , arrowHeight = defaultArrowHeight
                         , zIndex = 2
                         , xOffset = 0
                         }
                       )
                     , ( "c"
                       , { totalHeight = 3 * startingHeight + defaultArrowHeight + 2 * balloonOffset
                         , arrowHeight =
                            -- C is positioned on top of A.
                            2 * startingHeight + defaultArrowHeight + 2 * balloonOffset
                         , zIndex = 0
                         , xOffset = 0
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
            Block.getLabelPositions
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
                         { totalHeight = 1 * startingHeight + defaultArrowHeight
                         , arrowHeight = defaultArrowHeight
                         , zIndex = 0
                         , xOffset = 0
                         }
                       )
                     , -- B is positioned under C
                       ( "b"
                       , { totalHeight = 1 * startingHeight + defaultArrowHeight
                         , arrowHeight = defaultArrowHeight
                         , zIndex = 1
                         , xOffset = 0
                         }
                       )
                     , -- C is positioned over B
                       ( "c"
                       , { totalHeight = 2 * startingHeight + defaultArrowHeight + balloonOffset
                         , arrowHeight = 1 * startingHeight + defaultArrowHeight + balloonOffset
                         , zIndex = 0
                         , xOffset = 0
                         }
                       )
                     ]
                        |> Dict.fromList
                    )
    , test "Component Catalog example regression test: 3 labels on 1 line, with 1 overlap to resolve" <|
        \() ->
            let
                withElement element =
                    { scene = { width = 497, height = 1974 }
                    , viewport = { x = 0, y = 733, width = 497, height = 373 }
                    , element = element
                    }
            in
            Block.getLabelPositions
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
                    ([ ( "prepositionId", { arrowHeight = 8, totalHeight = 32, zIndex = 1, xOffset = 0 } )
                     , ( "directObjectId", { arrowHeight = 36, totalHeight = 60, zIndex = 0, xOffset = 0 } )
                     , ( "subjectId", { arrowHeight = 8, totalHeight = 32, zIndex = 0, xOffset = 0 } )
                     , ( "editorsNoteId", { arrowHeight = 8, totalHeight = 32, zIndex = 0, xOffset = 0 } )
                     ]
                        |> Dict.fromList
                    )
    , test "with overlapping labels on different lines, specifies the default heights" <|
        \() ->
            let
                startingHeight =
                    20
            in
            Block.getLabelPositions
                ([ ( "a"
                   , { label = dummyElement { x = 0, y = 0, width = 100, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 100, height = startingHeight }
                     }
                   )
                 , ( "b"
                   , { label = dummyElement { x = 0, y = 150, width = 100, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 100, height = startingHeight }
                     }
                   )
                 ]
                    |> Dict.fromList
                )
                |> Expect.equal
                    ([ ( "a"
                       , { totalHeight = startingHeight + defaultArrowHeight
                         , arrowHeight = defaultArrowHeight
                         , zIndex = 0
                         , xOffset = 0
                         }
                       )
                     , ( "b"
                       , { totalHeight = startingHeight + defaultArrowHeight
                         , arrowHeight = defaultArrowHeight
                         , zIndex = 0
                         , xOffset = 0
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
            Block.getLabelPositions
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
                   , { label = dummyElement { x = 0, y = 150, width = 300, height = 100 }
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
                            startingHeight + defaultArrowHeight + balloonOffset
                         , zIndex = 0
                         , xOffset = 0
                         }
                       )
                     , ( "b"
                       , { totalHeight = 1 * startingHeight + defaultArrowHeight
                         , arrowHeight = defaultArrowHeight
                         , zIndex = 1
                         , xOffset = 0
                         }
                       )
                     , ( "c"
                       , { totalHeight = 1 * startingHeight + defaultArrowHeight
                         , arrowHeight =
                            -- C is on a new line, so it goes back to default positioning.
                            defaultArrowHeight
                         , zIndex = 0
                         , xOffset = 0
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
            Block.getLabelPositions
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
                   , { label = dummyElement { x = 0, y = 150, width = 300, height = 100 }
                     , labelContent = dummyElement { x = 0, y = 0, width = 300, height = startingHeight }
                     }
                   )
                 ]
                    |> Dict.fromList
                )
                |> Expect.equal
                    ([ ( "a"
                       , { totalHeight = 1 * startingHeight + defaultArrowHeight
                         , arrowHeight = defaultArrowHeight
                         , zIndex = 0
                         , xOffset = 0
                         }
                       )
                     , ( "b"
                       , { totalHeight = 1 * startingHeight + defaultArrowHeight
                         , arrowHeight = defaultArrowHeight
                         , zIndex = 0
                         , xOffset = 0
                         }
                       )
                     , ( "c"
                       , { totalHeight = 1 * startingHeight + defaultArrowHeight
                         , arrowHeight = defaultArrowHeight
                         , zIndex = 0
                         , xOffset = 0
                         }
                       )
                     ]
                        |> Dict.fromList
                    )
    , test "when a label extends horizontally outside the viewport to the left" <|
        \() ->
            let
                width =
                    300

                x =
                    -150
            in
            Block.getLabelPositions
                (Dict.singleton "a"
                    { label = dummyElement { x = x, y = 0, width = width, height = 100 }
                    , labelContent = dummyElement { x = x, y = 0, width = width, height = 100 }
                    }
                )
                |> Expect.equal
                    ([ ( "a"
                       , { arrowHeight = 8
                         , totalHeight = 108
                         , zIndex = 0
                         , xOffset = 150
                         }
                       )
                     ]
                        |> Dict.fromList
                    )
    , test "when a label extends horizontally outside the viewport to the right" <|
        \() ->
            let
                width =
                    300

                x =
                    viewportWidth - 10
            in
            Block.getLabelPositions
                (Dict.singleton "a"
                    { label = dummyElement { x = x, y = 0, width = width, height = 100 }
                    , labelContent = dummyElement { x = x, y = 0, width = width, height = 100 }
                    }
                )
                |> Expect.equal
                    ([ ( "a"
                       , { arrowHeight = 8
                         , totalHeight = 108
                         , zIndex = 0
                         , xOffset = -290
                         }
                       )
                     ]
                        |> Dict.fromList
                    )
    , test "regression test - apparently impossible label top should not prevent identifying lables as being on same line" <|
        \() ->
            Block.getLabelPositions
                (Dict.fromList
                    [ ( "173-display-element-labeled-2-5"
                      , { label = dummyElement { x = 350.984375, y = 212.5, width = 150, height = 53 }
                        , labelContent = dummyElement { x = 350.984375, y = 261.5, width = 150, height = 45 }
                        }
                      )
                    , ( "173-display-element-labeled-6-12"
                      , { label = dummyElement { x = 477.2421875, y = 212.5, width = 150, height = 102 }
                        , labelContent = dummyElement { x = 477.2421875, y = 212.5, width = 150, height = 45 }
                        }
                      )
                    ]
                )
                |> Expect.equal
                    ([ ( "173-display-element-labeled-2-5", { arrowHeight = 8, totalHeight = 53, xOffset = 0, zIndex = 1 } )
                     , ( "173-display-element-labeled-6-12", { arrowHeight = 57, totalHeight = 102, xOffset = 0, zIndex = 0 } )
                     ]
                        |> Dict.fromList
                    )
    ]


renderReadAloudTests : List Test
renderReadAloudTests =
    [ test "Simple Text" <|
        \_ ->
            Block.renderReadAloud [ Block.plaintext "Simple Text" ]
                |> Expect.equal "Simple Text"
    , test "Simple emphasize block" <|
        \_ ->
            Block.renderReadAloud [ Block.emphasize, Block.content (Block.phrase "Simple Text") ]
                |> Expect.equal "Simple Text"
    , test "Simple blank" <|
        \_ ->
            Block.renderReadAloud []
                |> Expect.equal "blank"
    , test "Bold markup is ignored" <|
        \_ ->
            Block.renderReadAloud
                [ Block.content
                    (List.concat
                        [ Block.phrase "Hello there "
                        , [ Block.bold (Block.phrase "Mr.") ]
                        , Block.phrase " President"
                        ]
                    )
                ]
                |> Expect.equal "Hello there Mr. President"
    , test "Italic markup is ignored" <|
        \_ ->
            Block.renderReadAloud
                [ Block.content
                    (List.concat
                        [ Block.phrase "Hello there "
                        , [ Block.italic (Block.phrase "Mr.") ]
                        , Block.phrase " President"
                        ]
                    )
                ]
                |> Expect.equal "Hello there Mr. President"
    , test "Blanks in content are read" <|
        \_ ->
            Block.renderReadAloud
                [ Block.content
                    (List.concat
                        [ Block.phrase "Hello there "
                        , [ Block.blank { widthInChars = 5 } ]
                        , Block.phrase " President"
                        ]
                    )
                ]
                |> Expect.equal "Hello there blank President"
    , test "Label text is ignored" <|
        \_ ->
            Block.renderReadAloud
                [ Block.emphasize
                , Block.plaintext "Hello there Mr. President"
                , Block.label "Read this"
                ]
                |> Expect.equal "Hello there Mr. President"
    ]


balloonOffset : Float
balloonOffset =
    4


defaultArrowHeight : Float
defaultArrowHeight =
    8


dummyElement : { x : Float, y : Float, width : Float, height : Float } -> Element
dummyElement element =
    { scene = { width = 1000, height = 1000 }
    , viewport = { x = 0, y = 0, width = viewportWidth, height = 500 }
    , element = element
    }


viewportWidth : Float
viewportWidth =
    1000
