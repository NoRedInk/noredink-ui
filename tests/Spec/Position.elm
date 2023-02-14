module Spec.Position exposing (spec)

{-| -}

import Browser.Dom exposing (Element)
import Expect
import Position
import Test exposing (..)


spec : Test
spec =
    describe "Position"
        [ describe "xOffsetPx" xOffsetPxSpec
        , describe "xOffsetPxAgainstAncestor" xOffsetPxSpecAgainstContainer
        ]


xOffsetPxSpec : List Test
xOffsetPxSpec =
    [ test "when the element is in line with the left edge of the viewport, does not shift" <|
        \() ->
            element { viewportX = 0, viewportWidth = 100, elementX = 0, elementWidth = 50 }
                |> Position.xOffsetPx
                |> Expect.equal 0
    , test "when the element is in line with the right edge of the viewport, does not shift" <|
        \() ->
            element { viewportX = 0, viewportWidth = 100, elementX = 50, elementWidth = 50 }
                |> Position.xOffsetPx
                |> Expect.equal 0
    , test "when the viewport cuts off the element on the left side, shift to the right" <|
        \() ->
            let
                cutOffAmount =
                    100
            in
            element { viewportX = 0, viewportWidth = 500, elementX = -cutOffAmount, elementWidth = 200 }
                |> Position.xOffsetPx
                |> Expect.equal cutOffAmount
    , test "when the viewport cuts off the element on the right side, shift to the left" <|
        \() ->
            let
                cutOffAmount =
                    100
            in
            element { viewportX = 0, viewportWidth = 0, elementX = 0, elementWidth = cutOffAmount }
                |> Position.xOffsetPx
                |> Expect.equal -cutOffAmount
    , test "when the viewport cuts off the element on both sides, align to the left side of the viewport" <|
        \() ->
            element { viewportX = 0, viewportWidth = 100, elementX = -100, elementWidth = 300 }
                |> Position.xOffsetPx
                |> Expect.equal 100
    ]


element : { viewportX : Float, viewportWidth : Float, elementX : Float, elementWidth : Float } -> Element
element { viewportX, viewportWidth, elementWidth, elementX } =
    { scene = { width = 1000, height = 1000 }
    , viewport = { x = viewportX, y = 0, width = viewportWidth, height = 500 }
    , element = { x = elementX, width = elementWidth, y = 0, height = 10 }
    }


xOffsetPxSpecAgainstContainer : List Test
xOffsetPxSpecAgainstContainer =
    [ test "When the element is in line with the left edge of its container, does not shift" <|
        \() ->
            { element = measurement { x = 0, width = 200 }
            , container = measurement { x = 0, width = 500 }
            }
                |> Position.xOffsetPxAgainstContainer
                |> Expect.equal 0
    , test "when the element is in line with the right edge of its container, does not shift" <|
        \() ->
            { element = measurement { x = 300, width = 200 }
            , container = measurement { x = 0, width = 500 }
            }
                |> Position.xOffsetPxAgainstContainer
                |> Expect.equal 0
    , test "when the element overflows its container on the left side, shift to the right" <|
        \() ->
            { element = measurement { x = -100, width = 200 }
            , container = measurement { x = 0, width = 500 }
            }
                |> Position.xOffsetPxAgainstContainer
                |> Expect.equal 100
    , test "when the element overflows its container on the right side, shift to the left" <|
        \() ->
            { element = measurement { x = 400, width = 200 }
            , container = measurement { x = 0, width = 500 }
            }
                |> Position.xOffsetPxAgainstContainer
                |> Expect.equal -100
    , test "when the element overflows its container on both sides, align to the left side of the container" <|
        \() ->
            { element = measurement { x = -100, width = 300 }
            , container = measurement { x = 0, width = 100 }
            }
                |> Position.xOffsetPxAgainstContainer
                |> Expect.equal 100
    ]


measurement : { x : Float, width : Float } -> Element
measurement { width, x } =
    { scene = { width = 1000, height = 1000 }
    , viewport = { x = 0, y = 0, width = 1000, height = 2000 }
    , element = { x = x, width = width, y = 0, height = 10 }
    }
