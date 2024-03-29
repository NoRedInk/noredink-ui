module Nri.Test.MouseHelpers.V1 exposing
    ( click, mouseDown, mouseUp, mouseOver
    , cancelableMouseDown, cancelableMouseUp, cancelableMouseOver
    )

{-| `MouseHelpers` provides a set of functions to simulate mouse events for testing Elm programs.


# Basic helpers

@docs click, mouseDown, mouseUp, mouseOver


# Cancelable Events

@docs cancelableMouseDown, cancelableMouseUp, cancelableMouseOver

-}

import Json.Encode as Encode
import ProgramTest exposing (ProgramTest)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


{-| Simulate a click event on elements that match the given selectors.
-}
click : List Selector -> ProgramTest a b c -> ProgramTest a b c
click selectors =
    ProgramTest.simulateDomEvent
        (Query.find selectors)
        Event.click


{-| Simulate a mouse down event on elements that match the given selectors.
-}
mouseDown : List Selector -> ProgramTest a b c -> ProgramTest a b c
mouseDown selectors =
    ProgramTest.simulateDomEvent
        (Query.find selectors)
        Event.mouseDown


{-| Simulate a mouse up event on elements that match the given selectors.
-}
mouseUp : List Selector -> ProgramTest a b c -> ProgramTest a b c
mouseUp selectors =
    ProgramTest.simulateDomEvent
        (Query.find selectors)
        Event.mouseUp


{-| Simulate a mouse over event on elements that match the given selectors.
-}
mouseOver : List Selector -> ProgramTest a b c -> ProgramTest a b c
mouseOver selectors =
    ProgramTest.simulateDomEvent
        (Query.find selectors)
        Event.mouseOver


{-| Simulate a cancelable mouse down event on elements that match the given selectors.
-}
cancelableMouseDown : List Selector -> ProgramTest a b c -> ProgramTest a b c
cancelableMouseDown selectors =
    ProgramTest.simulateDomEvent
        (Query.find selectors)
        (Event.custom
            "mousedown"
            (Encode.object [ ( "cancelable", Encode.bool True ) ])
        )


{-| Simulate a cancelable mouse up event on elements that match the given selectors.
-}
cancelableMouseUp : List Selector -> ProgramTest a b c -> ProgramTest a b c
cancelableMouseUp selectors =
    ProgramTest.simulateDomEvent
        (Query.find selectors)
        (Event.custom
            "mouseup"
            (Encode.object [ ( "cancelable", Encode.bool True ) ])
        )


{-| Simulate a cancelable mouse over event on elements that match the given selectors.
-}
cancelableMouseOver : List Selector -> ProgramTest a b c -> ProgramTest a b c
cancelableMouseOver selectors =
    ProgramTest.simulateDomEvent
        (Query.find selectors)
        (Event.custom
            "mouseover"
            (Encode.object [ ( "cancelable", Encode.bool True ) ])
        )
