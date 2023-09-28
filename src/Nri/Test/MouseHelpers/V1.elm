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
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (Selector)


type alias SimulateDomEvent msg programTest =
    (Query.Single msg -> Query.Single msg) -> ( String, Encode.Value ) -> programTest -> programTest


{-| Simulate a click event on elements that match the given selectors.
-}
click : SimulateDomEvent msg programTest -> List Selector -> programTest -> programTest
click simulateDomEvent selectors =
    simulateDomEvent
        (Query.find selectors)
        Event.click


{-| Simulate a mouse down event on elements that match the given selectors.
-}
mouseDown : SimulateDomEvent msg programTest -> List Selector -> programTest -> programTest
mouseDown simulateDomEvent selectors =
    simulateDomEvent
        (Query.find selectors)
        Event.mouseDown


{-| Simulate a mouse up event on elements that match the given selectors.
-}
mouseUp : SimulateDomEvent msg programTest -> List Selector -> programTest -> programTest
mouseUp simulateDomEvent selectors =
    simulateDomEvent
        (Query.find selectors)
        Event.mouseUp


{-| Simulate a mouse over event on elements that match the given selectors.
-}
mouseOver : SimulateDomEvent msg programTest -> List Selector -> programTest -> programTest
mouseOver simulateDomEvent selectors =
    simulateDomEvent
        (Query.find selectors)
        Event.mouseOver


{-| Simulate a cancelable mouse down event on elements that match the given selectors.
-}
cancelableMouseDown : SimulateDomEvent msg programTest -> List Selector -> programTest -> programTest
cancelableMouseDown simulateDomEvent selectors =
    simulateDomEvent
        (Query.find selectors)
        (Event.custom
            "mousedown"
            (Encode.object [ ( "cancelable", Encode.bool True ) ])
        )


{-| Simulate a cancelable mouse up event on elements that match the given selectors.
-}
cancelableMouseUp : SimulateDomEvent msg programTest -> List Selector -> programTest -> programTest
cancelableMouseUp simulateDomEvent selectors =
    simulateDomEvent
        (Query.find selectors)
        (Event.custom
            "mouseup"
            (Encode.object [ ( "cancelable", Encode.bool True ) ])
        )


{-| Simulate a cancelable mouse over event on elements that match the given selectors.
-}
cancelableMouseOver : SimulateDomEvent msg programTest -> List Selector -> programTest -> programTest
cancelableMouseOver simulateDomEvent selectors =
    simulateDomEvent
        (Query.find selectors)
        (Event.custom
            "mouseover"
            (Encode.object [ ( "cancelable", Encode.bool True ) ])
        )
