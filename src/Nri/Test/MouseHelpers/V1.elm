module Nri.Test.MouseHelpers.V1 exposing
    ( click, mouseDown, mouseUp, mouseOver
    , cancelableMouseDown, cancelableMouseUp, cancelableMouseOver
    , Config
    )

{-| `MouseHelpers` provides a set of functions to simulate mouse events for testing Elm programs.


# Basic helpers

@docs click, mouseDown, mouseUp, mouseOver


# Cancelable Events

@docs cancelableMouseDown, cancelableMouseUp, cancelableMouseOver


# Config

@docs Config

-}

import Json.Encode as Encode


{-| A `Config` allow us to not depend strictly on elm-explorations/test and avh4/elm-program-test packages.
-}
type alias Config programTest selector querySingle =
    { programTest_simulateDomEvent : (querySingle -> querySingle) -> ( String, Encode.Value ) -> programTest -> programTest
    , query_find : List selector -> querySingle -> querySingle
    , event_click : ( String, Encode.Value )
    , event_mouseDown : ( String, Encode.Value )
    , event_mouseUp : ( String, Encode.Value )
    , event_mouseOver : ( String, Encode.Value )
    , event_custom : String -> Encode.Value -> ( String, Encode.Value )
    }


{-| Simulate a click event on elements that match the given selectors.
-}
click : Config programTest selector querySingle -> List selector -> programTest -> programTest
click config selectors =
    config.programTest_simulateDomEvent
        (config.query_find selectors)
        config.event_click


{-| Simulate a mouse down event on elements that match the given selectors.
-}
mouseDown : Config programTest selector querySingle -> List selector -> programTest -> programTest
mouseDown config selectors =
    config.programTest_simulateDomEvent
        (config.query_find selectors)
        config.event_mouseDown


{-| Simulate a mouse up event on elements that match the given selectors.
-}
mouseUp : Config programTest selector querySingle -> List selector -> programTest -> programTest
mouseUp config selectors =
    config.programTest_simulateDomEvent
        (config.query_find selectors)
        config.event_mouseUp


{-| Simulate a mouse over event on elements that match the given selectors.
-}
mouseOver : Config programTest selector querySingle -> List selector -> programTest -> programTest
mouseOver config selectors =
    config.programTest_simulateDomEvent
        (config.query_find selectors)
        config.event_mouseOver


{-| Simulate a cancelable mouse down event on elements that match the given selectors.
-}
cancelableMouseDown : Config programTest selector querySingle -> List selector -> programTest -> programTest
cancelableMouseDown config selectors =
    config.programTest_simulateDomEvent
        (config.query_find selectors)
        (config.event_custom
            "mousedown"
            (Encode.object [ ( "cancelable", Encode.bool True ) ])
        )


{-| Simulate a cancelable mouse up event on elements that match the given selectors.
-}
cancelableMouseUp : Config programTest selector querySingle -> List selector -> programTest -> programTest
cancelableMouseUp config selectors =
    config.programTest_simulateDomEvent
        (config.query_find selectors)
        (config.event_custom
            "mouseup"
            (Encode.object [ ( "cancelable", Encode.bool True ) ])
        )


{-| Simulate a cancelable mouse over event on elements that match the given selectors.
-}
cancelableMouseOver : Config programTest selector querySingle -> List selector -> programTest -> programTest
cancelableMouseOver config selectors =
    config.programTest_simulateDomEvent
        (config.query_find selectors)
        (config.event_custom
            "mouseover"
            (Encode.object [ ( "cancelable", Encode.bool True ) ])
        )
