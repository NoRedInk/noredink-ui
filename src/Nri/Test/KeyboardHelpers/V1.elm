module Nri.Test.KeyboardHelpers.V1 exposing
    ( pressKey, releaseKey
    , pressTab, pressTabBack, pressEsc, pressSpace, pressDownArrow, pressRightArrow, pressLeftArrow, pressShiftRight, pressShiftLeft, releaseRightArrow, releaseLeftArrow, releaseShiftRight, releaseShiftLeft
    )

{-| `KeyboardHelpers` provides a set of functions to simulate keyboard events for testing Elm programs.


# Basic helpers

@docs pressKey, releaseKey


# Common helpers

@docs pressTab, pressTabBack, pressEsc, pressSpace, pressDownArrow, pressRightArrow, pressLeftArrow, pressShiftRight, pressShiftLeft, releaseRightArrow, releaseLeftArrow, releaseShiftRight, releaseShiftLeft

-}

import Json.Encode as Encode
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (Selector)


type alias SimulateDomEvent msg programTest =
    (Query.Single msg -> Query.Single msg) -> ( String, Encode.Value ) -> programTest -> programTest


{-| Simulate a "keydown" event on the given element.
-}
pressKey :
    SimulateDomEvent msg programTest
    ->
        { targetDetails : List ( String, Encode.Value )
        , keyCode : Int
        , shiftKey : Bool
        }
    -> List Selector
    -> programTest
    -> programTest
pressKey simulateDomEvent { targetDetails, keyCode, shiftKey } selectors =
    simulateDomEvent
        (Query.find selectors)
        (Event.custom
            "keydown"
            (Encode.object
                [ ( "keyCode", Encode.int keyCode )
                , ( "shiftKey", Encode.bool shiftKey )
                , ( "target"
                  , Encode.object targetDetails
                  )
                ]
            )
        )


{-| Simulate a "keyup" event on the given element.
-}
releaseKey :
    SimulateDomEvent msg programTest
    ->
        { targetDetails : List ( String, Encode.Value )
        , keyCode : Int
        , shiftKey : Bool
        }
    -> List Selector
    -> programTest
    -> programTest
releaseKey simulateDomEvent { targetDetails, keyCode, shiftKey } selectors =
    simulateDomEvent
        (Query.find selectors)
        (Event.custom
            "keyup"
            (Encode.object
                [ ( "keyCode", Encode.int keyCode )
                , ( "shiftKey", Encode.bool shiftKey )
                , ( "target"
                  , Encode.object targetDetails
                  )
                ]
            )
        )


{-| Simulate a tab key press on the given element.
-}
pressTab :
    SimulateDomEvent msg programTest
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> programTest
    -> programTest
pressTab simulateDomEvent { targetDetails } =
    pressKey simulateDomEvent { targetDetails = targetDetails, keyCode = 9, shiftKey = False }


{-| Simulate a shift-tab key press on the given element.
-}
pressTabBack :
    SimulateDomEvent msg programTest
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> programTest
    -> programTest
pressTabBack simulateDomEvent { targetDetails } =
    pressKey simulateDomEvent { targetDetails = targetDetails, keyCode = 9, shiftKey = True }


{-| Simulate an escape key press on the given element.
-}
pressEsc :
    SimulateDomEvent msg programTest
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> programTest
    -> programTest
pressEsc simulateDomEvent { targetDetails } =
    pressKey simulateDomEvent { targetDetails = targetDetails, keyCode = 27, shiftKey = False }


{-| Simulate a spacebar key press on the given element.
-}
pressSpace :
    SimulateDomEvent msg programTest
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> programTest
    -> programTest
pressSpace simulateDomEvent { targetDetails } =
    pressKey simulateDomEvent { targetDetails = targetDetails, keyCode = 32, shiftKey = False }


{-| Simulate a down arrow key press on the given element.
-}
pressDownArrow :
    SimulateDomEvent msg programTest
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> programTest
    -> programTest
pressDownArrow simulateDomEvent { targetDetails } =
    pressKey simulateDomEvent { targetDetails = targetDetails, keyCode = 40, shiftKey = False }


{-| Simulate a right arrow key press on the given element.
-}
pressRightArrow :
    SimulateDomEvent msg programTest
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> programTest
    -> programTest
pressRightArrow simulateDomEvent { targetDetails } =
    pressKey simulateDomEvent { targetDetails = targetDetails, keyCode = 39, shiftKey = False }


{-| Simulate a left arrow key press on the given element.
-}
pressLeftArrow :
    SimulateDomEvent msg programTest
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> programTest
    -> programTest
pressLeftArrow simulateDomEvent { targetDetails } =
    pressKey simulateDomEvent { targetDetails = targetDetails, keyCode = 37, shiftKey = False }


{-| Simulate a right arrow key press with the shift key held down on the given element.
-}
pressShiftRight :
    SimulateDomEvent msg programTest
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> programTest
    -> programTest
pressShiftRight simulateDomEvent { targetDetails } =
    pressKey simulateDomEvent { targetDetails = targetDetails, keyCode = 39, shiftKey = True }


{-| Simulate a left arrow key press with the shift key held down on the given element.
-}
pressShiftLeft :
    SimulateDomEvent msg programTest
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> programTest
    -> programTest
pressShiftLeft simulateDomEvent { targetDetails } =
    pressKey simulateDomEvent { targetDetails = targetDetails, keyCode = 37, shiftKey = True }


{-| Simulate a right arrow key release on the given element.
-}
releaseRightArrow :
    SimulateDomEvent msg programTest
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> programTest
    -> programTest
releaseRightArrow simulateDomEvent { targetDetails } =
    releaseKey simulateDomEvent { targetDetails = targetDetails, keyCode = 39, shiftKey = False }


{-| Simulate a left arrow key release on the given element.
-}
releaseLeftArrow :
    SimulateDomEvent msg programTest
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> programTest
    -> programTest
releaseLeftArrow simulateDomEvent { targetDetails } =
    releaseKey simulateDomEvent { targetDetails = targetDetails, keyCode = 37, shiftKey = False }


{-| Simulate a right arrow key release with the shift key held down on the given element.
-}
releaseShiftRight :
    SimulateDomEvent msg programTest
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> programTest
    -> programTest
releaseShiftRight simulateDomEvent { targetDetails } =
    releaseKey simulateDomEvent { targetDetails = targetDetails, keyCode = 39, shiftKey = True }


{-| Simulate a left arrow key release with the shift key held down on the given element.
-}
releaseShiftLeft :
    SimulateDomEvent msg programTest
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> programTest
    -> programTest
releaseShiftLeft simulateDomEvent { targetDetails } =
    releaseKey simulateDomEvent { targetDetails = targetDetails, keyCode = 37, shiftKey = True }
