module Nri.Test.KeyboardHelpers.V1 exposing
    ( pressKey, releaseKey
    , pressTab, pressTabBack, pressEsc, pressSpace, pressDownArrow, pressRightArrow, pressLeftArrow, pressShiftRight, pressShiftLeft, releaseRightArrow, releaseLeftArrow, releaseShiftRight, releaseShiftLeft
    , Config
    )

{-| `KeyboardHelpers` provides a set of functions to simulate keyboard events for testing Elm programs.


# Basic helpers

@docs pressKey, releaseKey


# Common helpers

@docs pressTab, pressTabBack, pressEsc, pressSpace, pressDownArrow, pressRightArrow, pressLeftArrow, pressShiftRight, pressShiftLeft, releaseRightArrow, releaseLeftArrow, releaseShiftRight, releaseShiftLeft


# Config

@docs Config

-}

import Json.Encode as Encode


{-| A `Config` allow us to not depend strictly on elm-explorations/test and avh4/elm-program-test packages.
-}
type alias Config programTest selector querySingle =
    { programTest_simulateDomEvent : (querySingle -> querySingle) -> ( String, Encode.Value ) -> programTest -> programTest
    , query_find : List selector -> querySingle -> querySingle
    , event_custom : String -> Encode.Value -> ( String, Encode.Value )
    }


{-| Simulate a "keydown" event on the given element.
-}
pressKey :
    Config programTest selector querySingle
    ->
        { targetDetails : List ( String, Encode.Value )
        , keyCode : Int
        , shiftKey : Bool
        }
    -> List selector
    -> programTest
    -> programTest
pressKey config { targetDetails, keyCode, shiftKey } selectors =
    config.programTest_simulateDomEvent
        (config.query_find selectors)
        (config.event_custom
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
    Config programTest selector querySingle
    ->
        { targetDetails : List ( String, Encode.Value )
        , keyCode : Int
        , shiftKey : Bool
        }
    -> List selector
    -> programTest
    -> programTest
releaseKey config { targetDetails, keyCode, shiftKey } selectors =
    config.programTest_simulateDomEvent
        (config.query_find selectors)
        (config.event_custom
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
    Config programTest selector querySingle
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List selector
    -> programTest
    -> programTest
pressTab config { targetDetails } =
    pressKey config { targetDetails = targetDetails, keyCode = 9, shiftKey = False }


{-| Simulate a shift-tab key press on the given element.
-}
pressTabBack :
    Config programTest selector querySingle
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List selector
    -> programTest
    -> programTest
pressTabBack config { targetDetails } =
    pressKey config { targetDetails = targetDetails, keyCode = 9, shiftKey = True }


{-| Simulate an escape key press on the given element.
-}
pressEsc :
    Config programTest selector querySingle
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List selector
    -> programTest
    -> programTest
pressEsc config { targetDetails } =
    pressKey config { targetDetails = targetDetails, keyCode = 27, shiftKey = False }


{-| Simulate a spacebar key press on the given element.
-}
pressSpace :
    Config programTest selector querySingle
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List selector
    -> programTest
    -> programTest
pressSpace config { targetDetails } =
    pressKey config { targetDetails = targetDetails, keyCode = 32, shiftKey = False }


{-| Simulate a down arrow key press on the given element.
-}
pressDownArrow :
    Config programTest selector querySingle
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List selector
    -> programTest
    -> programTest
pressDownArrow config { targetDetails } =
    pressKey config { targetDetails = targetDetails, keyCode = 40, shiftKey = False }


{-| Simulate a right arrow key press on the given element.
-}
pressRightArrow :
    Config programTest selector querySingle
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List selector
    -> programTest
    -> programTest
pressRightArrow config { targetDetails } =
    pressKey config { targetDetails = targetDetails, keyCode = 39, shiftKey = False }


{-| Simulate a left arrow key press on the given element.
-}
pressLeftArrow :
    Config programTest selector querySingle
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List selector
    -> programTest
    -> programTest
pressLeftArrow config { targetDetails } =
    pressKey config { targetDetails = targetDetails, keyCode = 37, shiftKey = False }


{-| Simulate a right arrow key press with the shift key held down on the given element.
-}
pressShiftRight :
    Config programTest selector querySingle
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List selector
    -> programTest
    -> programTest
pressShiftRight config { targetDetails } =
    pressKey config { targetDetails = targetDetails, keyCode = 39, shiftKey = True }


{-| Simulate a left arrow key press with the shift key held down on the given element.
-}
pressShiftLeft :
    Config programTest selector querySingle
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List selector
    -> programTest
    -> programTest
pressShiftLeft config { targetDetails } =
    pressKey config { targetDetails = targetDetails, keyCode = 37, shiftKey = True }


{-| Simulate a right arrow key release on the given element.
-}
releaseRightArrow :
    Config programTest selector querySingle
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List selector
    -> programTest
    -> programTest
releaseRightArrow config { targetDetails } =
    releaseKey config { targetDetails = targetDetails, keyCode = 39, shiftKey = False }


{-| Simulate a left arrow key release on the given element.
-}
releaseLeftArrow :
    Config programTest selector querySingle
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List selector
    -> programTest
    -> programTest
releaseLeftArrow config { targetDetails } =
    releaseKey config { targetDetails = targetDetails, keyCode = 37, shiftKey = False }


{-| Simulate a right arrow key release with the shift key held down on the given element.
-}
releaseShiftRight :
    Config programTest selector querySingle
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List selector
    -> programTest
    -> programTest
releaseShiftRight config { targetDetails } =
    releaseKey config { targetDetails = targetDetails, keyCode = 39, shiftKey = True }


{-| Simulate a left arrow key release with the shift key held down on the given element.
-}
releaseShiftLeft :
    Config programTest selector querySingle
    -> { targetDetails : List ( String, Encode.Value ) }
    -> List selector
    -> programTest
    -> programTest
releaseShiftLeft config { targetDetails } =
    releaseKey config { targetDetails = targetDetails, keyCode = 37, shiftKey = True }
