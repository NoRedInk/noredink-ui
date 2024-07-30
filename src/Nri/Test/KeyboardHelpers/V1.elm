module Nri.Test.KeyboardHelpers.V1 exposing
    ( pressKey, releaseKey
    , pressTab, pressTabBack, pressEsc, pressSpace, pressDownArrow, pressRightArrow, pressLeftArrow, pressShiftRight, pressShiftLeft, releaseRightArrow, releaseLeftArrow, releaseShiftRight, releaseShiftLeft, releaseShift
    )

{-| `KeyboardHelpers` provides a set of functions to simulate keyboard events for testing Elm programs.


# Basic helpers

@docs pressKey, releaseKey


# Common helpers

@docs pressTab, pressTabBack, pressEsc, pressSpace, pressDownArrow, pressRightArrow, pressLeftArrow, pressShiftRight, pressShiftLeft, releaseRightArrow, releaseLeftArrow, releaseShiftRight, releaseShiftLeft, releaseShift

-}

import Json.Encode as Encode
import ProgramTest
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (Selector)


{-| Simulate a "keydown" event on the given element.
-}
pressKey :
    { targetDetails : List ( String, Encode.Value )
    , keyCode : Int
    , shiftKey : Bool
    }
    -> List Selector
    -> ProgramTest.ProgramTest model msg effect
    -> ProgramTest.ProgramTest model msg effect
pressKey { targetDetails, keyCode, shiftKey } selectors =
    ProgramTest.simulateDomEvent
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
    { targetDetails : List ( String, Encode.Value )
    , keyCode : Int
    , shiftKey : Bool
    }
    -> List Selector
    -> ProgramTest.ProgramTest model msg effect
    -> ProgramTest.ProgramTest model msg effect
releaseKey { targetDetails, keyCode, shiftKey } selectors =
    ProgramTest.simulateDomEvent
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
    { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> ProgramTest.ProgramTest model msg effect
    -> ProgramTest.ProgramTest model msg effect
pressTab { targetDetails } =
    pressKey { targetDetails = targetDetails, keyCode = 9, shiftKey = False }


{-| Simulate a shift-tab key press on the given element.
-}
pressTabBack :
    { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> ProgramTest.ProgramTest model msg effect
    -> ProgramTest.ProgramTest model msg effect
pressTabBack { targetDetails } =
    pressKey { targetDetails = targetDetails, keyCode = 9, shiftKey = True }


{-| Simulate an escape key press on the given element.
-}
pressEsc :
    { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> ProgramTest.ProgramTest model msg effect
    -> ProgramTest.ProgramTest model msg effect
pressEsc { targetDetails } =
    pressKey { targetDetails = targetDetails, keyCode = 27, shiftKey = False }


{-| Simulate a spacebar key press on the given element.
-}
pressSpace :
    { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> ProgramTest.ProgramTest model msg effect
    -> ProgramTest.ProgramTest model msg effect
pressSpace { targetDetails } =
    pressKey { targetDetails = targetDetails, keyCode = 32, shiftKey = False }


{-| Simulate a down arrow key press on the given element.
-}
pressDownArrow :
    { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> ProgramTest.ProgramTest model msg effect
    -> ProgramTest.ProgramTest model msg effect
pressDownArrow { targetDetails } =
    pressKey { targetDetails = targetDetails, keyCode = 40, shiftKey = False }


{-| Simulate a right arrow key press on the given element.
-}
pressRightArrow :
    { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> ProgramTest.ProgramTest model msg effect
    -> ProgramTest.ProgramTest model msg effect
pressRightArrow { targetDetails } =
    pressKey { targetDetails = targetDetails, keyCode = 39, shiftKey = False }


{-| Simulate a left arrow key press on the given element.
-}
pressLeftArrow :
    { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> ProgramTest.ProgramTest model msg effect
    -> ProgramTest.ProgramTest model msg effect
pressLeftArrow { targetDetails } =
    pressKey { targetDetails = targetDetails, keyCode = 37, shiftKey = False }


{-| Simulate a right arrow key press with the shift key held down on the given element.
-}
pressShiftRight :
    { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> ProgramTest.ProgramTest model msg effect
    -> ProgramTest.ProgramTest model msg effect
pressShiftRight { targetDetails } =
    pressKey { targetDetails = targetDetails, keyCode = 39, shiftKey = True }


{-| Simulate a left arrow key press with the shift key held down on the given element.
-}
pressShiftLeft :
    { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> ProgramTest.ProgramTest model msg effect
    -> ProgramTest.ProgramTest model msg effect
pressShiftLeft { targetDetails } =
    pressKey { targetDetails = targetDetails, keyCode = 37, shiftKey = True }


{-| Simulate a right arrow key release on the given element.
-}
releaseRightArrow :
    { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> ProgramTest.ProgramTest model msg effect
    -> ProgramTest.ProgramTest model msg effect
releaseRightArrow { targetDetails } =
    releaseKey { targetDetails = targetDetails, keyCode = 39, shiftKey = False }


{-| Simulate a left arrow key release on the given element.
-}
releaseLeftArrow :
    { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> ProgramTest.ProgramTest model msg effect
    -> ProgramTest.ProgramTest model msg effect
releaseLeftArrow { targetDetails } =
    releaseKey { targetDetails = targetDetails, keyCode = 37, shiftKey = False }


{-| Simulate a right arrow key release with the shift key held down on the given element.
-}
releaseShiftRight :
    { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> ProgramTest.ProgramTest model msg effect
    -> ProgramTest.ProgramTest model msg effect
releaseShiftRight { targetDetails } =
    releaseKey { targetDetails = targetDetails, keyCode = 39, shiftKey = True }


{-| Simulate a left arrow key release with the shift key held down on the given element.
-}
releaseShiftLeft :
    { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> ProgramTest.ProgramTest model msg effect
    -> ProgramTest.ProgramTest model msg effect
releaseShiftLeft { targetDetails } =
    releaseKey { targetDetails = targetDetails, keyCode = 37, shiftKey = True }


{-| Simulat shift key being released while focusing on the given element.
-}
releaseShift :
    { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> ProgramTest.ProgramTest model msg effect
    -> ProgramTest.ProgramTest model msg effect
releaseShift { targetDetails } =
    releaseKey { targetDetails = targetDetails, keyCode = 16, shiftKey = False }
