module Spec.KeyboardHelpers exposing (..)

import Json.Encode as Encode
import ProgramTest exposing (ProgramTest)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (Selector)


pressKey :
    { targetDetails : List ( String, Encode.Value )
    , keyCode : Int
    , shiftKey : Bool
    }
    -> List Selector
    -> ProgramTest model msg effect
    -> ProgramTest model msg effect
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


pressTabKey :
    { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> ProgramTest model msg effect
    -> ProgramTest model msg effect
pressTabKey { targetDetails } =
    pressKey { targetDetails = targetDetails, keyCode = 9, shiftKey = False }


pressEscKey :
    { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> ProgramTest model msg effect
    -> ProgramTest model msg effect
pressEscKey { targetDetails } =
    pressKey { targetDetails = targetDetails, keyCode = 27, shiftKey = False }


pressRightArrow :
    { targetDetails : List ( String, Encode.Value ) }
    -> List Selector
    -> ProgramTest model msg effect
    -> ProgramTest model msg effect
pressRightArrow { targetDetails } =
    pressKey { targetDetails = targetDetails, keyCode = 39, shiftKey = False }
