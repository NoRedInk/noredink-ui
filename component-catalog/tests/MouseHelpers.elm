module MouseHelpers exposing (..)

{-| Copied from Spec.MouseHelpers. At some point, we should open source these helpers as a separate package,
and then they'll be conveniently available as test dependencies for both noredink-ui elm.jsons.
-}

import Json.Encode as Encode
import ProgramTest exposing (ProgramTest)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (Selector)


click : List Selector -> ProgramTest model msg effect -> ProgramTest model msg effect
click selectors =
    ProgramTest.simulateDomEvent
        (Query.find selectors)
        Event.click


mouseDown : List Selector -> ProgramTest model msg effect -> ProgramTest model msg effect
mouseDown selectors =
    ProgramTest.simulateDomEvent
        (Query.find selectors)
        Event.mouseDown


mouseOver : List Selector -> ProgramTest model msg effect -> ProgramTest model msg effect
mouseOver selectors =
    ProgramTest.simulateDomEvent
        (Query.find selectors)
        Event.mouseOver


mouseUp : List Selector -> ProgramTest model msg effect -> ProgramTest model msg effect
mouseUp selectors =
    ProgramTest.simulateDomEvent
        (Query.find selectors)
        Event.mouseUp


cancelableMouseDown : List Selector -> ProgramTest model msg effect -> ProgramTest model msg effect
cancelableMouseDown selectors =
    ProgramTest.simulateDomEvent
        (Query.find selectors)
        (Event.custom
            "mousedown"
            (Encode.object [ ( "cancelable", Encode.bool True ) ])
        )


cancelableMouseOver : List Selector -> ProgramTest model msg effect -> ProgramTest model msg effect
cancelableMouseOver selectors =
    ProgramTest.simulateDomEvent
        (Query.find selectors)
        (Event.custom
            "mouseover"
            (Encode.object [ ( "cancelable", Encode.bool True ) ])
        )


cancelableMouseUp : List Selector -> ProgramTest model msg effect -> ProgramTest model msg effect
cancelableMouseUp selectors =
    ProgramTest.simulateDomEvent
        (Query.find selectors)
        (Event.custom
            "mouseup"
            (Encode.object [ ( "cancelable", Encode.bool True ) ])
        )
