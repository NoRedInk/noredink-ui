port module Examples.HighlighterPort exposing (..)

{-| Start listening to events on a highlighter
-}


port highlighterInit : ( String, Bool, Bool ) -> Cmd msg


{-| Listen to mouseup/touchend events on the whole document, to stop highlighting.
-}
port highlighterTouchPointerRelease : (( String, String ) -> msg) -> Sub msg


{-| Listen to touch events, and get the element under the finger.
-}
port highlighterOnTouchEvent : (( String, String, Int ) -> msg) -> Sub msg
