module Browser.Events.Extra exposing (escape)

{-|

@docs escape

-}

import Browser.Events
import Html.Styled.Events as Events
import Json.Decode


{-| -}
escape : msg -> Sub msg
escape msg =
    Browser.Events.onKeyDown
        (Json.Decode.andThen
            (\keyCode ->
                if keyCode == 27 then
                    Json.Decode.succeed msg

                else
                    Json.Decode.fail "Not a match"
            )
            Events.keyCode
        )
