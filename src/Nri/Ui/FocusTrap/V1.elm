module Nri.Ui.FocusTrap.V1 exposing (FocusTrap, toAttribute)

{-| Create a focus trap.

@docs FocusTrap, toAttribute

-}

import Accessibility.Styled as Html
import Html.Styled.Events as Events
import Json.Decode as Decode exposing (Decoder)


{-| Defines how focus will wrap in reponse to tab keypresses in a part of the UI.

The ids referenced here are expected to correspond to elements in the container
we are applying the focus trap on.

-}
type alias FocusTrap msg =
    { firstId : String
    , lastId : String
    , focus : String -> msg
    }


{-| Attach this attribute to add a focus trap to an HTML element.
-}
toAttribute : FocusTrap msg -> Html.Attribute msg
toAttribute { firstId, lastId, focus } =
    onTab <|
        \elementId shiftKey ->
            -- if the user tabs back while on the first id,
            -- we want to wrap around to the last id.
            if elementId == firstId && shiftKey then
                Decode.succeed
                    { message = focus lastId
                    , preventDefault = True
                    , stopPropagation = False
                    }

            else if elementId == lastId && not shiftKey then
                -- if the user tabs forward while on the last id,
                -- we want to wrap around to the first id.
                Decode.succeed
                    { message = focus firstId
                    , preventDefault = True
                    , stopPropagation = False
                    }

            else
                Decode.fail "No need to intercept the key press"


onTab :
    (String
     -> Bool
     -> Decoder { message : msg, preventDefault : Bool, stopPropagation : Bool }
    )
    -> Html.Attribute msg
onTab do =
    Events.custom "keydown"
        (Decode.andThen
            (\( id, keyCode, shiftKey ) ->
                if keyCode == 9 then
                    do id shiftKey

                else
                    Decode.fail "No need to intercept the key press"
            )
            decodeKeydown
        )


decodeKeydown : Decoder ( String, Int, Bool )
decodeKeydown =
    Decode.map3 (\id keyCode shiftKey -> ( id, keyCode, shiftKey ))
        (Decode.at [ "target", "id" ] Decode.string)
        (Decode.field "keyCode" Decode.int)
        (Decode.field "shiftKey" Decode.bool)
