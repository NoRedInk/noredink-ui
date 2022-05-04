module Nri.Ui.WhenFocusLeaves.V1 exposing (toAttribute)

{-| Listen for when the focus leaves the area, and then do an action.

@docs toAttribute

-}

import Accessibility.Styled as Html
import Html.Styled.Events as Events
import Json.Decode as Decode exposing (Decoder)


{-| Attach this attribute to add a focus watcher to an HTML element and define
what to do in reponse to tab keypresses in a part of the UI.

The ids referenced here are expected to correspond to elements in the container
we are adding the attribute to.

-}
toAttribute :
    { firstId : String
    , lastId : String
    , tabBackAction : msg
    , tabForwardAction : msg
    }
    -> Html.Attribute msg
toAttribute { firstId, lastId, tabBackAction, tabForwardAction } =
    onTab <|
        \elementId shiftKey ->
            -- if the user tabs back while on the first id,
            -- we execute the action
            if elementId == firstId && shiftKey then
                Decode.succeed
                    { message = tabBackAction
                    , preventDefault = False
                    , stopPropagation = False
                    }

            else if elementId == lastId && not shiftKey then
                -- if the user tabs forward while on the last id,
                -- we want to wrap around to the first id.
                Decode.succeed
                    { message = tabForwardAction
                    , preventDefault = False
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
