module Nri.Ui.WhenFocusLeaves.V1 exposing (toAttribute, toDecoder)

{-| Listen for when the focus leaves the area, and then do an action.

@docs toAttribute, toDecoder

-}

import Accessibility.Styled as Html
import Html.Styled.Events exposing (preventDefaultOn)
import Json.Decode as Decode exposing (Decoder)


{-| Attach this attribute to add a focus watcher to an HTML element and define
what to do in reponse to tab keypresses in a part of the UI.

The ids referenced here are expected to correspond to elements in the container
we are adding the attribute to.

NOTE: When needing to listen to multiple keys toDecoder should be used instead of toAttribute.

-}
toAttribute :
    { firstId : String
    , lastId : String
    , tabBackAction : msg
    , tabForwardAction : msg
    }
    -> Html.Attribute msg
toAttribute config =
    preventDefaultOn "keydown"
        (Decode.map (\msg -> ( msg, True )) (toDecoder config))


{-| Use this decoder to add a focus watcher to an HTML element and define
what to do in reponse to tab keypresses in a part of the UI.

The ids referenced here are expected to correspond to elements in the container
we are adding the attribute to.

NOTE: When needing to listen to multiple keys toDecoder should be used instead of toAttribute.

    import Accessibility.Styled.Key as Key

    Key.onKeyDown
        [ Key.escape CloseModal
        , toDecoder config
        ]

-}
toDecoder :
    { firstId : String
    , lastId : String
    , tabBackAction : msg
    , tabForwardAction : msg
    }
    -> Decoder msg
toDecoder { firstId, lastId, tabBackAction, tabForwardAction } =
    Decode.andThen
        (\( elementId, keyCode, shiftKey ) ->
            if keyCode == 9 then
                -- if the user tabs back while on the first id,
                -- we execute the action
                if elementId == firstId && shiftKey then
                    Decode.succeed tabBackAction

                else if elementId == lastId && not shiftKey then
                    -- if the user tabs forward while on the last id,
                    -- we want to wrap around to the first id.
                    Decode.succeed tabForwardAction

                else
                    Decode.fail "No need to intercept the key press"

            else
                Decode.fail "No need to intercept the key press"
        )
        decodeKeydown


decodeKeydown : Decoder ( String, Int, Bool )
decodeKeydown =
    Decode.map3 (\id keyCode shiftKey -> ( id, keyCode, shiftKey ))
        (Decode.at [ "target", "id" ] Decode.string)
        (Decode.field "keyCode" Decode.int)
        (Decode.field "shiftKey" Decode.bool)
