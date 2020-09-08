module Nri.Ui.FocusTrap.V1 exposing
    ( FocusTrap(..)
    , toAttribute
    )

{-| Create a focus trap.

@docs FocusTrap, toAttributer

-}

import Accessibility.Styled as Html exposing (..)
import Accessibility.Styled.Key as Key
import Browser.Dom as Dom
import Browser.Events
import Html.Styled.Attributes as Attributes exposing (class, id)
import Html.Styled.Events as Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Task


type FocusTrap
    = FocusTrap { firstId : String, lastId : String }


toAttribute : (String -> msg) -> FocusTrap -> Html.Attribute msg
toAttribute focus trap =
    onTab <|
        \elementId shiftKey ->
            case trap of
                FocusTrap { firstId, lastId } ->
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

                            -- Will this preventDefault break anything
                            -- if the element is an input
                            -- or dropdown (if the element has key-based
                            -- behavior already)?
                            , preventDefault = True
                            , stopPropagation = False
                            }

                    else
                        Decode.fail "No need to intercept the key press"


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


decodeKeydown =
    Decode.map3 (\id keyCode shiftKey -> ( id, keyCode, shiftKey ))
        (Decode.at [ "target", "id" ] Decode.string)
        (Decode.field "keyCode" Decode.int)
        (Decode.field "shiftKey" Decode.bool)
