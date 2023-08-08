module Nri.Ui.WhenFocusLeaves.V2 exposing
    ( onKeyDown, onKeyDownPreventDefault
    , toDecoder
    )

{-| Listen for when the focus leaves the area, and then do an action.

@docs onKeyDown, onKeyDownPreventDefault
@docs toDecoder

-}

import Accessibility.Styled as Html
import Accessibility.Styled.Key as Key
import Html.Styled.Events as Events
import Json.Decode as Decode exposing (Decoder)


{-| Use `WhenFocusLeaves.toDecoder` helper with the "keydown" event.
-}
onKeyDown :
    List (Key.Event msg)
    ->
        { firstIds : List String
        , lastIds : List String
        , tabBackAction : msg
        , tabForwardAction : msg
        }
    -> Html.Attribute msg
onKeyDown otherEventListeners config =
    Events.on "keydown" (toDecoder otherEventListeners config)


{-| Use `WhenFocusLeaves.toDecoder` helper with the "keydown" event and prevent default.
-}
onKeyDownPreventDefault :
    List (Key.Event msg)
    ->
        { firstIds : List String
        , lastIds : List String
        , tabBackAction : msg
        , tabForwardAction : msg
        }
    -> Html.Attribute msg
onKeyDownPreventDefault otherEventListeners config =
    Events.preventDefaultOn "keydown"
        (Decode.map (\e -> ( e, True ))
            (toDecoder otherEventListeners config)
        )


{-| Use this helper to add a focus watcher to an HTML element and define
what to do in reponse to tab keypresses in a part of the UI.

The ids referenced here are expected to correspond to elements in the container
we are adding the attribute to.

    import Accessibility.Styled.Key as Key
    import Nri.Ui.WhenFocusLeaves.V1 as WhenFocusLeaves
    import Html.Styled.Events as Events


    Events.on "keydown"
        (WhenFocusLeaves.toDecoder
            [ Key.escape CloseModal ]
            { firstIds = ["first-id"]
            , lastIds = ["last-id"]
            , tabBackAction = GoToLastId
            , tabForwardAction = GoToFirstId
            }
        )

-}
toDecoder :
    List (Key.Event msg)
    ->
        { firstIds : List String
        , lastIds : List String
        , tabBackAction : msg
        , tabForwardAction : msg
        }
    -> Decoder msg
toDecoder otherEventListeners { firstIds, lastIds, tabBackAction, tabForwardAction } =
    let
        keyDecoder : Decoder (Event msg)
        keyDecoder =
            Key.customOneOf
                (Key.tab Tab
                    :: Key.tabBack TabBack
                    :: List.map
                        (\e -> { msg = OtherKey e.msg, keyCode = e.keyCode, shiftKey = e.shiftKey })
                        otherEventListeners
                )

        applyKeyEvent ( elementId, event ) =
            -- if the user tabs back while on the first id,
            -- we execute the action
            if event == TabBack && List.member elementId firstIds then
                Decode.succeed tabBackAction

            else if event == Tab && List.member elementId lastIds then
                -- if the user tabs forward while on the last id,
                -- we want to wrap around to the first id.
                Decode.succeed tabForwardAction

            else
                case event of
                    OtherKey e ->
                        Decode.succeed e

                    _ ->
                        Decode.fail "No need to intercept the key press"
    in
    Decode.andThen applyKeyEvent
        (Decode.map2 (\a b -> ( a, b ))
            (Decode.at [ "target", "id" ] Decode.string)
            keyDecoder
        )


type Event msg
    = Tab
    | TabBack
    | OtherKey msg
