module Nri.Ui.Html.V3 exposing
    ( viewJust, viewIf
    , onKeyUp, defaultOptions
    )

{-|

@docs viewJust, viewIf
@docs onKeyUp, defaultOptions

-}

import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Events as Events exposing (..)
import Json.Decode


{-| -}
viewIf : (() -> Html msg) -> Bool -> Html msg
viewIf view condition =
    case condition of
        True ->
            view ()

        False ->
            Html.text ""


{-| View value of if `Maybe` is a `Just`, otherwise show nothing.
-}
viewJust : (a -> Html msg) -> Maybe a -> Html msg
viewJust view maybe =
    case maybe of
        Just whatever ->
            view whatever

        Nothing ->
            Html.text ""


{-| -}
defaultOptions : { preventDefault : Bool, stopPropagation : Bool }
defaultOptions =
    { preventDefault = False
    , stopPropagation = False
    }


{-| Convert a keycode into a message on keyup
-}
onKeyUp : { preventDefault : Bool, stopPropagation : Bool } -> (Int -> Maybe a) -> Attribute a
onKeyUp options toMaybeMsg =
    keyCode
        |> Json.Decode.andThen
            (\keyCode_ ->
                keyCode_
                    |> toMaybeMsg
                    |> Maybe.map Json.Decode.succeed
                    |> Maybe.withDefault (Json.Decode.fail (String.fromInt keyCode_))
            )
        |> Json.Decode.map
            (\data ->
                { message = data
                , stopPropagation = options.stopPropagation
                , preventDefault = options.preventDefault
                }
            )
        |> Events.custom "keyup"
