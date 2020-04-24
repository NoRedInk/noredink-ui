module Nri.Ui.Html.V3 exposing (onKeyUp, defaultOptions)

{-|

@docs onKeyUp, defaultOptions

-}

import Char
import Html.Styled as Html exposing (Attribute, Html, span, text)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events as Events exposing (..)
import Json.Decode


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
