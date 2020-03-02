module EventExtras exposing (onClickPreventDefaultForLinkWithHref)

import Html
import Html.Events as Events
import Json.Decode


{-| This is necessary to use in single-page apps (SPA) when intercepting the
`onClick` of `<a>` tags that trigger navigation within the app,
as a normal `onClickPreventDefault` will prevent the user from opening the link
in a new tab/window.

(From <https://github.com/elm-lang/html/issues/110>)

-}
onClickPreventDefaultForLinkWithHref : msg -> Html.Attribute msg
onClickPreventDefaultForLinkWithHref msg =
    let
        isSpecialClick : Json.Decode.Decoder Bool
        isSpecialClick =
            Json.Decode.map2
                (\isCtrl isMeta -> isCtrl || isMeta)
                (Json.Decode.field "ctrlKey" Json.Decode.bool)
                (Json.Decode.field "metaKey" Json.Decode.bool)

        succeedIfFalse : a -> Bool -> Json.Decode.Decoder a
        succeedIfFalse msg_ preventDefault =
            case preventDefault of
                False ->
                    Json.Decode.succeed msg_

                True ->
                    Json.Decode.fail "succeedIfFalse: condition was True"
    in
    Events.preventDefaultOn "click"
        (isSpecialClick
            |> Json.Decode.andThen (succeedIfFalse msg)
            |> Json.Decode.map (\a -> ( a, True ))
        )
