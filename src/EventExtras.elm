module EventExtras exposing
    ( onClickForLinkWithHref
    , onClickPreventDefaultAndStopPropagation
    , onClickPreventDefaultForLinkWithHref
    , onClickStopPropagation
    , onKeyDownPreventDefault
    )

import Html.Styled as Html
import Html.Styled.Events as Events
import Json.Decode exposing (Decoder)


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


{-| This is necessary to use when intercepting the
`onClick` of `<a>` tags that trigger navigation within the app,
as a normal `onClick` will prevent the user from opening the link
in a new tab/window.

(From <https://github.com/elm-lang/html/issues/110>)

-}
onClickForLinkWithHref : msg -> Html.Attribute msg
onClickForLinkWithHref msg =
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
    Events.on "click"
        (isSpecialClick
            |> Json.Decode.andThen (succeedIfFalse msg)
        )


{-| -}
onClickStopPropagation : msg -> Html.Attribute msg
onClickStopPropagation msg =
    onWithStopPropagation "click" msg


{-| -}
onWithStopPropagation : String -> msg -> Html.Attribute msg
onWithStopPropagation name msg =
    Events.stopPropagationOn name
        (Json.Decode.succeed ( msg, True ))


{-| Use with a list of key decoders.

    import EventExtras exposing (onKeyDownPreventDefault)
    import Accessibility.Styled.Key as Key

    button
        [ onKeyDownPreventDefault [ Key.down DoSomething ] ]
        [ text "Try arrow down" ]

-}
onKeyDownPreventDefault : List (Decoder msg) -> Html.Attribute msg
onKeyDownPreventDefault decoders =
    Events.preventDefaultOn "keydown"
        (Json.Decode.map (\d -> ( d, True )) (Json.Decode.oneOf decoders))


{-| -}
onClickPreventDefaultAndStopPropagation : msg -> Html.Attribute msg
onClickPreventDefaultAndStopPropagation msg =
    onWithPreventDefaultAndStopPropagation "click" msg


{-| -}
onWithPreventDefaultAndStopPropagation : String -> msg -> Html.Attribute msg
onWithPreventDefaultAndStopPropagation name msg =
    Events.custom name <|
        Json.Decode.succeed
            { preventDefault = True
            , stopPropagation = True
            , message = msg
            }
