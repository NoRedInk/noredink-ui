module Nri.Ui.Html.Extra exposing (..)

{-| For all utils involving HTML.

@docs role, noOpHref, noOpHrefUrl

@docs onEsc, onEnter, onKeyUp, onEnterAndSpace

@docs textFromList, oxfordifyWithHtml, nbsp

-}

import Char
import Html exposing (Attribute, Html, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode


{-| Convenience for defining role attributes, e.g. <div role="tabpanel">
-}
role : String -> Attribute msg
role =
    attribute "role"


{-| -}
noOpHref : Attribute a
noOpHref =
    href noOpHrefUrl


{-| This is a better choice for a no-op than "#" because "#" changes your
location bar. See <http://stackoverflow.com/a/20676911> for more details.
-}
noOpHrefUrl : String
noOpHrefUrl =
    "javascript:void(0)"


{-| -}
onEsc : a -> a -> Attribute a
onEsc onEscAction onOtherKey =
    on "keyup"
        (Json.Decode.map
            (\keyCode ->
                if keyCode == 27 then
                    onEscAction
                else
                    onOtherKey
            )
            keyCode
        )


{-| -}
onEnter : a -> Attribute a
onEnter onEnterAction =
    onKeyUp defaultOptions
        (\keyCode ->
            if keyCode == 13 then
                Just onEnterAction
            else
                Nothing
        )


{-| "Buttons" should trigger on Enter and on Space.
-}
onEnterAndSpace : msg -> Attribute msg
onEnterAndSpace msg =
    onKeyUp defaultOptions
        (\keyCode ->
            if keyCode == 13 || keyCode == 32 then
                Just msg
            else
                Nothing
        )


{-| Convert a keycode into a message on keyup
-}
onKeyUp : Options -> (Int -> Maybe a) -> Attribute a
onKeyUp options toMaybeMsg =
    onWithOptions "keyup" options <|
        Json.Decode.andThen
            (\keyCode ->
                keyCode
                    |> toMaybeMsg
                    |> Maybe.map Json.Decode.succeed
                    |> Maybe.withDefault (Json.Decode.fail (toString keyCode))
            )
            keyCode


{-| Takes a list of strings, joins them with a space and returns it as a Html.text.
textFromList ["Hello", "World"] == text [ String.join " " ["Hello", "World" ] ]
-}
textFromList : List String -> Html msg
textFromList =
    String.join " " >> text


{-| -}
oxfordifyWithHtml : String -> String -> List (Html msg) -> List (Html msg)
oxfordifyWithHtml pre post items =
    let
        textSpan string =
            span [] [ text string ]

        final centrals =
            [ textSpan pre ] ++ centrals ++ [ textSpan post ]
    in
    case items of
        [] ->
            []

        [ single ] ->
            final [ single ]

        [ first, second ] ->
            final [ first, textSpan " and ", second ]

        many ->
            let
                beforeAnd =
                    List.take (List.length many - 1) many

                afterAnd =
                    List.drop (List.length many - 1) many
                        |> List.head
                        |> Maybe.withDefault (textSpan "")
            in
            final (List.intersperse (textSpan ", ") beforeAnd ++ [ textSpan ", and ", afterAnd ])


{-| Workaround for `Html.text "&nbsp;"` not working in elm.
-}
nbsp : Html msg
nbsp =
    Char.fromCode 160
        |> String.fromChar
        |> Html.text
