module Nri.Ui.FocusLoop.V1 exposing (addEvents, lazy, lazy2, lazy3, lazy4, lazy5)

{-| Sometimes, there are sets of interactive elements that we want users to be able to navigate
through with arrow keys rather than with tabs, and we want the final focus change to wrap.
This module makes it easier to set up this focus and wrapping behavior.

@docs addEvents, lazy, lazy2, lazy3, lazy4, lazy5

-}

import Accessibility.Styled exposing (Html)
import Accessibility.Styled.Key as Key exposing (Event)
import Html.Styled.Lazy as Lazy


{-| -}
lazy :
    { id : a -> String
    , focus : String -> msg
    , view : List (Event msg) -> a -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List a
    -> List (Html msg)
lazy config items =
    case items of
        [] ->
            []

        item :: [] ->
            [ Lazy.lazy2 config.view [] item ]

        _ ->
            List.map
                (\( item_, ( prev, next ) ) ->
                    Lazy.lazy3 (view config) (config.id prev) (config.id next) item_
                )
                (siblings items)


{-| -}
lazy2 :
    { id : ( a1, a2 ) -> String
    , focus : String -> msg
    , view : List (Event msg) -> a1 -> a2 -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List ( a1, a2 )
    -> List (Html msg)
lazy2 config items =
    case items of
        [] ->
            []

        ( a1, a2 ) :: [] ->
            [ Lazy.lazy3 config.view [] a1 a2 ]

        _ ->
            List.map
                (\( ( a1, a2 ), ( prev, next ) ) ->
                    Lazy.lazy4 (view config) (config.id prev) (config.id next) a1 a2
                )
                (siblings items)


{-| -}
lazy3 :
    { id : ( a1, a2, a3 ) -> String
    , focus : String -> msg
    , view : List (Event msg) -> a1 -> a2 -> a3 -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List ( a1, a2, a3 )
    -> List (Html msg)
lazy3 config items =
    case items of
        [] ->
            []

        ( a1, a2, a3 ) :: [] ->
            [ Lazy.lazy4 config.view [] a1 a2 a3 ]

        _ ->
            List.map
                (\( ( a1, a2, a3 ), ( prev, next ) ) ->
                    Lazy.lazy5 (view config) (config.id prev) (config.id next) a1 a2 a3
                )
                (siblings items)


{-| -}
lazy4 :
    { id : { a1 : a1, a2 : a2, a3 : a3, a4 : a4 } -> String
    , focus : String -> msg
    , view : List (Event msg) -> a1 -> a2 -> a3 -> a4 -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List { a1 : a1, a2 : a2, a3 : a3, a4 : a4 }
    -> List (Html msg)
lazy4 config items =
    case items of
        [] ->
            []

        { a1, a2, a3, a4 } :: [] ->
            [ Lazy.lazy5 config.view [] a1 a2 a3 a4 ]

        _ ->
            List.map
                (\( { a1, a2, a3, a4 }, ( prev, next ) ) ->
                    Lazy.lazy6 (view config) (config.id prev) (config.id next) a1 a2 a3 a4
                )
                (siblings items)


{-| -}
lazy5 :
    { id : { a1 : a1, a2 : a2, a3 : a3, a4 : a4, a5 : a5 } -> String
    , focus : String -> msg
    , view : List (Event msg) -> a1 -> a2 -> a3 -> a4 -> a5 -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List { a1 : a1, a2 : a2, a3 : a3, a4 : a4, a5 : a5 }
    -> List (Html msg)
lazy5 config items =
    case items of
        [] ->
            []

        { a1, a2, a3, a4, a5 } :: [] ->
            [ Lazy.lazy6 config.view [] a1 a2 a3 a4 a5 ]

        _ ->
            List.map
                (\( { a1, a2, a3, a4, a5 }, ( prev, next ) ) ->
                    Lazy.lazy7 (view config) (config.id prev) (config.id next) a1 a2 a3 a4 a5
                )
                (siblings items)


{-| -}
addEvents :
    { focus : a -> msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List a
    -> List ( a, List (Event msg) )
addEvents config items =
    case items of
        [] ->
            []

        item :: [] ->
            [ ( item, [] ) ]

        _ ->
            siblings items
                |> List.map (Tuple.mapSecond (keyEvents config))


view :
    { config
        | view : List (Event msg) -> html
        , focus : a -> msg
        , leftRight : Bool
        , upDown : Bool
    }
    -> a
    -> a
    -> html
view config prevId nextId =
    config.view (keyEvents config ( prevId, nextId ))


keyEvents :
    { config | focus : a -> msg, leftRight : Bool, upDown : Bool }
    -> ( a, a )
    -> List (Event msg)
keyEvents config ( prev, next ) =
    let
        leftRightEvents =
            if config.leftRight then
                [ Key.right (config.focus next)
                , Key.left (config.focus prev)
                ]

            else
                []

        upDownEvents =
            if config.upDown then
                [ Key.down (config.focus next)
                , Key.up (config.focus prev)
                ]

            else
                []
    in
    leftRightEvents ++ upDownEvents


siblings : List a -> List ( a, ( a, a ) )
siblings items =
    let
        previousIds : List (Maybe a)
        previousIds =
            finalId :: List.map Just items

        firstId : Maybe a
        firstId =
            List.head items

        finalId : Maybe a
        finalId =
            List.head (List.reverse items)
    in
    List.map2 (\id nextItem -> ( id, nextItem )) previousIds items
        |> List.foldr
            (\( previousId, item ) ( nextId, acc ) ->
                ( Just item
                , ( item, Maybe.map2 Tuple.pair previousId nextId ) :: acc
                )
            )
            ( firstId, [] )
        |> Tuple.second
        |> List.filterMap
            (\( item, maybeSiblings ) ->
                Maybe.map (Tuple.pair item) maybeSiblings
            )
