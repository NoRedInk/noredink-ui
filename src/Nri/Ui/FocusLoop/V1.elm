module Nri.Ui.FocusLoop.V1 exposing (addEvents)

{-| Sometimes, there are sets of interactive elements that we want users to be able to navigate through with arrow keys rather than with tabs, and we want the final focus change to wrap. This module makes it easier to set up this focus and wrapping behavior.

@docs addEvents

-}

import Accessibility.Styled.Key as Key exposing (Event)


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


keyEvents :
    { focus : a -> msg, leftRight : Bool, upDown : Bool }
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
