module Nri.Ui.FocusLoop.Internal exposing (..)

import Accessibility.Styled.Key as Key


keyEvents :
    { config
        | focus : a -> msg
        , leftRight : Bool
        , upDown : Bool
    }
    -> ( a, a )
    -> List (Key.Event msg)
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


siblings : List a -> List ( a, Maybe ( a, a ) )
siblings items =
    let
        previousItems =
            finalItem :: List.map Just items

        finalItem =
            List.head (List.reverse items)

        init =
            ( List.head items, [] )
    in
    case items of
        [] ->
            []

        singleton :: [] ->
            [ ( singleton, Nothing ) ]

        _ ->
            List.map2 Tuple.pair previousItems items
                |> List.foldr
                    (\( previousId, item ) ( nextId, acc ) ->
                        ( Just item
                        , ( item, Maybe.map2 Tuple.pair previousId nextId ) :: acc
                        )
                    )
                    init
                |> Tuple.second
