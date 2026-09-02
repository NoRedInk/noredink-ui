module Nri.Ui.FocusLoop.InternalV2 exposing (..)

import Accessibility.Styled.Key as Key


keyEvents :
    { config
        | focus : a -> msg
        , leftRight : Bool
        , upDown : Bool
    }
    -> ( Maybe a, Maybe a )
    -> List (Key.Event msg)
keyEvents config ( maybePrevious, maybeNext ) =
    let
        leftRightEvents =
            if config.leftRight then
                List.filterMap identity
                    [ Maybe.map (\next -> Key.right (config.focus next)) maybeNext
                    , Maybe.map (\prev -> Key.left (config.focus prev)) maybePrevious
                    ]

            else
                []

        upDownEvents =
            if config.upDown then
                List.filterMap identity
                    [ Maybe.map (\next -> Key.down (config.focus next)) maybeNext
                    , Maybe.map (\prev -> Key.up (config.focus prev)) maybePrevious
                    ]

            else
                []
    in
    leftRightEvents ++ upDownEvents


siblings : Bool -> List a -> List ( a, ( Maybe a, Maybe a ) )
siblings wrappable items =
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
            [ ( singleton, ( Nothing, Nothing ) ) ]

        _ ->
            if wrappable then
                List.map2 Tuple.pair previousItems items
                    |> List.foldr
                        (\( previousId, item ) ( nextId, acc ) ->
                            ( Just item
                            , ( item, Tuple.pair previousId nextId ) :: acc
                            )
                        )
                        init
                    |> Tuple.second

            else
                List.map2 Tuple.pair previousItems items
                    |> List.foldr
                        (\( previousId, item ) ( nextId, acc ) ->
                            let
                                nextIsFirst =
                                    Tuple.first init == nextId

                                previousIsFinal =
                                    previousId == finalItem

                                previous =
                                    if previousIsFinal then
                                        Nothing

                                    else
                                        previousId

                                next =
                                    if nextIsFirst then
                                        Nothing

                                    else
                                        nextId
                            in
                            ( Just item
                            , ( item, Tuple.pair previous next ) :: acc
                            )
                        )
                        init
                    |> Tuple.second
