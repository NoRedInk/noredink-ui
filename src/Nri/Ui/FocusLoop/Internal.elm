module Nri.Ui.FocusLoop.Internal exposing (..)

import Accessibility.Styled.Key as Key
import Html.Styled exposing (Attribute)


view :
    { config
        | view : Attribute msg -> html
        , focus : a -> msg
        , leftRight : Bool
        , upDown : Bool
    }
    -> a
    -> a
    -> html
view config prevId nextId =
    config.view (Key.onKeyDownPreventDefault (keyEvents config ( prevId, nextId )))


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


siblings : List a -> List ( a, ( a, a ) )
siblings items =
    let
        previousItems : List (Maybe a)
        previousItems =
            finalItem :: List.map Just items

        finalItem : Maybe a
        finalItem =
            List.head (List.reverse items)

        init =
            ( List.head items, [] )
    in
    List.map2 Tuple.pair previousItems items
        |> List.foldr
            (\( previousId, item ) ( nextId, acc ) ->
                ( Just item
                , ( item, Maybe.map2 Tuple.pair previousId nextId ) :: acc
                )
            )
            init
        |> Tuple.second
        |> List.filterMap
            (\( item, maybeSiblings ) ->
                Maybe.map (Tuple.pair item) maybeSiblings
            )
