module Nri.Ui.FocusLoop.Internal exposing (..)

import Accessibility.Styled exposing (Attribute, Html)
import Accessibility.Styled.Key as Key


type alias Config id msg args =
    { id : args -> id
    , focus : id -> msg
    , view : Attribute msg -> args -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }


view : Config id msg args -> args -> id -> id -> Html msg
view config current prevId nextId =
    config.view (Key.onKeyDownPreventDefault (keyEvents config ( prevId, nextId ))) current


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
