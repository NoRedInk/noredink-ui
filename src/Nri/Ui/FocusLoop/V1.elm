module Nri.Ui.FocusLoop.V1 exposing (addEvents)

{-| Sometimes, there are sets of interactive elements that we want users to be able to navigate through with arrow keys rather than with tabs, and we want the final focus change to wrap. This module makes it easier to set up this focus and wrapping behavior.

@docs addEvents

-}

import Accessibility.Styled.Key as Key exposing (Event)


{-| -}
addEvents :
    { toId : a -> String
    , focus : String -> msg
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
            addEvents_ config items


addEvents_ :
    { toId : a -> String
    , focus : String -> msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List a
    -> List ( a, List (Event msg) )
addEvents_ config items =
    let
        ids : List String
        ids =
            List.map config.toId items

        previousIds : List (Maybe String)
        previousIds =
            finalId :: List.map Just ids

        firstId : Maybe String
        firstId =
            List.head ids

        finalId : Maybe String
        finalId =
            List.head (List.reverse ids)
    in
    List.map2 (\id nextItem -> ( id, nextItem )) previousIds items
        |> List.foldr
            (\( previousId, item ) ( nextId, acc ) ->
                let
                    leftRightEvents =
                        if config.leftRight then
                            [ Maybe.map (config.focus >> Key.right) nextId
                            , Maybe.map (config.focus >> Key.left) previousId
                            ]

                        else
                            []

                    upDownEvents =
                        if config.upDown then
                            [ Maybe.map (config.focus >> Key.down) nextId
                            , Maybe.map (config.focus >> Key.up) previousId
                            ]

                        else
                            []
                in
                ( Just (config.toId item)
                , ( item, List.filterMap identity (leftRightEvents ++ upDownEvents) ) :: acc
                )
            )
            ( firstId, [] )
        |> Tuple.second
