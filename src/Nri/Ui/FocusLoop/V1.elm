module Nri.Ui.FocusLoop.V1 exposing (addEvents, lazy, lazy2, lazy3, lazy4, lazy5)

{-| Sometimes, there are sets of interactive elements that we want users to be able to navigate
through with arrow keys rather than with tabs, and we want the final focus change to wrap.
This module makes it easier to set up this focus and wrapping behavior.

@docs addEvents, lazy, lazy2, lazy3, lazy4, lazy5

-}

import Accessibility.Styled exposing (Html)
import Accessibility.Styled.Key as Key
import Html.Styled exposing (Attribute)
import Html.Styled.Lazy as Lazy


{-| Helper for creating a list of elements navigable via arrow keys, with wrapping.

Your `view` function will be called for each item with the corresponding keyboard
event handlers, as well as the item itself.

e.g.

    FocusLoop.lazy
        { id = .id
        , focus = Focus
        , leftRight = True
        , upDown = True
        , view =
            \handlers item ->
                div
                    [ handlers ]
                    [ text item.name ]
        }
        items

As the name suggests, this function uses Html.Lazy.lazy to render your `view` function.

Please ensure that the arguments you are passing to `view`, that is the contents
of your array, will behave as expected with Html.Lazy. Hint, you may need to
flatten records into tuples and use lazy2, lazy3, etc. if your arguments are being
checked by reference instead of by value. See <https://guide.elm-lang.org/optimization/lazy>
for more information.

-}
lazy :
    { id : a -> String
    , focus : String -> msg
    , view : Attribute msg -> a -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List a
    -> List (Html msg)
lazy config =
    siblings
        >> List.map
            (\( item, ( prev, next ) ) ->
                Lazy.lazy3 (view config) (config.id prev) (config.id next) item
            )


{-| Like FocusLoop.lazy, but with 2 arguments to your view function.
-}
lazy2 :
    { id : ( a1, a2 ) -> String
    , focus : String -> msg
    , view : Attribute msg -> a1 -> a2 -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List ( a1, a2 )
    -> List (Html msg)
lazy2 config =
    siblings
        >> List.map
            (\( ( a1, a2 ), ( prev, next ) ) ->
                Lazy.lazy4 (view config) (config.id prev) (config.id next) a1 a2
            )


{-| Like FocusLoop.lazy, but with 3 arguments to your view function.
-}
lazy3 :
    { id : ( a1, a2, a3 ) -> String
    , focus : String -> msg
    , view : Attribute msg -> a1 -> a2 -> a3 -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List ( a1, a2, a3 )
    -> List (Html msg)
lazy3 config =
    siblings
        >> List.map
            (\( ( a1, a2, a3 ), ( prev, next ) ) ->
                Lazy.lazy5 (view config) (config.id prev) (config.id next) a1 a2 a3
            )


{-| Like FocusLoop.lazy, but with 4 arguments to your view function.
-}
lazy4 :
    { id : { a1 : a1, a2 : a2, a3 : a3, a4 : a4 } -> String
    , focus : String -> msg
    , view : Attribute msg -> a1 -> a2 -> a3 -> a4 -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List { a1 : a1, a2 : a2, a3 : a3, a4 : a4 }
    -> List (Html msg)
lazy4 config =
    siblings
        >> List.map
            (\( { a1, a2, a3, a4 }, ( prev, next ) ) ->
                Lazy.lazy6 (view config) (config.id prev) (config.id next) a1 a2 a3 a4
            )


{-| Like FocusLoop.lazy, but with 5 arguments to your view function.
-}
lazy5 :
    { id : { a1 : a1, a2 : a2, a3 : a3, a4 : a4, a5 : a5 } -> String
    , focus : String -> msg
    , view : Attribute msg -> a1 -> a2 -> a3 -> a4 -> a5 -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List { a1 : a1, a2 : a2, a3 : a3, a4 : a4, a5 : a5 }
    -> List (Html msg)
lazy5 config =
    siblings
        >> List.map
            (\( { a1, a2, a3, a4, a5 }, ( prev, next ) ) ->
                Lazy.lazy7 (view config) (config.id prev) (config.id next) a1 a2 a3 a4 a5
            )


{-| Zip a list of items with its corresponding keyboard events.

Prefer `lazy`, `lazy2`, `lazy3`, `lazy4`, or `lazy5` to this function where possible.

If you must use this and intend to use Html.Lazy yourself, please keep in mind
that the List (Event msg) returned from this function will be checked by reference,
so you must attach it to your model in such a way that each vdom evaluation is
receiving the same reference. In other words, call this in init/update and keep a reference
to the event handlers in the model, don't call it directly in the view.

-}
addEvents :
    { focus : a -> msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List a
    -> List ( a, List (Key.Event msg) )
addEvents config =
    siblings >> List.map (Tuple.mapSecond (keyEvents config))


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
