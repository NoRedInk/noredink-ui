module Nri.Ui.FocusLoop.Lazy.V1 exposing (lazy, lazy2, lazy3, lazy4, lazy5)

{-| Like FocusLoop, but with lazy rendering. Prefer this when the set of items can change.

@docs lazy, lazy2, lazy3, lazy4, lazy5

-}

import Accessibility.Styled exposing (Html)
import Accessibility.Styled.Key as Key
import Html.Styled.Lazy as Lazy
import Nri.Ui.FocusLoop.Internal exposing (keyEvents, siblings)


{-| Lazy version of FocusLoop.view

As the name suggests, this function uses Html.Lazy.lazy to render your `view` function.

Please ensure that the arguments you are passing to `view`, i.e. the contents
of your array, will behave as expected with Html.Lazy. tl;dr, only primitives are
checked by value; everything else is checked by reference. This means that if you are
constructing records in your view that are passed into `lazy`, they will be reconstructed
on every call to view, causing the lazy to evaluate as not equal and ultimately cause your
view to re-render on every call to `view`.

See <https://guide.elm-lang.org/optimization/lazy> for more details.

Hint, you may need to flatten records into individual arguments and use lazy2, lazy3, etc.
so that you can pass primitives and ensure they are checked by value, or individual references
(i.e. not a record container many, a la `config`).

-}
lazy :
    { toId : item -> String
    , focus : String -> msg
    , view : List (Key.Event msg) -> item -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List item
    -> List ( String, Html msg )
lazy config =
    lazyHelp Lazy.lazy3
        apply
        identity
        { apply = \f item -> f item
        , toId = config.toId
        , focus = config.focus
        , view = config.view
        , leftRight = config.leftRight
        , upDown = config.upDown
        }


{-| Like FocusLoop.lazy, but with 2 arguments to your view function.

Use FocusLoop.lazy2Args to construct your arguments, e.g.

    FocusLoop.lazy2
        { id = \arg1 -> arg2 -> ...
        , applyLazy = \lazy item -> lazy item.arg1 item.arg2
        , focus = Focus
        , leftRight = True
        , upDown = True
        , view = \arrowKeyHandlers arg1 arg2 -> ...
        }
        [ FocusLoop.lazy2Args a1 a2
        , FocusLoop.lazy2Args b1 b2
        ]

-}
lazy2 :
    { apply : (a -> b -> Args2 a b) -> item -> Args2 a b
    , toId : item -> String
    , focus : String -> msg
    , view : List (Key.Event msg) -> a -> b -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List item
    -> List ( String, Html msg )
lazy2 =
    lazyHelp Lazy.lazy4 apply2 Args2


{-| Like FocusLoop.lazy, but with 3 arguments to your view function.

See lazy2 usage example for more details.

-}
lazy3 :
    { apply : (a -> b -> c -> Args3 a b c) -> item -> Args3 a b c
    , toId : item -> String
    , focus : String -> msg
    , view : List (Key.Event msg) -> a -> b -> c -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List item
    -> List ( String, Html msg )
lazy3 =
    lazyHelp Lazy.lazy5 apply3 Args3


{-| Like FocusLoop.lazy, but with 4 arguments to your view function.

See lazy2 usage example for more details.

-}
lazy4 :
    { apply : (a -> b -> c -> d -> Args4 a b c d) -> item -> Args4 a b c d
    , toId : item -> String
    , focus : String -> msg
    , view : List (Key.Event msg) -> a -> b -> c -> d -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List item
    -> List ( String, Html msg )
lazy4 =
    lazyHelp Lazy.lazy6 apply4 Args4


{-| Like FocusLoop.lazy, but with 5 arguments to your view function.

See lazy2 usage example for more details.

-}
lazy5 :
    { apply : (a -> b -> c -> d -> e -> Args5 a b c d e) -> item -> Args5 a b c d e
    , toId : item -> String
    , focus : String -> msg
    , view : List (Key.Event msg) -> a -> b -> c -> d -> e -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List item
    -> List ( String, Html msg )
lazy5 =
    lazyHelp Lazy.lazy7 apply5 Args5


lazyHelp :
    ((String -> String -> toView) -> String -> String -> toView)
    -> (toView -> args -> Html msg)
    -> toArgs
    ->
        { config
            | view : List (Key.Event msg) -> toView
            , toId : item -> String
            , focus : String -> msg
            , leftRight : Bool
            , upDown : Bool
            , apply : toArgs -> item -> args
        }
    -> List item
    -> List ( String, Html msg )
lazyHelp lazyN applyView toArgs config =
    let
        -- Don't inline this, lazy will not work if passed an anonymous function.
        view prevId nextId =
            config.view
                (case ( prevId, nextId ) of
                    ( "", "" ) ->
                        []

                    _ ->
                        keyEvents config ( prevId, nextId )
                )
    in
    siblings
        >> List.map
            (\( item, maybeSiblings ) ->
                let
                    ( prevId, nextId ) =
                        Maybe.map (Tuple.mapBoth config.toId config.toId) maybeSiblings
                            -- We have to use empty strings here instead of maybes
                            -- so that lazy will check by value instead of by reference.
                            |> Maybe.withDefault ( "", "" )
                in
                ( config.toId item
                , applyView (lazyN view prevId nextId) (config.apply toArgs item)
                )
            )


{-| Helper type for lazy2
-}
type Args2 a1 a2
    = Args2 a1 a2


{-| Helper type for lazy3
-}
type Args3 a1 a2 a3
    = Args3 a1 a2 a3


{-| Helper type for lazy4
-}
type Args4 a1 a2 a3 a4
    = Args4 a1 a2 a3 a4


{-| Helper type for lazy5
-}
type Args5 a1 a2 a3 a4 a5
    = Args5 a1 a2 a3 a4 a5


apply : (a -> b) -> a -> b
apply f a =
    f a


apply2 : (a -> b -> c) -> Args2 a b -> c
apply2 f (Args2 a b) =
    f a b


apply3 : (a -> b -> c -> d) -> Args3 a b c -> d
apply3 f (Args3 a b c) =
    f a b c


apply4 : (a -> b -> c -> d -> e) -> Args4 a b c d -> e
apply4 f (Args4 a b c d) =
    f a b c d


apply5 : (a -> b -> c -> d -> e -> f) -> Args5 a b c d e -> f
apply5 f (Args5 a b c d e) =
    f a b c d e
