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

There are special considerations to take into account when using this or the laziness will
not actually work. It is very easy to get this wrong or to accidentally break it, so please
read the following carefully.

This function MUST be called with with a configuration and assigned to a TOP-LEVEL CONSTANT.

Specifically, this works:

    viewFocusLoop : List item -> List ( String, Html msg )
    viewFocusLoop =
        FocusLoop.lazy
            { toId = .id
            , focus = Focus
            , view = \arrowKeyHandlers item -> ...
            , leftRight = True
            , upDown = True
            }

This does NOT work:

    viewFocusLoop : List item -> List ( String, Html msg )
    viewFocusLoop items =
        FocusLoop.lazy
            { toId = .id
            , focus = Focus
            , view = \arrowKeyHandlers item -> ...
            , leftRight = True
            , upDown = True
            }
            items

Further, the same considerations you would take for `Html.Lazy.lazyN` apply
to the `item`s passed to this function. That is primitives are checked by value
and everything else is checked by reference, see <https://guide.elm-lang.org/optimization/lazy>

Crucially, this means that you cannot use records or instances of custom types that do not exist
on the model (such that the same reference will be passed on each render). Or put into inverse
terms, you may not pass records or custom types that are instantiated in the view.

Consider the following example using Tooltips. Knowing that we must assign the result of
`FocusLoop.lazy` to a top-level constant, we might try the following:

    type alias Model =
        { activeTooltip : Maybe Tooltip
        , items : List Item
        }

    type Tooltip
        = ItemTooltip ItemId

    type alias Item =
        { id : ItemId
        }

    view : Model -> Html msg
    view model =
        div []
            [ viewFocusLoop
                (List.map
                    (\item ->
                        { item = item
                        , tooltipOpen = model.activeTooltip == Just (ItemTooltip item.id)
                        }
                    )
                    model.items
                )
            ]

    viewFocusLoop : List { item, tooltipOpen } -> List ( String, Html msg )
    viewFocusLoop =
        FocusLoop.lazy
            { toId = .item.id
            , focus = Focus
            , view = \arrowKeyHandlers { item, tooltipOpen } -> ...
            , leftRight = True
            , upDown = True
            }

However this will not work, because in the List.map call we are instantiating
a new anonymous record, and thus a new reference, on each call to view.

In this case, you will need to use one of the other FocusLoop.lazy functions. To follow
this thread, see the example for FocusLoop.lazy2.

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
        { apply = apply
        , toId = config.toId
        , focus = config.focus
        , view = config.view
        , leftRight = config.leftRight
        , upDown = config.upDown
        }


{-| Like FocusLoop.lazy, but with 2 arguments to your view function.

Picking up from the usage example for FocusLoop.lazy, we can use this function to
use Html.Lazy.lazy2 (actually Html.Lazy.lazy4, but the first two arguments are used
by FocusLoop.lazy2 itself!) to check on individual arguments.

Because the restriction regarding assigning to a top-level constant still applies,
our mapping side is the same as before:

    view : Model -> Html msg
    view model =
        div []
            [ viewFocusLoop
                (List.map
                    (\item ->
                        { item = item
                        , tooltipOpen = model.activeTooltip == Just (ItemTooltip item.id)
                        }
                    )
                    model.items
                )
            ]

But for our `viewFocusLoop` function we will use `lazy2` and supply an additional `apply`
in the configuration to extract our arguments:

    FocusLoop.lazy2
        { apply = \f { item, tooltipOpen } -> f item tooltipOpen
        , ...
        }

You can think of `f` in this function as being the `view` function passed to `Html.Lazy.lazy2`.

i.e.

                        -- vvv This lambda is `f` in the above example. vvv --
    Html.Lazy.lazy2 <|              \item tooltipOpen -> ...

For a more in-depth example, see UsageExamples/FocusLoop in the component catalog.

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

See lazy and lazy2 usage example for more details.

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

See lazy and lazy2 usage example for more details.

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

See lazy and lazy2 usage example for more details.

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
    -- This has to be point-free, adding an `items` arg will break lazy.
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


type Args2 a1 a2
    = Args2 a1 a2


type Args3 a1 a2 a3
    = Args3 a1 a2 a3


type Args4 a1 a2 a3 a4
    = Args4 a1 a2 a3 a4


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
