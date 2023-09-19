module Nri.Ui.FocusLoop.Lazy.V1 exposing (lazy, lazy2, lazy3, lazy4, lazy5, Args2(..), Args3(..), Args4(..), Args5(..))

{-| Sometimes, there are sets of interactive elements that we want users to be able to navigate
through with arrow keys rather than with tabs, and we want the final focus change to wrap.
This module makes it easier to set up this focus and wrapping behavior.

@docs addEvents, lazy, lazy2, lazy3, lazy4, lazy5, Args2, Args3, Args4, Args5

-}

import Accessibility.Styled exposing (Html)
import Html.Styled.Lazy as Lazy
import Nri.Ui.FocusLoop.Internal exposing (siblings)
import Nri.Ui.FocusLoop.V1 exposing (Config, view)


type alias Lazy id msg args =
    Config id msg args -> List args -> List ( id, Html msg )


type Args2 a1 a2
    = Args2 a1 a2


type Args3 a1 a2 a3
    = Args3 a1 a2 a3


type Args4 a1 a2 a3 a4
    = Args4 a1 a2 a3 a4


type Args5 a1 a2 a3 a4 a5
    = Args5 a1 a2 a3 a4 a5


{-| Helper for creating a list of elements navigable via arrow keys, with wrapping.

Your `view` function will be called for each item with the corresponding keyboard
event handlers, as well as the item itself.

e.g.

    FocusLoop.lazy
        { id = .id
        , focus = Focus
        , leftRight = True
        , upDown = True
        , view = viewFocusableItem
        }
        items

    viewFocusableItem handlers item =
        div
            [ handlers ]
            [ text item.name ]

As the name suggests, this function uses Html.Lazy.lazy to render your `view` function.

Please ensure that the arguments you are passing to `view`, i.e. the contents
of your array, will behave as expected with Html.Lazy. tl;dr, only primitives are
checked by value; everything else is checked by reference. This means that if you are
constructing records in your view that are passed into `lazy`, they will be reconstructed
on every call to view, causing the lazy to evaluate as not equal and ultimately cause your
view to re-render on every call to `view`.

See <https://guide.elm-lang.org/optimization/lazy> for more details.

Hint, you may need to flatten records into individual arguments (tuples in the list
of items) and use lazy2, lazy3, etc. so that you can pass primitives and ensure they
are checked by value, or individual references (i.e. not a record container many, a la
`config`).

-}
lazy : Lazy id msg a
lazy config =
    let
        -- DO NOT INLINE THIS.
        view_ =
            view config
    in
    siblings
        >> List.map
            (\( item, ( prev, next ) ) ->
                ( config.id item
                , Lazy.lazy3 view_ item (config.id prev) (config.id next)
                )
            )


{-| Like FocusLoop.lazy, but with 2 arguments to your view function.

e.g.

    FocusLoop.lazy
        { id = \(FocusLoop.Args2 a1 a2) -> ...
        , focus = Focus
        , leftRight = True
        , upDown = True
        , view = viewFocusableItem
        }
        [ FocusLoop.Args2 a1 a2
        , FocusLoop.Args2 b1 a2
        ]

    viewFocusableItem handlers (FocusLoop.Args2 arg1 arg2) =
        ...

-}
lazy2 : Lazy id msg (Args2 a1 a2)
lazy2 config =
    let
        view_ a1 a2 =
            view config (Args2 a1 a2)
    in
    siblings
        >> List.map
            (\( (Args2 a1 a2) as args, ( prev, next ) ) ->
                ( config.id args
                , Lazy.lazy4 view_ a1 a2 (config.id prev) (config.id next)
                )
            )


{-| Like FocusLoop.lazy, but with 3 arguments to your view function.
-}
lazy3 : Lazy id msg (Args3 a1 a2 a3)
lazy3 config =
    let
        view_ a1 a2 a3 =
            view config (Args3 a1 a2 a3)
    in
    siblings
        >> List.map
            (\( (Args3 a1 a2 a3) as args, ( prev, next ) ) ->
                ( config.id args
                , Lazy.lazy5 view_ a1 a2 a3 (config.id prev) (config.id next)
                )
            )


{-| Like FocusLoop.lazy, but with 4 arguments to your view function.
-}
lazy4 : Lazy id msg (Args4 a1 a2 a3 a4)
lazy4 config =
    let
        view_ a1 a2 a3 a4 =
            view config (Args4 a1 a2 a3 a4)
    in
    siblings
        >> List.map
            (\( (Args4 a1 a2 a3 a4) as args, ( prev, next ) ) ->
                ( config.id args
                , Lazy.lazy6 view_ a1 a2 a3 a4 (config.id prev) (config.id next)
                )
            )


{-| Like FocusLoop.lazy, but with 5 arguments to your view function.
-}
lazy5 : Lazy id msg (Args5 a1 a2 a3 a4 a5)
lazy5 config =
    let
        view_ a1 a2 a3 a4 a5 =
            view config (Args5 a1 a2 a3 a4 a5)
    in
    siblings
        >> List.map
            (\( (Args5 a1 a2 a3 a4 a5) as args, ( prev, next ) ) ->
                ( config.id args
                , Lazy.lazy7 view_ a1 a2 a3 a4 a5 (config.id prev) (config.id next)
                )
            )
