module Nri.Ui.FocusLoop.Lazy.V1 exposing (lazy, lazy2, lazy3, lazy4, lazy5, Args2(..), Args3(..), Args4(..), Args5(..))

{-| Sometimes, there are sets of interactive elements that we want users to be able to navigate
through with arrow keys rather than with tabs, and we want the final focus change to wrap.
This module makes it easier to set up this focus and wrapping behavior.

@docs lazy, lazy2, lazy3, lazy4, lazy5, Args2, Args3, Args4, Args5

-}

import Accessibility.Styled exposing (Html)
import Html.Styled.Lazy as Lazy
import Nri.Ui.FocusLoop.Internal exposing (siblings)
import Nri.Ui.FocusLoop.V1 exposing (Config, view)


type alias Lazy msg args =
    Config String msg args -> List args -> List ( String, Html msg )


type Args2 a1 a2
    = Args2 a1 a2


type Args3 a1 a2 a3
    = Args3 a1 a2 a3


type Args4 a1 a2 a3 a4
    = Args4 a1 a2 a3 a4


type Args5 a1 a2 a3 a4 a5
    = Args5 a1 a2 a3 a4 a5


{-| Lazy version of FocusLoop.view

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
lazy : Lazy msg a
lazy config =
    let
        -- DO NOT INLINE THIS.
        --
        -- Lazy.lazyN will not work if this is an anonymous function.
        view_ =
            view config
    in
    siblings >> List.map (lazyHelp config (Lazy.lazy3 view_))


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
lazy2 : Lazy msg (Args2 a1 a2)
lazy2 config =
    let
        view_ a1 a2 =
            view config (Args2 a1 a2)
    in
    siblings >> List.map (lazyHelp config (\(Args2 a1 a2) -> Lazy.lazy4 view_ a1 a2))


{-| Like FocusLoop.lazy, but with 3 arguments to your view function.
-}
lazy3 : Lazy msg (Args3 a1 a2 a3)
lazy3 config =
    let
        view_ a1 a2 a3 =
            view config (Args3 a1 a2 a3)
    in
    siblings
        >> List.map (lazyHelp config (\(Args3 a1 a2 a3) -> Lazy.lazy5 view_ a1 a2 a3))


{-| Like FocusLoop.lazy, but with 4 arguments to your view function.
-}
lazy4 : Lazy msg (Args4 a1 a2 a3 a4)
lazy4 config =
    let
        view_ a1 a2 a3 a4 =
            view config (Args4 a1 a2 a3 a4)
    in
    siblings
        >> List.map (lazyHelp config (\(Args4 a1 a2 a3 a4) -> Lazy.lazy6 view_ a1 a2 a3 a4))


{-| Like FocusLoop.lazy, but with 5 arguments to your view function.
-}
lazy5 : Lazy msg (Args5 a1 a2 a3 a4 a5)
lazy5 config =
    let
        view_ a1 a2 a3 a4 a5 =
            view config (Args5 a1 a2 a3 a4 a5)
    in
    siblings
        >> List.map (lazyHelp config (\(Args5 a1 a2 a3 a4 a5) -> Lazy.lazy7 view_ a1 a2 a3 a4 a5))


lazyHelp :
    { config | id : args -> String }
    -> (args -> String -> String -> html)
    -> ( args, Maybe ( args, args ) )
    -> ( String, html )
lazyHelp { id } view_ =
    \( args, maybeSiblings ) ->
        let
            ( prevId, nextId ) =
                maybeSiblings
                    |> Maybe.map (Tuple.mapBoth id id)
                    |> Maybe.withDefault ( "", "" )
        in
        ( id args
        , view_ args prevId nextId
        )
