module Nri.Ui.FocusLoop.Lazy.V1 exposing (lazy, lazy2, lazy3, lazy4, lazy5)

{-| Sometimes, there are sets of interactive elements that we want users to be able to navigate
through with arrow keys rather than with tabs, and we want the final focus change to wrap.
This module makes it easier to set up this focus and wrapping behavior.

@docs addEvents, lazy, lazy2, lazy3, lazy4, lazy5

-}

import Accessibility.Styled exposing (Html)
import Html.Styled exposing (Attribute)
import Html.Styled.Lazy as Lazy
import Nri.Ui.FocusLoop.Internal exposing (siblings, view)


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
lazy :
    { id : a -> String
    , focus : String -> msg
    , view : Attribute msg -> a -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List a
    -> List ( String, Html msg )
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
                , Lazy.lazy3 view_ (config.id prev) (config.id next) item
                )
            )


{-| Like FocusLoop.lazy, but with 2 arguments to your view function.

For safest usage, place `FocusLoop.lazy2` at the top level of its own function.

e.g.

    viewSelectedTermsKeyed :
        SelectedTermToggleSource
        -> Maybe Tooltip
        -> List SerializableSelectableTerm
        -> List ( String, Html Msg )
    viewSelectedTermsKeyed source selectedTerms =
        List.map (\\term -> ( term, source )) >> viewSelectedTermsKeyed\_

    viewSelectedTermsKeyed\_ :
        List
        ( SerializableSelectableTerm
        , SelectedTermToggleSource
        , Bool
        )
        -> List ( String, Html Msg )
    viewSelectedTermsKeyed\_ =
        FocusLoop.lazy3
        { id = ( term, source, \_ ) -> selectedTermToggleId source term.id
        , focus = Focus
        , leftRight = True
        , upDown = False
        , view = viewSelectedTerm
        }

This ensures anything used in the view function is being accounted for by the lazy
equality check and you don't end up with broken interactions that don't cause
re-renders when they should.

-}
lazy2 :
    { id : ( a1, a2 ) -> String
    , focus : String -> msg
    , view : Attribute msg -> a1 -> a2 -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }
    -> List ( a1, a2 )
    -> List ( String, Html msg )
lazy2 config =
    let
        view_ =
            view config
    in
    siblings
        >> List.map
            (\( ( a1, a2 ) as args, ( prev, next ) ) ->
                ( config.id args
                , Lazy.lazy4 view_ (config.id prev) (config.id next) a1 a2
                )
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
    -> List ( String, Html msg )
lazy3 config =
    let
        view_ =
            view config
    in
    siblings
        >> List.map
            (\( ( a1, a2, a3 ) as args, ( prev, next ) ) ->
                ( config.id args
                , Lazy.lazy5 view_ (config.id prev) (config.id next) a1 a2 a3
                )
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
    -> List ( String, Html msg )
lazy4 config =
    let
        view_ =
            view config
    in
    siblings
        >> List.map
            (\( { a1, a2, a3, a4 } as args, ( prev, next ) ) ->
                ( config.id args
                , Lazy.lazy6 view_ (config.id prev) (config.id next) a1 a2 a3 a4
                )
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
    -> List ( String, Html msg )
lazy5 config =
    let
        view_ =
            view config
    in
    siblings
        >> List.map
            (\( { a1, a2, a3, a4, a5 } as args, ( prev, next ) ) ->
                ( config.id args
                , Lazy.lazy7 view_ (config.id prev) (config.id next) a1 a2 a3 a4 a5
                )
            )
