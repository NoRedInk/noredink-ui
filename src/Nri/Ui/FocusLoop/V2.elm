module Nri.Ui.FocusLoop.V2 exposing (..)

{-| Sometimes, there are sets of interactive elements that we want users to be able to navigate
through with arrow keys rather than with tabs, and we want the final focus change to wrap.
This module makes it easier to set up this focus and wrapping behavior.

@docs view, addEvents

-}

import Accessibility.Styled exposing (Html)
import Accessibility.Styled.Key as Key
import Nri.Ui.FocusLoop.Internal exposing (keyEvents, siblings)


{-| Helper for creating a list of elements with looping arrow-key navigation.

Your `view` function will be called for each item with the corresponding keyboard
event handlers, as well as the item itself.

e.g.

    FocusLoop.view
        { id = .id
        , focus = Focus
        , leftRight = True
        , upDown = True
        , view = viewFocusableItem
        }
        [ { id = 1, name = "Foo" } ]

    viewFocusableItem item arrowKeyHandlers =
        div
            [ Key.onKeyDownPreventDefault arrowKeyHandlers ]
            [ text item.name ]

Does your list support adding and removing items? If so, check out `FocusLoop.Lazy` which
will prevent recalculation of event handlers for every item when the list changes.

-}
view :
    { toId : item -> id
    , focus : id -> msg
    , view : List (Key.Event msg) -> item -> Html msg
    , leftRight : Bool
    , upDown : Bool
    , wrappable : Bool
    }
    -> List item
    -> List (Html msg)
view config items =
    siblings config.wrappable items
        |> List.map
            (\( item, ( maybePrevious, maybeNext ) ) ->
                config.view
                    (keyEvents config ( Maybe.map config.toId maybePrevious, Maybe.map config.toId maybeNext ))
                    item
            )


{-| Zip a list of items with its corresponding keyboard events.
-}
addEvents :
    { focus : a -> msg
    , leftRight : Bool
    , upDown : Bool
    , wrappable : Bool
    }
    -> List a
    -> List ( a, List (Key.Event msg) )
addEvents config items =
    siblings config.wrappable items |> List.map (Tuple.mapSecond (keyEvents config))
