module Nri.Ui.FocusLoop.V1 exposing (Config, view, addEvents)

{-| Sometimes, there are sets of interactive elements that we want users to be able to navigate
through with arrow keys rather than with tabs, and we want the final focus change to wrap.
This module makes it easier to set up this focus and wrapping behavior.

@docs Config, view, addEvents

-}

import Accessibility.Styled exposing (Attribute, Html)
import Accessibility.Styled.Key as Key
import Nri.Ui.FocusLoop.Internal exposing (keyEvents, siblings)


{-| FocusLoop.view Configuration
-}
type alias Config id msg args =
    { id : args -> id
    , focus : id -> msg
    , view : Attribute msg -> args -> Html msg
    , leftRight : Bool
    , upDown : Bool
    }


{-| View a group of elements that should be keyboard navigable in a loop.
-}
view : Config id msg args -> args -> id -> id -> Html msg
view config current prevId nextId =
    config.view (Key.onKeyDownPreventDefault (keyEvents config ( prevId, nextId ))) current


{-| Zip a list of items with its corresponding keyboard events.
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
