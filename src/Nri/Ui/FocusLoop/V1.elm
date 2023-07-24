module Nri.Ui.FocusLoop.V1 exposing (addEvents)

{-| Sometimes, there are sets of interactive elements that we want users to be able to navigate through with arrow keys rather than with tabs, and we want the final focus change to wrap. This module makes it easier to set up this focus and wrapping behavior.

@docs addEvents

-}

import Accessibility.Styled.Key exposing (Event)


{-| -}
addEvents :
    { toId : a -> String
    , leftRight : Bool
    , upDown : Bool
    }
    -> List a
    -> List ( a, List (Event msg) )
addEvents config items =
    []
