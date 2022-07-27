module Nri.Ui.FocusTrap.V1 exposing (FocusTrap, toAttribute)

{-| Create a focus trap.

@docs FocusTrap, toAttribute

-}

import Accessibility.Styled as Html
import Nri.Ui.WhenFocusLeaves.V1 as WhenFocusLeaves


{-| Defines how focus will wrap in reponse to tab keypresses in a part of the UI.

The ids referenced here are expected to correspond to elements in the container
we are applying the focus trap on.

-}
type alias FocusTrap msg =
    { firstId : String
    , lastId : String
    , focus : String -> msg
    }


{-| Attach this attribute to add a focus trap to an HTML element.
-}
toAttribute : FocusTrap msg -> Html.Attribute msg
toAttribute { firstId, lastId, focus } =
    WhenFocusLeaves.toAttribute
        { firstId = Debug.log "firstId" firstId
        , lastId = Debug.log "lastId " lastId
        , -- if the user tabs back while on the first id,
          -- we want to wrap around to the last id.
          tabBackAction = focus lastId
        , -- if the user tabs forward while on the last id,
          -- we want to wrap around to the first id.
          tabForwardAction = focus firstId
        }
