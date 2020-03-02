module AttributeExtras exposing (targetBlank)

import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attributes


{-| Use this list of attributes instead of applying `Attributes.target "_blank"`
directly. This prevents an exploits like "tabnabbing", among other things.

See these resources:

  - <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a#Security_and_privacy_concerns>
  - <https://www.jitbit.com/alexblog/256-targetblank---the-most-underestimated-vulnerability-ever>

-}
targetBlank : List (Attribute msg)
targetBlank =
    [ Attributes.target "_blank"
    , Attributes.rel "noopener noreferrer"
    ]
