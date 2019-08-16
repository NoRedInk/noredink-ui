module Nri.Tooltip.V1 exposing (primaryLabel, auxillaryDescription, toggleTip)

{-| A tooltip component!

@docs primaryLabel, auxillaryDescription, toggleTip

These tooltips follow the accessibility recommendations from: <https://inclusive-components.design/tooltips-toggletips>

-}

import Accessibility.Styled exposing (Html, text)


{-| Used when the content of the tooltip is the "primary label" for its content. The tooltip content
will supercede the content of the trigger HTML for screen readers.
-}
primaryLabel : Html msg
primaryLabel =
    text ""


{-| Used when the content of the tooltip provides an "auxillary description" for its content.
-}
auxillaryDescription : Html msg
auxillaryDescription =
    text ""


{-| Supplementary information triggered by a "?" icon
-}
toggleTip : Html msg
toggleTip =
    text ""
