module Nri.Ui.Shadows.V1 exposing (low, medium, high)

{-| We use three levels of shadow:

@docs low, medium, high

-}

import Css exposing (Style)


{-| Low: for standard containers and similar elements like large messages
-}
low : Style
low =
    Css.property "box-shadow" "0 0.8px 0.7px hsl(0deg 0% 0% / 0.1), 0 1px 1px -1.2px hsl(0deg 0% 0% / 0.1), 0 5px 2.5px -2.5px hsl(0deg 0% 0% / 0.1);"


{-| Medium: for larger, more prominent containers like Container.Pillow and marketing site cards
-}
medium : Style
medium =
    Css.property "box-shadow" "0 0.5px 0.7px hsl(0deg 0% 0% / 0.075), 0 1.6px 2px -0.8px hsl(0deg 0% 0% / 0.075), 0 4.1px 5.2px -1.7px hsl(0deg 0% 0% / 0.075), 5px 10px 12.6px -2.5px hsl(0deg 0% 0% / 0.075);"


{-| High: for “floating” elements like tooltips, popovers, and modals
-}
high : Style
high =
    Css.property "box-shadow" "0 1px 1px hsl(0deg 0% 0% / 0.075), 0 2px 2px hsl(0deg 0% 0% / 0.075), 0 4px 4px hsl(0deg 0% 0% / 0.075), 0 8px 8px hsl(0deg 0% 0% / 0.075), 0 16px 16px hsl(0deg 0% 0% / 0.075)"
