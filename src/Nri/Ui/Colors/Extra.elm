module Nri.Ui.Colors.Extra exposing (toCoreColor, withAlpha)

{-| Helpers for working with colors.


# Conversions

@docs toCoreColor, withAlpha

-}

import Color
import Css exposing (..)


{-| Convert a Css.Color into a Color.Color
toCoreColor (Css.hex "#FFFFFF") -- "RGBA 255 255 255 1 : Color.Color"
-}
toCoreColor : Css.Color -> Color.Color
toCoreColor cssColor =
    Color.rgba
        (toFloat cssColor.red / 255)
        (toFloat cssColor.green / 255)
        (toFloat cssColor.blue / 255)
        cssColor.alpha


{-| Add an alpha property to a Css.Color
grassland -- "{ value = "#56bf74", color = Compatible, red = 86, green = 191, blue = 116, alpha = 1, warnings = [] } : Css.Color"
withAlpha 0.5 grassland -- "{ value = "rgba(86, 191, 116, 0.5)", color = Compatible, warnings = [], red = 86, green = 191, blue = 116, alpha = 0.5 } : Css.Color"
-}
withAlpha : Float -> Css.Color -> Css.Color
withAlpha alpha { red, green, blue } =
    Css.rgba red green blue alpha
