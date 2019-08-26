module Nri.Ui.Colors.Extra exposing (fromCssColor, toCssColor, withAlpha)

{-| Helpers for working with colors.


# Conversions

@docs fromCssColor, toCssColor, withAlpha

-}

import Color
import Css exposing (..)


{-| Convert a Css.Color into a Color.Color
fromCssColor (Css.hex "#FFFFFF") -- "RgbaSpace 1 1 1 1 : Color.Color"
-}
fromCssColor : Css.Color -> Color.Color
fromCssColor cssColor =
    Color.rgba
        (toFloat cssColor.red / 255)
        (toFloat cssColor.green / 255)
        (toFloat cssColor.blue / 255)
        cssColor.alpha


{-| Convert a Color.Color into a Css.Color
toCssColor (Color.rgba 1.0 1.0 1.0 1.0) -- "{ alpha = 1, blue = 255, color = Compatible, green = 255, red = 255, value = "rgba(255, 255, 255, 1)" } : Css.Color"
-}
toCssColor : Color.Color -> Css.Color
toCssColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    Css.rgba (Basics.round (red * 255))
        (Basics.round (green * 255))
        (Basics.round (blue * 255))
        alpha


{-| Add an alpha property to a Css.Color
grassland -- "{ value = "#56bf74", color = Compatible, red = 86, green = 191, blue = 116, alpha = 1, warnings = [] } : Css.Color"
withAlpha 0.5 grassland -- "{ value = "rgba(86, 191, 116, 0.5)", color = Compatible, warnings = [], red = 86, green = 191, blue = 116, alpha = 0.5 } : Css.Color"
-}
withAlpha : Float -> Css.Color -> Css.Color
withAlpha alpha { red, green, blue } =
    Css.rgba red green blue alpha
