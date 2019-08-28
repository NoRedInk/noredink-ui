module Nri.Ui.Colors.Extra exposing
    ( toCssColor, fromCssColor
    , withAlpha
    )

{-| Helpers for working with colors.


# Conversions

@docs toCssColor, fromCssColor
@docs withAlpha

-}

import Color
import Css


{-| -}
fromCssColor : Css.Color -> Color.Color
fromCssColor color =
    Color.fromRGB
        ( toFloat color.red
        , toFloat color.green
        , toFloat color.blue
        )


{-| -}
toCssColor : Color.Color -> Css.Color
toCssColor color =
    let
        ( red, green, blue ) =
            Color.toRGB color
    in
    Css.rgb (round red) (round green) (round blue)


{-| Add an alpha property to a Css.Color
grassland -- "{ value = "#56bf74", color = Compatible, red = 86, green = 191, blue = 116, alpha = 1, warnings = [] } : Css.Color"
withAlpha 0.5 grassland -- "{ value = "rgba(86, 191, 116, 0.5)", color = Compatible, warnings = [], red = 86, green = 191, blue = 116, alpha = 0.5 } : Css.Color"
-}
withAlpha : Float -> Css.Color -> Css.Color
withAlpha alpha { red, green, blue } =
    Css.rgba red green blue alpha
