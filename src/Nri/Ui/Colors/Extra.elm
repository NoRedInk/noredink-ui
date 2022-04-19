module Nri.Ui.Colors.Extra exposing
    ( toCssColor, fromCssColor
    , withAlpha
    , toCssString
    )

{-| Helpers for working with colors.


# Conversions

@docs toCssColor, fromCssColor
@docs withAlpha
@docs toCssString

-}

import Css
import SolidColor exposing (SolidColor)


{-| -}
fromCssColor : Css.Color -> SolidColor
fromCssColor color =
    SolidColor.fromRGB
        ( toFloat color.red
        , toFloat color.green
        , toFloat color.blue
        )


{-| -}
toCssColor : SolidColor -> Css.Color
toCssColor color =
    let
        ( red, green, blue ) =
            SolidColor.toRGB color
    in
    Css.rgb (round red) (round green) (round blue)


{-| Add an alpha property to a Css.Color
grassland -- "{ value = "#56bf74", color = Compatible, red = 86, green = 191, blue = 116, alpha = 1, warnings = [] } : Css.Color"
withAlpha 0.5 grassland -- "{ value = "rgba(86, 191, 116, 0.5)", color = Compatible, warnings = [], red = 86, green = 191, blue = 116, alpha = 0.5 } : Css.Color"
-}
withAlpha : Float -> Css.Color -> Css.Color
withAlpha alpha { red, green, blue } =
    Css.rgba red green blue alpha


{-| -}
toCssString : Css.Color -> String
toCssString =
    SolidColor.toRGBString << fromCssColor
