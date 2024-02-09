module Nri.Ui.Colors.V1 exposing
    ( aqua, aquaDark, aquaLight, azure, azureDark
    , white
    , cornflower, cornflowerDark, cornflowerLight, cyan
    , frost
    , gray20, gray45, gray75, gray85, gray92, gray96
    , glacier, grassland, green, greenDark, greenDarkest, greenLight, greenLightest
    , highlightLightBlue, highlightLightMagenta, highlightLightYellow
    , highlightBlue, highlightBlueLightest, highlightBlueDark
    , highlightCyan, highlightCyanLightest, highlightCyanDark
    , highlightGreen, highlightGreenLightest, highlightGreenDark
    , highlightMagenta, highlightMagentaLightest, highlightMagentaDark
    , highlightPurple, highlightPurpleLightest, highlightPurpleDark
    , highlightYellow, highlightYellowLightest, highlightYellowDark
    , highlightBrown, highlightBrownLightest, highlightBrownDark
    , textHighlightYellow, textHighlightCyan, textHighlightMagenta, textHighlightGreen, textHighlightBlue, textHighlightPurple, textHighlightBrown
    , lichen
    , magenta
    , navy
    , orange, ochre, ochreDark
    , purple, purpleDark, purpleLight
    , red, redDark, redLight
    , sunshine
    , turquoise, turquoiseDark, turquoiseLight
    , mustard
    )

{-| Comprehensive list of named colors.

For helpers & conversions, see Nri.Ui.Colors.Extra, or
consider [elm-color-extra](http://package.elm-lang.org/packages/eskimoblood/elm-color-extra/5.0.0/).

@docs aqua, aquaDark, aquaLight, azure, azureDark
@docs white
@docs cornflower, cornflowerDark, cornflowerLight, cyan
@docs frost
@docs gray20, gray45, gray75, gray85, gray92, gray96
@docs glacier, grassland, green, greenDark, greenDarkest, greenLight, greenLightest
@docs highlightLightBlue, highlightLightMagenta, highlightLightYellow
@docs highlightBlue, highlightBlueLightest, highlightBlueDark
@docs highlightCyan, highlightCyanLightest, highlightCyanDark
@docs highlightGreen, highlightGreenLightest, highlightGreenDark
@docs highlightMagenta, highlightMagentaLightest, highlightMagentaDark
@docs highlightPurple, highlightPurpleLightest, highlightPurpleDark
@docs highlightYellow, highlightYellowLightest, highlightYellowDark
@docs highlightBrown, highlightBrownLightest, highlightBrownDark
@docs textHighlightYellow, textHighlightCyan, textHighlightMagenta, textHighlightGreen, textHighlightBlue, textHighlightPurple, textHighlightBrown
@docs lichen
@docs magenta
@docs navy
@docs orange, ochre, ochreDark
@docs purple, purpleDark, purpleLight
@docs red, redDark, redLight
@docs sunshine
@docs turquoise, turquoiseDark, turquoiseLight
@docs mustard

-}

import Css exposing (hex)
import Nri.Ui.Colors.Extra exposing (withAlpha)


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
aqua : Css.Color
aqua =
    hex "#00cbeb"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
aquaDark : Css.Color
aquaDark =
    hex "#008da3"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
aquaLight : Css.Color
aquaLight =
    hex "#e6fcff"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
azure : Css.Color
azure =
    hex "#0A64FF"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
azureDark : Css.Color
azureDark =
    hex "#004cc9"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
cornflower : Css.Color
cornflower =
    hex "#00aaff"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
cornflowerDark : Css.Color
cornflowerDark =
    hex "#0074ad"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
cornflowerLight : Css.Color
cornflowerLight =
    hex "#e6f7ff"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
cyan : Css.Color
cyan =
    hex "#43dcff"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
frost : Css.Color
frost =
    hex "#eef9ff"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
glacier : Css.Color
glacier =
    hex "#d4f0ff"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
grassland : Css.Color
grassland =
    hex "#56bf74"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
gray20 : Css.Color
gray20 =
    hex "#333333"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
gray45 : Css.Color
gray45 =
    hex "#707070"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
gray75 : Css.Color
gray75 =
    hex "#bfbfbf"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
gray85 : Css.Color
gray85 =
    hex "#d9d9d9"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
gray92 : Css.Color
gray92 =
    hex "#ebebeb"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
gray96 : Css.Color
gray96 =
    hex "#f5f5f5"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
green : Css.Color
green =
    hex "#00d93e"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
greenDark : Css.Color
greenDark =
    hex "#28ab00"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
greenDarkest : Css.Color
greenDarkest =
    hex "#228000"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
greenLight : Css.Color
greenLight =
    hex "#b3ffc9"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
greenLightest : Css.Color
greenLightest =
    hex "#e6ffed"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightLightBlue : Css.Color
highlightLightBlue =
    withAlpha 0.75 cyan


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightLightMagenta : Css.Color
highlightLightMagenta =
    withAlpha 0.5 magenta


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightLightYellow : Css.Color
highlightLightYellow =
    withAlpha 0.75 mustard


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightYellow : Css.Color
highlightYellow =
    hex "#fee798"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightYellowLightest : Css.Color
highlightYellowLightest =
    hex "#fffdf5"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightYellowDark : Css.Color
highlightYellowDark =
    hex "#795d01"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
textHighlightYellow : Css.Color
textHighlightYellow =
    hex "#ab8403"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightCyan : Css.Color
highlightCyan =
    hex "#9BFFF4"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightCyanLightest : Css.Color
highlightCyanLightest =
    hex "#f5fffe"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightCyanDark : Css.Color
highlightCyanDark =
    hex "#006156"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
textHighlightCyan : Css.Color
textHighlightCyan =
    hex "#009e8e"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightMagenta : Css.Color
highlightMagenta =
    hex "#ffb8f3"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightMagentaLightest : Css.Color
highlightMagentaLightest =
    hex "#fff8fe"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightMagentaDark : Css.Color
highlightMagentaDark =
    hex "#a30088"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
textHighlightMagenta : Css.Color
textHighlightMagenta =
    hex "#ff33dd"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightGreen : Css.Color
highlightGreen =
    hex "#8fffb3"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightGreenLightest : Css.Color
highlightGreenLightest =
    hex "#f4fff7"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightGreenDark : Css.Color
highlightGreenDark =
    hex "#007025"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
textHighlightGreen : Css.Color
textHighlightGreen =
    hex "#00a336"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightBlue : Css.Color
highlightBlue =
    hex "#ccdeff"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightBlueLightest : Css.Color
highlightBlueLightest =
    hex "#fafcff"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightBlueDark : Css.Color
highlightBlueDark =
    hex "#002e85"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
textHighlightBlue : Css.Color
textHighlightBlue =
    hex "#3d81ff"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightPurple : Css.Color
highlightPurple =
    hex "#e3d6ff"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightPurpleLightest : Css.Color
highlightPurpleLightest =
    hex "#fcfbff"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightPurpleDark : Css.Color
highlightPurpleDark =
    hex "#3c00bd"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
textHighlightPurple : Css.Color
textHighlightPurple =
    hex "#702eff"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightBrown : Css.Color
highlightBrown =
    hex "#ffc6a1"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightBrownLightest : Css.Color
highlightBrownLightest =
    hex "#fff9f6"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
highlightBrownDark : Css.Color
highlightBrownDark =
    hex "#943b00"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
textHighlightBrown : Css.Color
textHighlightBrown =
    hex "#e05a00"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
lichen : Css.Color
lichen =
    hex "#99bfa4"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
magenta : Css.Color
magenta =
    hex "#ff00bd"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
navy : Css.Color
navy =
    hex "#004e95"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
orange : Css.Color
orange =
    hex "#f5a623"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
ochre : Css.Color
ochre =
    hex "#f28f00"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
ochreDark : Css.Color
ochreDark =
    hex "#ad6500"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
purple : Css.Color
purple =
    hex "#a839e7"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
purpleLight : Css.Color
purpleLight =
    hex "#f7ebff"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
purpleDark : Css.Color
purpleDark =
    hex "#7721a7"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
red : Css.Color
red =
    hex "#e70d4f"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
redLight : Css.Color
redLight =
    hex "#ffe0e6"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
redDark : Css.Color
redDark =
    hex "#c2003a"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
sunshine : Css.Color
sunshine =
    hex "#fffadc"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
turquoise : Css.Color
turquoise =
    hex "#00cfbe"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
turquoiseDark : Css.Color
turquoiseDark =
    hex "#00a39b"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
turquoiseLight : Css.Color
turquoiseLight =
    hex "#e0fffe"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
white : Css.Color
white =
    hex "#ffffff"


{-| See <https://noredink-ui.netlify.com/#/doodad/Colors>
-}
mustard : Css.Color
mustard =
    hex "#fec709"
