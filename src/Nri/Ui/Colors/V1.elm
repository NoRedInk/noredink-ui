module Nri.Ui.Colors.V1 exposing
    ( aqua, aquaDark, aquaLight, azure, azureDark
    , white
    , blue, blueDeep
    , cornflower, cornflowerDark, cornflowerLight, cyan
    , frost
    , gray20, gray45, gray75, gray92, gray96
    , glacier, grassland, green, greenDark, greenDarkest, greenLight, greenLightest
    , highlightLightBlue, highlightLightMagenta, highlightLightYellow
    , highlightBlue, highlightBlueDark
    , highlightCyan, highlightCyanDark
    , highlightGreen, highlightGreenDark
    , highlightMagenta, highlightMagentaDark
    , highlightPurple, highlightPurpleDark
    , highlightYellow, highlightYellowDark
    , highlightBrown, highlightBrownDark
    , lichen
    , magenta
    , navy
    , orange, ochre, ochreDark
    , purple, purpleDark, purpleLight
    , red, redDark, redLight
    , sunshine
    , turquoise, turquoiseDark, turquoiseLight
    , yellow
    )

{-| Comprehensive list of named colors.

For helpers & conversions, see Nri.Ui.Colors.Extra, or
consider [elm-color-extra](http://package.elm-lang.org/packages/eskimoblood/elm-color-extra/5.0.0/).

@docs aqua, aquaDark, aquaLight, azure, azureDark
@docs white
@docs blue, blueDeep
@docs cornflower, cornflowerDark, cornflowerLight, cyan
@docs frost
@docs gray20, gray45, gray75, gray92, gray96
@docs glacier, grassland, green, greenDark, greenDarkest, greenLight, greenLightest
@docs highlightLightBlue, highlightLightMagenta, highlightLightYellow
@docs highlightBlue, highlightBlueDark
@docs highlightCyan, highlightCyanDark
@docs highlightGreen, highlightGreenDark
@docs highlightMagenta, highlightMagentaDark
@docs highlightPurple, highlightPurpleDark
@docs highlightYellow, highlightYellowDark
@docs highlightBrown, highlightBrownDark
@docs lichen
@docs magenta
@docs navy
@docs orange, ochre, ochreDark
@docs purple, purpleDark, purpleLight
@docs red, redDark, redLight
@docs sunshine
@docs turquoise, turquoiseDark, turquoiseLight
@docs yellow

-}

import Css exposing (hex, rgba)
import Nri.Ui.Colors.Extra exposing (withAlpha)


{-|

<p style="font-size:2em; color: #00cbeb">#00cbeb</p>

-}
aqua : Css.Color
aqua =
    hex "#00cbeb"


{-|

<p style="font-size:2em; color: #008da3">#008da3</p>

-}
aquaDark : Css.Color
aquaDark =
    hex "#008da3"


{-|

<p style="font-size:2em; color: #e6fcff">#e6fcff</p>

-}
aquaLight : Css.Color
aquaLight =
    hex "#e6fcff"


{-|

<p style="font-size:2em; color: #146aff">#146aff</p>

-}
azure : Css.Color
azure =
    hex "#0A64FF"


{-|

<p style="font-size:2em; color: #004cc9">#004cc9</p>

-}
azureDark : Css.Color
azureDark =
    hex "#004cc9"


{-| TODO

<p style="font-size:2em; color: #40a8e4">#40a8e4</p>

-}
blue : Css.Color
blue =
    hex "#40a8e4"


{-| TODO

<p style="font-size:2em; color: #4a79a7">#4a79a7</p>

-}
blueDeep : Css.Color
blueDeep =
    hex "#4a79a7"


{-|

<p style="font-size:2em; color: #00aaff">#00aaff</p>

-}
cornflower : Css.Color
cornflower =
    hex "#00aaff"


{-|

<p style="font-size:2em; color: #0074ad">#0074ad</p>

-}
cornflowerDark : Css.Color
cornflowerDark =
    hex "#0074ad"


{-|

<p style="font-size:2em; color: #e6f7ff">#e6f7ff</p>

-}
cornflowerLight : Css.Color
cornflowerLight =
    hex "#e6f7ff"


{-|

<p style="font-size:2em; color: #43dcff">#43dcff</p>

-}
cyan : Css.Color
cyan =
    hex "#43dcff"


{-|

<p style="font-size:2em; color: #eef9ff">#eef9ff</p>

-}
frost : Css.Color
frost =
    hex "#eef9ff"


{-|

<p style="font-size:2em; color: #d4f0ff">#d4f0ff</p>

-}
glacier : Css.Color
glacier =
    hex "#d4f0ff"


{-|

<p style="font-size:2em; color: #56bf74">#56bf74</p>

-}
grassland : Css.Color
grassland =
    hex "#56bf74"


{-|

<p style="font-size:2em; color: #333333">#333333</p>

-}
gray20 : Css.Color
gray20 =
    hex "#333333"


{-|

<p style="font-size:2em; color: #727272">#727272</p>

-}
gray45 : Css.Color
gray45 =
    hex "#727272"


{-|

<p style="font-size:2em; color: #bfbfbf">#bfbfbf</p>

-}
gray75 : Css.Color
gray75 =
    hex "#bfbfbf"


{-|

<p style="font-size:2em; color: #ebebeb">#ebebeb</p>

-}
gray92 : Css.Color
gray92 =
    hex "#ebebeb"


{-|

<p style="font-size:2em; color: #f5f5f5">#f5f5f5</p>

-}
gray96 : Css.Color
gray96 =
    hex "#f5f5f5"


{-|

<p style="font-size:2em; color: #00d93e">#00d93e</p>

-}
green : Css.Color
green =
    hex "#00d93e"


{-|

<p style="font-size:2em; color: #26a300">#26a300</p>

-}
greenDark : Css.Color
greenDark =
    hex "#26a300"


{-|

<p style="font-size:2em; color: #228000">#228000</p>

-}
greenDarkest : Css.Color
greenDarkest =
    hex "#228000"


{-|

<p style="font-size:2em; color: #b3ffc9">#b3ffc9</p>

-}
greenLight : Css.Color
greenLight =
    hex "#b3ffc9"


{-|

<p style="font-size:2em; color: #e6ffed; background-color: black;">#e6ffed</p>

-}
greenLightest : Css.Color
greenLightest =
    hex "#e6ffed"


{-| cyan with alpha of 0.75

<p style="font-size:2em; color: rgba(66, 219, 255, 0.75)">rgba(66, 219, 255, 0.75)</p>

-}
highlightLightBlue : Css.Color
highlightLightBlue =
    withAlpha 0.75 cyan


{-| magenta with alpha of 0.5

<p style="font-size:2em; color: rgba(255, 0 ,189, 0.5)">rgba(255, 0 ,189, 0.5)</p>

-}
highlightLightMagenta : Css.Color
highlightLightMagenta =
    withAlpha 0.5 magenta


{-| yellow with alpha of 0.75

<p style="font-size:2em; color: rgba(254, 199 ,9, 0.75)">rgba(254, 199 ,9, 0.75)</p>

-}
highlightLightYellow : Css.Color
highlightLightYellow =
    withAlpha 0.75 yellow


{-|

<p style="font-size:2em; background-color: #fee798">#fee798</p>

-}
highlightYellow : Css.Color
highlightYellow =
    hex "#fee798"


{-|

<p style="font-size:2em; background-color: #795d01">#795d01</p>

-}
highlightYellowDark : Css.Color
highlightYellowDark =
    hex "#795d01"


{-|

<p style="font-size:2em; background-color: #b9fff7">#b9fff7</p>

-}
highlightCyan : Css.Color
highlightCyan =
    hex "#9BFFF4"


{-|

<p style="font-size:2em; background-color: #006156">#006156</p>

-}
highlightCyanDark : Css.Color
highlightCyanDark =
    hex "#006156"


{-|

<p style="font-size:2em; background-color: #ffb8f3">#ffb8f3</p>

-}
highlightMagenta : Css.Color
highlightMagenta =
    hex "#ffb8f3"


{-|

<p style="font-size:2em; background-color: #a30088">#a30088</p>

-}
highlightMagentaDark : Css.Color
highlightMagentaDark =
    hex "#a30088"


{-|

<p style="font-size:2em; background-color: #8fffb3">#8fffb3</p>

-}
highlightGreen : Css.Color
highlightGreen =
    hex "#8fffb3"


{-|

<p style="font-size:2em; background-color: #007025">#007025</p>

-}
highlightGreenDark : Css.Color
highlightGreenDark =
    hex "#007025"


{-|

<p style="font-size:2em; background-color: #ccdeff">#ccdeff</p>

-}
highlightBlue : Css.Color
highlightBlue =
    hex "#ccdeff"


{-|

<p style="font-size:2em; background-color: #002e85">#002e85</p>

-}
highlightBlueDark : Css.Color
highlightBlueDark =
    hex "#002e85"


{-|

<p style="font-size:2em; background-color: #c9b0ff">#c9b0ff</p>

-}
highlightPurple : Css.Color
highlightPurple =
    hex "#e3d6ff"


{-|

<p style="font-size:2em; background-color: #3c00bd">#3c00bd</p>

-}
highlightPurpleDark : Css.Color
highlightPurpleDark =
    hex "#3c00bd"


{-|

<p style="font-size:2em; background-color: #ffc6a1">#ffc6a1</p>

-}
highlightBrown : Css.Color
highlightBrown =
    hex "#ffc6a1"


{-|

<p style="font-size:2em; background-color: #943b00">#943b00</p>

-}
highlightBrownDark : Css.Color
highlightBrownDark =
    hex "#943b00"


{-|

<p style="font-size:2em; color: #99bfa4">#99bfa4</p>

-}
lichen : Css.Color
lichen =
    hex "#99bfa4"


{-|

<p style="font-size:2em; color: #ff00bd">#ff00bd</p>

-}
magenta : Css.Color
magenta =
    hex "#ff00bd"


{-|

<p style="font-size:2em; color: #004e95">#004e95</p>

-}
navy : Css.Color
navy =
    hex "#004e95"


{-| -- TODO

<p style="font-size:2em; color: #f5a623">#f5a623</p>

-}
orange : Css.Color
orange =
    hex "#f5a623"


{-|

<p style="font-size:2em; color: #f28f00">#f28f00</p>

-}
ochre : Css.Color
ochre =
    hex "#f28f00"


{-|

<p style="font-size:2em; color: #ad6500">#ad6500</p>

-}
ochreDark : Css.Color
ochreDark =
    hex "#ad6500"


{-|

<p style="font-size:2em; color: #a839e7">#a839e7</p>

-}
purple : Css.Color
purple =
    hex "#a839e7"


{-|

<p style="font-size:2em; color: #f7ebff">#f7ebff</p>

-}
purpleLight : Css.Color
purpleLight =
    hex "#f7ebff"


{-|

<p style="font-size:2em; color: #7721a7">#7721a7</p>

-}
purpleDark : Css.Color
purpleDark =
    hex "#7721a7"


{-|

<p style="font-size:2em; color: #f3336c">#f3336c</p>

-}
red : Css.Color
red =
    hex "#e70d4f"


{-|

<p style="font-size:2em; color: #ffe0e6">#ffe0e6</p>

-}
redLight : Css.Color
redLight =
    hex "#ffe0e6"


{-|

<p style="font-size:2em; color: #c2003a">#c2003a</p>

-}
redDark : Css.Color
redDark =
    hex "#c2003a"


{-|

<p style="font-size:2em; color: #fffadc">#fffadc</p>

-}
sunshine : Css.Color
sunshine =
    hex "#fffadc"


{-|

<p style="font-size:2em; color: #00cfbe">#00cfbe</p>

-}
turquoise : Css.Color
turquoise =
    hex "#00cfbe"


{-|

<p style="font-size:2em; color: #00a39b">#00a39b</p>

-}
turquoiseDark : Css.Color
turquoiseDark =
    hex "#00a39b"


{-|

<p style="font-size:2em; color: #e0fffe">#e0fffe</p>

-}
turquoiseLight : Css.Color
turquoiseLight =
    hex "#e0fffe"


{-|

<p style="font-size:2em; color: #ffffff; background-color: black;">#ffffff</p>

-}
white : Css.Color
white =
    hex "#ffffff"


{-|

<p style="font-size:2em; color: #fec709">#fec709</p>

-}
yellow : Css.Color
yellow =
    hex "#fec709"
