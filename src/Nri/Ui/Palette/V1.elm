module Nri.Ui.Palette.V1 exposing
    ( Palette, PaletteName(..)
    , white, gray, darkGray, blue, darkBlue, purple, turquoise, green, red, aqua, cornflower
    )

{-| Predefined color palettes for use in configurable components

@docs Palette, PaletteName
@docs white, gray, darkGray, blue, darkBlue, purple, turquoise, green, red, aqua, cornflower

-}

import Css
import Nri.Ui.Colors.V1 as Colors


{-| -}
type alias Palette =
    { border : Css.Color
    , background : Css.Color
    , primary : Css.Color
    , name : PaletteName
    }


{-| -}
type PaletteName
    = Gray
    | DarkGray
    | Blue
    | DarkBlue
    | Purple
    | Turquoise
    | Red
    | Green
    | White
    | Cornflower
    | Aqua


{-| Gray palette
-}
gray : Palette
gray =
    { border = Colors.gray75
    , background = Colors.gray96
    , primary = Colors.gray75
    , name = Gray
    }


{-| Aqua palette
-}
aqua : Palette
aqua =
    { border = Colors.aqua
    , background = Colors.aquaLight
    , primary = Colors.aqua
    , name = Aqua
    }


{-| Dark Gray palette
-}
darkGray : Palette
darkGray =
    { border = Colors.gray45
    , background = Colors.gray96
    , primary = Colors.gray45
    , name = DarkGray
    }


{-| Blue palette
-}
blue : Palette
blue =
    { border = Colors.azure
    , background = Colors.frost
    , primary = Colors.azure
    , name = Blue
    }


{-| Dark blue palette
-}
darkBlue : Palette
darkBlue =
    { border = Colors.navy
    , background = Colors.frost
    , primary = Colors.navy
    , name = DarkBlue
    }


{-| Purple palette
-}
purple : Palette
purple =
    { border = Colors.purple
    , background = Colors.purpleLight
    , primary = Colors.purple
    , name = Purple
    }


{-| Turquoise palette
-}
turquoise : Palette
turquoise =
    { border = Colors.turquoise
    , background = Colors.turquoiseLight
    , primary = Colors.turquoise
    , name = Turquoise
    }


{-| Green palette
-}
green : Palette
green =
    { border = Colors.green
    , background = Colors.greenLightest
    , primary = Colors.green
    , name = Green
    }


{-| Red palette
-}
red : Palette
red =
    { border = Colors.red
    , background = Colors.redLight
    , primary = Colors.red
    , name = Red
    }


{-| White palette (borders are blue)
-}
white : Palette
white =
    { border = Colors.navy
    , background = Colors.white
    , primary = Colors.navy
    , name = White
    }


{-| Cornflower palette
-}
cornflower : Palette
cornflower =
    { border = Colors.cornflower
    , background = Colors.cornflowerLight
    , primary = Colors.cornflower
    , name = Cornflower
    }
