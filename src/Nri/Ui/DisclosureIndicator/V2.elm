module Nri.Ui.DisclosureIndicator.V2 exposing (medium, large)

{-| TODO: Remove this module, in favor of Nri.Ui.AnimatedIcon.


# Changes from V1

  - Removes dependency on Icon that makes versioned assets hard to work with
  - Renames the helpers to `medium` and `large`
  - Removes `Config` in favor of an explicit type annotation


# About:

A caret that indicates that a section can expand and collapse. When `isOpen` is True, the caret will rotate down.
"Disclosure indicator" is a standard term for something that indicates that section can expand.

@docs medium, large

-}

import Css exposing (..)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
medium : List Css.Style -> Bool -> Svg
medium styles isOpen =
    view { isOpen = isOpen, size = px 15, styles = styles }


{-| -}
large : List Css.Style -> Bool -> Svg
large styles isOpen =
    view { isOpen = isOpen, size = px 17, styles = styles }


{-| -}
view :
    { isOpen : Bool
    , size : Css.Px
    , styles : List Css.Style
    }
    -> Svg
view { styles, size, isOpen } =
    UiIcon.arrowLeft
        |> Svg.withColor Colors.azure
        |> Svg.withWidth size
        |> Svg.withHeight size
        |> Svg.withCss
            ([ property "transition" "transform 0.1s"
             , if isOpen then
                transform (rotate (deg -90))

               else
                transform (rotate (deg -180))
             ]
                ++ styles
            )
