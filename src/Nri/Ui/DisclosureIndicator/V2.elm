module Nri.Ui.DisclosureIndicator.V2 exposing (medium, large)

{-|


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
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.SpriteSheet exposing (arrowLeft)
import Nri.Ui.Svg.V1 as NriSvg


{-| -}
medium : List Css.Style -> Bool -> Html msg
medium styles isOpen =
    view { isOpen = isOpen, size = px 15, styles = styles }


{-| -}
large : List Css.Style -> Bool -> Html msg
large styles isOpen =
    view { isOpen = isOpen, size = px 17, styles = styles }


{-| -}
view :
    { isOpen : Bool
    , size : Css.Px
    , styles : List Css.Style
    }
    -> Html msg
view { styles, size, isOpen } =
    div
        [ css
            ([ Css.display Css.inlineBlock
             , cursor pointer
             , minWidth size
             , minHeight size
             , maxWidth size
             , maxHeight size
             ]
                ++ styles
            )
        ]
        [ arrowLeft
            |> NriSvg.withCss
                [ Css.displayFlex
                , Css.justifyContent Css.center
                , Css.alignItems Css.center
                , color Colors.azure
                , property "transition" "transform 0.1s"
                , if isOpen then
                    transform (rotate (deg -90))

                  else
                    transform (rotate (deg -180))
                ]
            |> NriSvg.toHtml
        ]
