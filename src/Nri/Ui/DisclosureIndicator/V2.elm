module Nri.Ui.DisclosureIndicator.V2 exposing (view)

{-|


# Changes from V1

  - Removes dependency on Icon that makes versioned assets hard to work with
  - Allows for customized size and styles with a single `view` function
  - Removes `Config` in favor of an explicit type annotation

(If you need the old `view` and `viewInline` styles, please copy the configurations
from the styleguide examples.)


# About:

A caret that indicates that a section can expand and collapse. When `isOpen` is True, the caret will rotate down.
"Disclosure indicator" is a standard term for something that indicates that section can expand.

@docs view

-}

import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.SpriteSheet exposing (arrowLeft)
import Nri.Ui.Svg.V1 as NriSvg


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
             , width size
             , height size
             ]
                ++ styles
            )
        ]
        [ div
            [ css
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
            ]
            [ NriSvg.toHtml arrowLeft
            ]
        ]
