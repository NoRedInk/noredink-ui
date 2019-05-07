module Nri.Ui.DisclosureIndicator.V2 exposing (view)

{-| A caret that indicates that a section can expand. When the isOpen attribute is passed in as True, it will rotate. A "disclosure indicator" is a standard term for something that indicates that section can expand.

@docs medium, small

-}

import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.SpriteSheet exposing (arrowLeft)
import Nri.Ui.Svg.V1 as NriSvg


type alias Config =
    { isOpen : Bool
    , size : Css.Px
    , styles : List Css.Style
    }


{-| -}
view : Config -> Html msg
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
