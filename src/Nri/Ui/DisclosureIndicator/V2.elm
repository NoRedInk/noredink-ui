module Nri.Ui.DisclosureIndicator.V2 exposing (view, viewInline)

{-| A caret that indicates that a section can expand. When the isOpen attribute is passed in as True, it will rotate. A "disclosure indicator" is a standard term for something that indicates that section can expand.

@docs view, viewInline

-}

import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.SpriteSheet exposing (arrowLeft)
import Nri.Ui.Svg.V1 as NriSvg


type alias Config =
    { isOpen : Bool
    }


{-| -}
view : Config -> Html msg
view config =
    viewWithStyle
        [ marginRight (px 10) ]
        (px 15)
        config


{-| The inline variant of the indicator is smaller and occupies
less vertical space so it can be inlined in lists or tables
without breaking text flow. Also, it rotates from right to
down direction when expanding.
-}
viewInline : Config -> Html msg
viewInline config =
    viewWithStyle
        [ padding2 (px 0) (px 8) ]
        (px 9)
        config


viewWithStyle : List Css.Style -> Css.Px -> Config -> Html msg
viewWithStyle style size config =
    div
        [ css
            ([ Css.display Css.inlineBlock
             , cursor pointer
             , width size
             , height size
             ]
                ++ style
            )
        ]
        [ div
            [ css
                [ Css.displayFlex
                , Css.justifyContent Css.center
                , Css.alignItems Css.center
                , color Colors.azure
                , property "transition" "transform 0.1s"
                , if config.isOpen then
                    transform (rotate (deg -90))

                  else
                    transform (rotate (deg -180))
                ]
            ]
            [ NriSvg.toHtml arrowLeft
            ]
        ]
