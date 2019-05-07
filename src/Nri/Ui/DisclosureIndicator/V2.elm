module Nri.Ui.DisclosureIndicator.V2 exposing (view, viewInline)

{-| A caret that indicates that a section can expand. When the isOpen attribute is passed in as True, it will rotate. A "disclosure indicator" is a standard term for something that indicates that section can expand.

@docs view, viewInline

-}

import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (alt, type_)
import Nri.Ui.AssetPath as AssetPath


type alias Config =
    { isOpen : Bool
    , label : String
    }


type alias Assets r =
    { r
        | icons_arrowDownBlue_svg : AssetPath.Asset
    }


{-| -}
view : Assets a -> Config -> Html msg
view assets config =
    viewWithStyle
        [ marginRight (px 10)
        , width (px 15)
        , height (px 15)
        , cursor pointer
        , property "transition" "transform 0.2s"
        , if config.isOpen then
            transform (rotate <| deg 0)

          else
            transform (rotate <| deg -90)
        ]
        assets
        config


{-| The inline variant of the indicator is smaller and occupies
less vertical space so it can be inlined in lists or tables
without breaking text flow. Also, it rotates from right to
down direction when expanding.
-}
viewInline : Assets a -> Config -> Html msg
viewInline assets config =
    viewWithStyle
        [ padding2 (px 0) (px 8)
        , height (px 9)
        , cursor pointer
        , property "transition" "transform 0.1s"
        , if config.isOpen then
            transform (rotate <| deg 0)

          else
            transform (rotate <| deg -90)
        ]
        assets
        config


viewWithStyle : List Css.Style -> Assets a -> Config -> Html msg
viewWithStyle style assets config =
    let
        label =
            if config.isOpen then
                "hide " ++ config.label

            else
                "show " ++ config.label
    in
    img
        [ alt label
        , Attributes.src <| AssetPath.url <| assets.icons_arrowDownBlue_svg
        , Attributes.css style
        ]
        []
