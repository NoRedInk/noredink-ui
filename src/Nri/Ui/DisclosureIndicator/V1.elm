module Nri.Ui.DisclosureIndicator.V1 exposing (styles, view, viewInline)

{-| A caret that indicates that a section can expand. When the isOpen attribute is passed in as True, it will rotate. A "disclosure indicator" is a standard term for something that indicates that section can expand.
@docs styles, view, viewInline
-}

import Css exposing (..)
import Css.Foreign exposing (Snippet, adjacentSiblings, children, class, descendants, each, everything, media, selector, withClass)
import Html exposing (..)
import Html.Attributes exposing (alt, type_)
import Nri.Ui.AssetPath as AssetPath
import Nri.Ui.Styles.V1


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
view =
    viewWithStyle HeaderStyle


{-| The inline variant of the indicator is smaller and occupies
less vertical space so it can be inlined in lists or tables
without breaking text flow. Also, it rotates from right to
down direction when expanding.
-}
viewInline : Assets a -> Config -> Html msg
viewInline =
    viewWithStyle InlineStyle


viewWithStyle : CssClasses -> Assets a -> Config -> Html msg
viewWithStyle style assets config =
    let
        label =
            if config.isOpen then
                "hide " ++ config.label
            else
                "show " ++ config.label

        classList =
            if config.isOpen then
                [ style, IsOpen ]
            else
                [ style ]
    in
    img
        [ alt label
        , Html.Attributes.src <| AssetPath.url <| assets.icons_arrowDownBlue_svg
        , styles.class classList
        ]
        []


type CssClasses
    = HeaderStyle
    | InlineStyle
    | IsOpen


{-| -}
styles : Nri.Ui.Styles.V1.Styles Never CssClasses msg
styles =
    Nri.Ui.Styles.V1.styles "Nri-DisclosureIndicator-"
        [ -- TODO: make the focus border of the button not rotate
          Css.Foreign.class HeaderStyle
            [ marginRight (px 10)
            , width (px 15)
            , height (px 15)
            , cursor pointer
            , property "transition" "transform 0.2s"
            , transform (rotate <| deg -90)
            , Css.Foreign.withClass IsOpen
                [ transform (rotate <| deg 0)
                ]
            ]
        , Css.Foreign.class InlineStyle
            [ padding2 (px 0) (px 8)
            , height (px 9)
            , cursor pointer
            , property "transition" "transform 0.1s"
            , transform (rotate <| deg -90)
            , Css.Foreign.withClass IsOpen
                [ transform (rotate <| deg 0)
                ]
            ]
        ]
