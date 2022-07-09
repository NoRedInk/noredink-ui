module Nri.Ui.Svg.V1 exposing
    ( Svg
    , withColor, withLabel, withWidth, withHeight, withCss, withCustom
    , init, toHtml
    )

{-|

@docs Svg, withViewBox
@docs withColor, withLabel, withWidth, withHeight, withCss, withCustom
@docs init, toHtml

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Role as Role
import Css exposing (Color)
import Html.Styled.Attributes
import Svg.Styled
import Svg.Styled.Attributes exposing (..)


{-| Opaque type describing a non-interactable Html element.
-}
type Svg
    = Svg
        { icon : List (Svg.Styled.Svg Never)
        , color : Maybe Color
        , width : Maybe Css.Px
        , height : Maybe Css.Px
        , css : List Css.Style
        , label : Maybe String
        , viewBox : String
        , attributes : List (Svg.Styled.Attribute Never)
        }


{-| Pass through the viewbox as the first argument and the contents of the svg node as the second argument.
-}
init : String -> List (Svg.Styled.Svg Never) -> Svg
init viewBox icon =
    Svg
        { icon = icon
        , color = Nothing
        , height = Nothing
        , width = Nothing
        , css = [ Css.flexShrink Css.zero ]
        , label = Nothing
        , viewBox = viewBox
        , attributes = []
        }


{-| -}
withColor : Color -> Svg -> Svg
withColor color (Svg record) =
    Svg { record | color = Just color }


{-| Add a title to the svg. Note that when the label is _not_ present, the icon will be entirely hidden from screenreader users.

Read [Carie Fisher's "Accessible Svgs"](https://www.smashingmagazine.com/2021/05/accessible-svg-patterns-comparison/) article to learn more about accessible svgs.

Go through the [WCAG images tutorial](https://www.w3.org/WAI/tutorials/images/) to learn more about identifying when images are functional or decorative or something else.

-}
withLabel : String -> Svg -> Svg
withLabel label (Svg record) =
    Svg { record | label = Just label }


{-| -}
withWidth : Css.Px -> Svg -> Svg
withWidth width (Svg record) =
    Svg { record | width = Just width }


{-| -}
withHeight : Css.Px -> Svg -> Svg
withHeight height (Svg record) =
    Svg { record | height = Just height }


{-| Css for the SVG's container.
-}
withCss : List Css.Style -> Svg -> Svg
withCss css (Svg record) =
    Svg { record | css = record.css ++ css }


{-| -}
withCustom : List (Svg.Styled.Attribute Never) -> Svg -> Svg
withCustom attributes (Svg record) =
    Svg { record | attributes = record.attributes ++ attributes }


{-| render an svg.
-}
toHtml : Svg -> Svg.Styled.Svg msg
toHtml (Svg record) =
    let
        width =
            Maybe.map Css.width record.width
                |> Maybe.withDefault (Css.width (Css.pct 100))

        height =
            Maybe.map Css.height record.height
                |> Maybe.withDefault (Css.height (Css.pct 100))

        color =
            Maybe.map Css.color record.color
                |> Maybe.withDefault (Css.batch [])
    in
    Svg.Styled.svg
        (List.filterMap identity
            [ Just (viewBox record.viewBox)
            , Just (fill "currentcolor")
            , Just (css (width :: height :: color :: record.css))
            , -- TODO: Use a title svg node instead
              Maybe.map Aria.label record.label
                |> Maybe.withDefault (Aria.hidden True)
                |> Just
            , Just Role.img
            , Just (Html.Styled.Attributes.attribute "focusable" "false")
            ]
            ++ record.attributes
        )
        (case record.label of
            Just label ->
                Svg.Styled.title [] [ Svg.Styled.text label ]
                    :: record.icon

            Nothing ->
                record.icon
        )
        |> Svg.Styled.map never
