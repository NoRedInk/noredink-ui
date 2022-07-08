module Nri.Ui.Svg.V1 exposing
    ( Svg, withViewBox
    , withColor, withLabel, withWidth, withHeight, withCss
    , init, render
    )

{-|

@docs Svg, withViewBox
@docs withColor, withLabel, withWidth, withHeight, withCss
@docs init, render

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Role as Role
import Css exposing (Color)
import Nri.Ui.Colors.Extra exposing (toCssString)
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
        }


{-| Tag html as being an svg.
-}
init : List (Svg.Styled.Svg Never) -> Svg
init icon =
    Svg
        { icon = icon
        , color = Nothing
        , height = Nothing
        , width = Nothing
        , css = []
        , label = Nothing
        , viewBox = "0 0 25 25"
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
withViewBox : String -> Svg -> Svg
withViewBox viewBox (Svg record) =
    Svg { record | viewBox = viewBox }


{-| Render an svg.
-}
render : Svg -> Svg.Styled.Svg msg
render (Svg record) =
    let
        width =
            Maybe.map Css.width record.width
                |> Maybe.withDefault (Css.width (Css.pct 100))

        height =
            Maybe.map Css.height record.height
                |> Maybe.withDefault (Css.height (Css.pct 100))

        color =
            fill (Maybe.withDefault "currentcolor" (Maybe.map toCssString record.color))
    in
    Svg.Styled.svg
        ([ Just (viewBox record.viewBox)
         , Just color
         , -- TODO: what css is supported on this svg node?
           Just (css (width :: height :: record.css))
         , -- TODO: Use a title svg node instead
           Maybe.map Aria.label record.label
            |> Maybe.withDefault (Aria.hidden True)
            |> Just
         , -- TODO: double check property
           Just Role.img

         -- TODO: make sure svg is not focusable
         ]
            |> List.filterMap identity
        )
        record.icon
        |> Svg.Styled.map never
