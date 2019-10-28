module Nri.Ui.Svg.V1 exposing
    ( Svg
    , withColor
    , fromHtml, toHtml
    )

{-|

@docs Svg
@docs withColor
@docs fromHtml, toHtml

-}

import Css exposing (Color)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes


{-| -}
type Svg
    = Svg (Html Never)


{-| -}
withColor : Color -> Svg -> Svg
withColor color (Svg svg) =
    Svg (Html.span [ Attributes.css [ Css.color color ] ] [ svg ])


{-| Tag html as being an svg.
-}
fromHtml : Html Never -> Svg
fromHtml =
    Svg


{-| Render an svg.
-}
toHtml : Svg -> Html msg
toHtml (Svg svg) =
    Html.map never svg
