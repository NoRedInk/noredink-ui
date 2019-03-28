module Nri.Ui.Svg.V1 exposing
    ( Svg
    , fromHtml, toHtml
    )

{-|

@docs Svg
@docs fromHtml, toHtml

-}

import Html.Styled as Html exposing (Html)
import Svg


{-| -}
type Svg
    = Svg (Html Never)


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
