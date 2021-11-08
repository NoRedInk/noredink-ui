module Nri.Ui.Css.V1 exposing (styleIf, styleJust)

{-|

@docs styleIf, styleJust

-}

import Css exposing (Style, batch)


{-| Apply a style only if a condition is `True`.
-}
styleIf : Bool -> Style -> Style
styleIf condition view =
    if condition then
        view

    else
        batch []


{-| Apply a style if the `Maybe` is a `Just`, otherwise apply nothing.
-}
styleJust : (a -> Style) -> Maybe a -> Style
styleJust view maybe =
    case maybe of
        Just whatever ->
            view whatever

        Nothing ->
            batch []
