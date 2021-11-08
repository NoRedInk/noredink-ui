module Nri.Ui.Css.V1 exposing (styleIf, styleJust)

{-|

@docs styleIf, styleJust

-}

import Css exposing (Style, batch)


{-| Apply a list of styles only if a condition is `True`.
-}
styleIf : Bool -> List Style -> Style
styleIf condition stuff =
    batch
        (if condition then
            stuff

         else
            []
        )


{-| Apply a style if the `Maybe` is a `Just`, otherwise apply nothing.
-}
styleJust : (a -> Style) -> Maybe a -> Style
styleJust view maybe =
    case maybe of
        Just whatever ->
            view whatever

        Nothing ->
            batch []
