module Nri.Ui.Css.V1 exposing (..)

import Css exposing (Style, property)


{-| Apply a style only if a condition is `True`.
-}
styleIf : (() -> Style) -> Bool -> Style
styleIf view condition =
    if condition then
        view ()

    else
        property "-nri" "noop"


{-| View value of if `Maybe` is a `Just`, otherwise show nothing.
-}
styleJust : (a -> Style) -> Maybe a -> Style
styleJust view maybe =
    case maybe of
        Just whatever ->
            view whatever

        Nothing ->
            property "-nri" "noop"
