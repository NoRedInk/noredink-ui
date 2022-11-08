module Highlighter.Style exposing (dynamicHighlighted)

{-| This module is responsible for getting the correct CSS classes for a Highlightable.
-}

import Css
import Nri.Ui.Highlightable.V1 exposing (UIState(..))
import Nri.Ui.HighlighterTool.V1 as Tool


{-| -}
dynamicHighlighted : Tool.MarkerModel marker -> Bool -> UIState -> Maybe (Tool.MarkerModel marker) -> Css.Style
dynamicHighlighted marker interactive uiState marked =
    case ( uiState, marked ) of
        ( Hovered, Just markedWith ) ->
            if interactive then
                -- Override marking with cursor's marker if interactive
                Css.batch marker.hoverHighlightClass

            else
                -- Use the marked style if non-interactive
                Css.batch markedWith.hoverHighlightClass

        ( Hovered, Nothing ) ->
            if interactive then
                [ marker.hoverClass
                , marker.startGroupClass
                , marker.endGroupClass
                ]
                    |> List.concat
                    |> Css.batch

            else
                Css.backgroundColor Css.transparent

        ( Hinted, _ ) ->
            Css.batch marker.hintClass

        ( None, Just markedWith ) ->
            Css.batch markedWith.highlightClass

        ( None, Nothing ) ->
            Css.backgroundColor Css.transparent
