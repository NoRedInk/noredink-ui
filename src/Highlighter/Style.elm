module Highlighter.Style exposing
    ( dynamicHighlighted
    , groupPosition
    , staticHighlighted
    )

{-| This module is responsible for getting the correct CSS classes for a Highlightable.
-}

import Css
import Highlighter.Grouping as Grouping
import Nri.Ui.Highlightable.V1 as Highlightable exposing (Highlightable, UIState(..))
import Nri.Ui.HighlighterTool.V1 as Tool


highlightedStyle : Tool.MarkerModel marker -> Bool -> UIState -> Maybe (Tool.MarkerModel marker) -> Css.Style
highlightedStyle marker interactive uiState marked =
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
                Css.batch marker.hoverClass

            else
                Css.backgroundColor Css.transparent

        ( Hinted, _ ) ->
            Css.batch marker.hintClass

        ( None, Just markedWith ) ->
            Css.batch markedWith.highlightClass

        ( None, Nothing ) ->
            Css.backgroundColor Css.transparent


dynamicHighlighted : Tool.MarkerModel marker -> Grouping.Position -> Bool -> UIState -> Maybe (Tool.MarkerModel marker) -> Css.Style
dynamicHighlighted marker groupPos interactive uiState marked =
    let
        groupStyle =
            case ( uiState, interactive, marked ) of
                ( Hovered, False, Nothing ) ->
                    Css.batch []

                ( Hovered, False, Just markedWith ) ->
                    groupPosition groupPos markedWith

                ( Hovered, True, _ ) ->
                    -- Hovered interactive always uses the cursor's marker
                    groupPosition groupPos marker

                ( Hinted, _, _ ) ->
                    groupPosition groupPos marker

                ( None, _, Just markedWith ) ->
                    groupPosition groupPos markedWith

                ( None, _, Nothing ) ->
                    Css.batch []
    in
    Css.batch [ groupStyle, highlightedStyle marker interactive uiState marked ]


{-| Style for static views.
-}
staticHighlighted : Grouping.Position -> Highlightable marker -> Css.Style
staticHighlighted groupPos { customAttributes, marked } =
    case marked of
        Just markedWith ->
            Css.batch
                [ Css.batch markedWith.highlightClass
                , groupPosition groupPos markedWith
                ]

        Nothing ->
            Css.batch
                [ Css.backgroundColor Css.transparent ]


{-| Adds a class to the different parts of a section
-}
groupPosition :
    Grouping.Position
    -> { a | startGroupClass : List Css.Style, endGroupClass : List Css.Style }
    -> Css.Style
groupPosition groupPos marker =
    case groupPos of
        Grouping.Start ->
            Css.batch marker.startGroupClass

        Grouping.End ->
            Css.batch marker.endGroupClass

        Grouping.Standalone ->
            Css.batch (marker.startGroupClass ++ marker.endGroupClass)

        _ ->
            Css.batch []
