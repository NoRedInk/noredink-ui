module Nri.Ui.HighlighterTool.V1 exposing (Tool(..), EraserModel, MarkerModel)

{-|

@docs Tool, EraserModel, MarkerModel

-}

import Css


{-| Tool that can be used on a highlighter
-}
type Tool kind
    = Marker (MarkerModel kind)
    | Eraser EraserModel


{-| EraserModel
-}
type alias EraserModel =
    { hoverClass : List Css.Style
    , hintClass : List Css.Style
    , startGroupClass : List Css.Style
    , endGroupClass : List Css.Style
    }


{-| A marker is used to highlight highlightables.
A user can define a `kind`, which contains the meaning of a selection.
Marker is also used to define styling of a selection.
-}
type alias MarkerModel kind =
    { hoverClass : List Css.Style
    , hintClass : List Css.Style
    , startGroupClass : List Css.Style
    , endGroupClass : List Css.Style
    , highlightClass : List Css.Style
    , hoverHighlightClass : List Css.Style
    , kind : kind
    }
