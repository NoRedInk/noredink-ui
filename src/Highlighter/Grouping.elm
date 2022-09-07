module Highlighter.Grouping exposing (buildGroups, inSameRange)

{-|

@docs buildGroups, inSameRange

Grouping is a helper to group highlightables with the same uiState/marked.
Start and end of a group is marked with a special tag and groups containing only one highlightable are tagged as standalone.

-}

import List.Extra
import Nri.Ui.Highlightable.V1 exposing (Highlightable, UIState(..))
import Set exposing (Set)


{-| Groups highlightables with the same state together.
-}
buildGroups : List (Highlightable marker) -> List (List (Highlightable marker))
buildGroups =
    List.Extra.groupWhile groupHighlightables
        >> List.map (\( elem, list ) -> elem :: list)


groupHighlightables : Highlightable marker -> Highlightable marker -> Bool
groupHighlightables x y =
    ((x.uiState == y.uiState)
        && (x.marked == Nothing)
        && (y.marked == Nothing)
    )
        || (x.marked == y.marked && x.marked /= Nothing)
        || (x.marked /= Nothing && y.uiState == Hinted)
        || (y.marked /= Nothing && x.uiState == Hinted)


{-| Finds the group indexes of the groups which are in the same highlighting as the group index
passed in the first argument.
-}
inSameRange : Int -> List (Highlightable marker) -> Set Int
inSameRange groupIndex highlightables =
    List.Extra.groupWhile (\a b -> a.marked == b.marked) highlightables
        |> List.map (\( first, rest ) -> first.groupIndex :: List.map .groupIndex rest)
        |> List.Extra.find (List.member groupIndex)
        |> Maybe.withDefault []
        |> Set.fromList
