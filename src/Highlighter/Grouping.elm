module Highlighter.Grouping exposing (Position(..), buildGroups, inSameRange)

{-|

@docs Position, buildGroups, inSameRange

Grouping is a helper to group highlightables with the same uiState/marked.
Start and end of a group is marked with a special tag and groups containing only one highlightable are tagged as standalone.

-}

import List.Extra
import Nri.Ui.Highlightable.V1 exposing (Highlightable, UIState(..))
import Set exposing (Set)


{-| Position tags highlightable with the position of it in a group.
-}
type Position
    = Start
    | End
    | Standalone
    | Middle


{-| Groups highlightables with the same state together and taggs them with their position.
-}
buildGroups : List (Highlightable marker) -> List ( Position, Highlightable marker )
buildGroups =
    List.Extra.groupWhile groupHighlightables
        >> List.concatMap markGroup


groupHighlightables : Highlightable marker -> Highlightable marker -> Bool
groupHighlightables x y =
    ((x.uiState == y.uiState)
        && (x.marked == Nothing)
        && (y.marked == Nothing)
    )
        || (x.marked == y.marked && x.marked /= Nothing)
        || (x.marked /= Nothing && y.uiState == Hinted)
        || (y.marked /= Nothing && x.uiState == Hinted)


markGroup : ( Highlightable marker, List (Highlightable marker) ) -> List ( Position, Highlightable marker )
markGroup highlightables =
    let
        buildPositions remaining acc =
            case remaining of
                [] ->
                    List.reverse acc

                [ last ] ->
                    buildPositions [] (( End, last ) :: acc)

                elem :: rest ->
                    buildPositions rest (( Middle, elem ) :: acc)
    in
    case highlightables of
        ( elem, [] ) ->
            [ ( Standalone, elem ) ]

        ( first, rest ) ->
            buildPositions rest [ ( Start, first ) ]


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
