module Highlighter.Internal exposing
    ( blurAt, hoverAt, hintBetween, saveHinted, toggleHinted, removeHinted
    , removeHighlights
    )

{-| Utility functions for dealing with a list of Highlightables.


# UIState related

@docs blurAt, hoverAt, hintBetween, saveHinted, toggleHinted, removeHinted


# Marker related

@docs removeHighlights

-}

import Highlighter.Grouping as Grouping
import List.Extra
import Nri.Ui.Highlightable.V1 as Highlightable exposing (Highlightable)
import Nri.Ui.HighlighterTool.V1 as Tool
import Set
import String.Extra


{-| -}
blurAt : Int -> List (Highlightable marker) -> List (Highlightable marker)
blurAt groupIndex =
    updateHighlightableWhen
        (ifSameGroupIndex groupIndex)
        { isTrue = Highlightable.blur
        , isFalse = identity
        }


{-| -}
hoverAt : Int -> List (Highlightable marker) -> List (Highlightable marker)
hoverAt groupIndex =
    updateHighlightableWhen
        (ifSameGroupIndex groupIndex)
        { isTrue = Highlightable.hover
        , isFalse = identity
        }


{-| -}
hintBetween : Int -> Int -> List (Highlightable marker) -> List (Highlightable marker)
hintBetween beginning end =
    updateHighlightableWhen
        (ifGroupIndexIsWithin beginning end)
        { isTrue = Highlightable.hint
        , isFalse = Highlightable.clearHint
        }


{-| -}
saveHinted : Tool.MarkerModel marker -> List (Highlightable marker) -> List (Highlightable marker)
saveHinted marker highlightables =
    highlightables
        |> updateHighlightableWhen
            (\{ uiState } -> uiState == Highlightable.Hinted)
            { isTrue = Highlightable.clearHint << Highlightable.set (Just marker)
            , isFalse = Highlightable.clearHint
            }
        |> trimHighlightableGroups


{-| -}
toggleHinted : Int -> Tool.MarkerModel marker -> List (Highlightable marker) -> List (Highlightable marker)
toggleHinted groupIndex marker highlightables =
    let
        hintedRange =
            Grouping.inSameRange groupIndex highlightables

        inClickedRange highlightable =
            hintedRange
                |> Set.member highlightable.groupIndex

        toggle highlightable =
            if inClickedRange highlightable && Just marker == highlightable.marked then
                Highlightable.set Nothing highlightable

            else if ifSameGroupIndex groupIndex highlightable then
                Highlightable.set (Just marker) highlightable

            else
                highlightable
    in
    List.map (toggle >> Highlightable.clearHint) highlightables
        |> trimHighlightableGroups


{-| -}
removeHinted : List (Highlightable marker) -> List (Highlightable marker)
removeHinted =
    updateHighlightableWhen
        (\{ uiState } -> uiState == Highlightable.Hinted)
        { isTrue = Highlightable.set Nothing >> Highlightable.clearHint
        , isFalse = Highlightable.clearHint
        }
        >> trimHighlightableGroups


{-| Remove all highlights.
-}
removeHighlights : List (Highlightable marker) -> List (Highlightable marker)
removeHighlights =
    List.map (Highlightable.set Nothing)



-- UPDATE HELPERS


updateHighlightableWhen :
    (Highlightable marker -> Bool)
    ->
        { isTrue : Highlightable marker -> Highlightable marker
        , isFalse : Highlightable marker -> Highlightable marker
        }
    -> List (Highlightable marker)
    -> List (Highlightable marker)
updateHighlightableWhen predicate { isTrue, isFalse } =
    List.map <|
        \highlightable ->
            if predicate highlightable then
                isTrue highlightable

            else
                isFalse highlightable


ifSameGroupIndex : Int -> Highlightable marker -> Bool
ifSameGroupIndex groupIndex highlightable =
    highlightable.groupIndex == groupIndex


ifGroupIndexIsWithin : Int -> Int -> Highlightable marker -> Bool
ifGroupIndexIsWithin from to { groupIndex } =
    if from < to then
        from <= groupIndex && groupIndex <= to

    else
        to <= groupIndex && groupIndex <= from


{-| This removes highlighting of leading and trailing whitespace from all groups.
It is meant to be called as a clean up after the highlightings have been changed.
-}
trimHighlightableGroups : List (Highlightable marker) -> List (Highlightable marker)
trimHighlightableGroups highlightables =
    highlightables
        |> List.Extra.groupWhile (\a b -> a.marked == b.marked)
        |> List.map (trimHighlightedWhitespace >> killOnlyStaticHighlights)
        |> List.concat


trimHighlightedWhitespace : ( Highlightable marker, List (Highlightable marker) ) -> List (Highlightable marker)
trimHighlightedWhitespace ( first, highlightables ) =
    first
        :: highlightables
        |> trimHighlightedLeadingWhitespace
        |> List.reverse
        |> trimHighlightedLeadingWhitespace
        |> List.reverse


trimHighlightedLeadingWhitespace : List (Highlightable marker) -> List (Highlightable marker)
trimHighlightedLeadingWhitespace highlightables =
    highlightables
        |> List.Extra.break (not << String.Extra.isBlank << .text)
        |> Tuple.mapFirst removeHighlights
        |> (\( statics, rest ) -> statics ++ rest)


killOnlyStaticHighlights : List (Highlightable marker) -> List (Highlightable marker)
killOnlyStaticHighlights highlightables =
    if List.all (\h -> h.type_ == Highlightable.Static) highlightables then
        removeHighlights highlightables

    else
        highlightables
