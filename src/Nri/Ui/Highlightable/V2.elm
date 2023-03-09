module Nri.Ui.Highlightable.V2 exposing
    ( Highlightable, Type(..), UIState(..), Attribute(..)
    , init, initFragments
    , fromMarkdown
    , blur, clearHint, hint, hover
    , set
    , attributeSorter
    , asFragmentTuples, usedMarkers, text
    )

{-| A Highlightable represents a span of text, typically a word, and its state.

Highlightable is the unit by which text-wrapping happens. Depending on how the
Highlighter is initialized, it's very possible for a Highlightable to consist of
just a single whitespace.


## Changes from V1

  - move asFragmentTuples, usedMarkers, and text to the Highlightable module
  - remove initFragment, splitHighlightableOnWords, splitWords
  - remove toggle, which is not used
  - rename groupIndex -> index
  - ensure that fromMarkdown indexes the fragments correctly
  - support multiple kinds of mark


## Types

@docs Highlightable, Type, UIState, Attribute


## Initializers

@docs init, initFragments
@docs fromMarkdown


## UIState and marker

@docs blur, clearHint, hint, hover
@docs set


## Attribute related

@docs attributeSorter


## Getters

@docs asFragmentTuples, usedMarkers, text

-}

import List.Extra
import Markdown.Block
import Markdown.Inline
import Maybe.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.HighlighterTool.V1 as Tool
import Regex exposing (Regex)
import Sort exposing (Sorter)
import Sort.Set
import String.Extra


{-| A Highlightable comes in two flavors:

  - **Highlightable** | Interactive piece of text.

  - **Static** | Non-interactive piece of text. Gets highlighted grudgingly if the
    UI is going to get unintuitive otherwise. i.e. does not get highlighted
    when hovered etc.

    Data for a Highlightable:

  - **text**: String to display.

  - **uiState**: How the user is currently interacting with the Highlightable.

  - **customAttributes**: User-supplied attributes that do not change once a Highlightable is initialized.

  - **marked**: Current highlights, if any.

  - **index**: Index that identifies the fragment this Highlightable belongs to. Must be unique in the list of highlightables.

-}
type alias Highlightable marker =
    { text : String
    , uiState : UIState
    , customAttributes : List Attribute
    , marked : List (Tool.MarkerModel marker)
    , index : Int
    , type_ : Type
    }


{-| -}
type Type
    = Interactive
    | Static


{-| Custom attributes that do not change after initialization.

  - `Class`: add a class to the highlight
  - `Data`: add a `data-X` to the highlight (you don't need to prepend
    `data-` to this name.)

-}
type Attribute
    = Class String
    | Data String String


{-| A Sorter for Attributes
-}
attributeSorter : Sorter Attribute
attributeSorter =
    Sort.by
        (\elem ->
            case elem of
                Class class ->
                    "class-" ++ class

                Data key value ->
                    "data-" ++ key ++ "-" ++ value
        )
        Sort.alphabetical


{-| UIState connects user-behavior to the highlightable.

State transitions:

  - None → (mouse over) → Hovered
  - None → (click & drag over highlightables) → Hinted
  - Hovered → (mouse out) → None
  - Hovered → (mouse down) → Hinted
  - Hinted → (mouse out) → None
  - Hinted → (mouse up while on a different Highlightable) → None
  - Hinted → (mouse up while on me) → Hovered

-}
type UIState
    = Hovered
    | Hinted
    | None


{-| -}
init : Type -> List (Tool.MarkerModel marker) -> Int -> ( List Attribute, String ) -> Highlightable marker
init type_ marked index ( attributes, text_ ) =
    { text = text_
    , uiState = None
    , customAttributes = attributes
    , marked = marked
    , index = index
    , type_ = type_
    }


whitespace : Regex
whitespace =
    Regex.fromString "\\s+"
        |> Maybe.withDefault Regex.never


{-| Initialize highlightables from a string.

Note that we're transforming all whitespace to spaces, so newlines are not preserved
as me move to and from fragments. Spaces will be treated as static elements. Words will be interactive.

-}
initFragments : List (Tool.MarkerModel marker) -> String -> List (Highlightable marker)
initFragments marked text_ =
    let
        spaceOrInit index maybeWord =
            case maybeWord of
                Just word ->
                    init Interactive marked index ( [], word )

                Nothing ->
                    init Static [] index ( [], " " )
    in
    Regex.split whitespace text_
        |> List.map Just
        |> List.intersperse Nothing
        |> List.indexedMap spaceOrInit


{-| How do we know which elements should be marked, if all we have is a markdown string?

We do some funky parsing to interpret empty anchor tags and tagged spans as highlighted!

    fromMarkdown "for example, [this phrase]() will show as highlighted"

will result in a list of highlightables where "this phrase" is marked with the default marker.

-}
fromMarkdown : String -> List (Highlightable ())
fromMarkdown markdownString =
    let
        static maybeMark mapStrings c =
            init Static (Maybe.Extra.toList maybeMark) -1 ( [], mapStrings c )

        defaultMark =
            Tool.buildMarker
                { highlightColor = Colors.highlightYellow
                , hoverColor = Colors.highlightYellow
                , hoverHighlightColor = Colors.highlightYellow
                , kind = ()
                , name = Nothing
                }

        highlightableFromInline : Maybe (Tool.MarkerModel ()) -> (String -> String) -> Markdown.Inline.Inline i -> List (Highlightable ())
        highlightableFromInline maybeMark mapStrings inline =
            case inline of
                Markdown.Inline.Text text_ ->
                    [ static maybeMark mapStrings text_ ]

                Markdown.Inline.HardLineBreak ->
                    [ static maybeMark mapStrings "\n" ]

                Markdown.Inline.CodeInline text_ ->
                    [ static maybeMark mapStrings text_ ]

                Markdown.Inline.Link "" maybeTitle inlines ->
                    -- empty links should be interpreted as content that's supposed to be highlighted!
                    List.concatMap (highlightableFromInline (Just defaultMark) mapStrings) inlines

                Markdown.Inline.Link url maybeTitle inlines ->
                    let
                        lastIndex =
                            List.length inlines - 1

                        addLinkOpening i str =
                            if i == 0 then
                                "[" ++ str

                            else
                                str

                        addLinkClosing i str =
                            if i == lastIndex then
                                str ++ "](" ++ url ++ ")"

                            else
                                str
                    in
                    List.indexedMap
                        (\i ->
                            highlightableFromInline maybeMark
                                (mapStrings >> addLinkOpening i >> addLinkClosing i)
                        )
                        inlines
                        |> List.concat

                Markdown.Inline.Image _ _ inlines ->
                    List.concatMap (highlightableFromInline maybeMark mapStrings) inlines

                Markdown.Inline.HtmlInline _ _ inlines ->
                    List.concatMap (highlightableFromInline maybeMark mapStrings) inlines

                Markdown.Inline.Emphasis level inlines ->
                    let
                        marker =
                            String.repeat level "*"

                        addMarkers str =
                            marker ++ str ++ marker
                    in
                    List.concatMap
                        (highlightableFromInline maybeMark (mapStrings >> addMarkers))
                        inlines

                Markdown.Inline.Custom _ inlines ->
                    List.concatMap (highlightableFromInline maybeMark mapStrings) inlines

        highlightableFromBlock : Markdown.Block.Block b i -> List (Highlightable ())
        highlightableFromBlock block =
            case block of
                Markdown.Block.BlankLine text_ ->
                    [ static Nothing identity text_ ]

                Markdown.Block.ThematicBreak ->
                    []

                Markdown.Block.Heading _ _ inlines ->
                    List.concatMap (highlightableFromInline Nothing identity) inlines

                Markdown.Block.CodeBlock _ text_ ->
                    [ static Nothing identity text_ ]

                Markdown.Block.Paragraph _ inlines ->
                    List.concatMap (highlightableFromInline Nothing identity) inlines

                Markdown.Block.BlockQuote blocks ->
                    List.concatMap highlightableFromBlock blocks

                Markdown.Block.List _ listOfBlocks ->
                    List.concatMap (List.concatMap highlightableFromBlock) listOfBlocks

                Markdown.Block.PlainInlines inlines ->
                    List.concatMap (highlightableFromInline Nothing identity) inlines

                Markdown.Block.Custom _ blocks ->
                    List.concatMap highlightableFromBlock blocks
    in
    if String.isEmpty markdownString then
        []

    else
        Markdown.Block.parse Nothing markdownString
            |> List.concatMap highlightableFromBlock
            |> List.foldr
                -- ensure that adjacent highlights are in a single mark element
                (\segment ( lastInteractiveHighlight, acc ) ->
                    ( segment.marked
                    , case acc of
                        last :: remainder ->
                            if List.head segment.marked == List.head last.marked then
                                { segment | text = segment.text ++ last.text }
                                    :: remainder

                            else
                                segment :: acc

                        _ ->
                            segment :: acc
                    )
                )
                ( [], [] )
            |> Tuple.second
            |> List.indexedMap (\i highlightable -> { highlightable | index = i })


{-| -}
hover : Highlightable marker -> Highlightable marker
hover highlightable =
    case highlightable.uiState of
        Hinted ->
            highlightable

        _ ->
            { highlightable | uiState = Hovered }


{-| -}
blur : Highlightable marker -> Highlightable marker
blur highlightable =
    case highlightable.uiState of
        Hinted ->
            highlightable

        _ ->
            { highlightable | uiState = None }


{-| -}
hint : Highlightable marker -> Highlightable marker
hint highlightable =
    { highlightable | uiState = Hinted }


{-| -}
clearHint : Highlightable marker -> Highlightable marker
clearHint highlightable =
    { highlightable | uiState = None }


{-| -}
set : Maybe (Tool.MarkerModel marker) -> Highlightable marker -> Highlightable marker
set marked highlightable =
    { highlightable | marked = Maybe.Extra.toList marked }


{-| Get unique markers that have been used.
-}
usedMarkers : Sorter marker -> List (Highlightable marker) -> Sort.Set.Set marker
usedMarkers sorter highlightables =
    highlightables
        |> List.concatMap
            (\highlightable ->
                if String.Extra.isBlank highlightable.text then
                    []

                else
                    List.map .kind highlightable.marked
            )
        |> Sort.Set.fromList sorter


{-| Get a list of fragment texts and whether or not they are marked.
Useful for encoding answers.
-}
asFragmentTuples : List (Highlightable marker) -> List ( List marker, String )
asFragmentTuples highlightables =
    highlightables
        |> List.Extra.groupWhile (\a b -> a.index == b.index)
        |> List.map
            (\( first, rest ) ->
                ( List.map .kind first.marked
                , text (first :: rest)
                )
            )


{-| Fetch the text from a series of highlightables.
-}
text : List (Highlightable marker) -> String
text highlightables =
    List.map .text highlightables
        |> String.concat
