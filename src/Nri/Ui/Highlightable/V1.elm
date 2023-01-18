module Nri.Ui.Highlightable.V1 exposing
    ( Highlightable, Type(..), UIState(..), Attribute(..)
    , init, initFragment, initFragments
    , fromMarkdown
    , splitHighlightableOnWords, splitWords
    , blur, clearHint, hint, hover
    , set, toggle
    , attributeSorter
    )

{-| The next version of Highlightable should remove `groupIndex.`

A Highlightable represents a span of text, typically a word, and its state.

Highlightable is the unit by which text-wrapping happens. Depending on how the
Highlighter is initialized, it's very possible for a Highlightable to consist of
just a single whitespace.


# Types

@docs Highlightable, Type, UIState, Attribute


# Initializers

@docs init, initFragment, initFragments
@docs fromMarkdown


# Transformations

@docs splitHighlightableOnWords, splitWords


# UIState related

@docs blur, clearHint, hint, hover


# Marker related

@docs set, toggle


# Attribute related

@docs attributeSorter

-}

import Markdown
import Markdown.Block
import Markdown.Inline
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.HighlighterTool.V1 as Tool
import Regex exposing (Regex)
import Sort exposing (Sorter)


{-| A Highlightable comes in two flavors:

  - **Highlightable** | Interactive piece of text.

  - **Static** | Non-interactive piece of text. Gets highlighted grudgingly if the
    UI is going to get unintuitive otherwise. i.e. does not get highlighted
    when hovered etc.

    Data for a Highlightable:

  - **text**: String to display.

  - **uiState**: How the user is currently interacting with the Highlightable.

  - **customAttributes**: User-supplied attributes that do not change once a Highlightable is initialized.

  - **marked**: Current highlight.

  - **groupIndex**: Index that identifies the fragment this Highlightable belongs to.

-}
type alias Highlightable marker =
    { text : String
    , uiState : UIState
    , customAttributes : List Attribute
    , marked : Maybe (Tool.MarkerModel marker)
    , groupIndex : Int
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
init : Type -> Maybe (Tool.MarkerModel marker) -> Int -> ( List Attribute, String ) -> Highlightable marker
init type_ marked index ( attributes, text_ ) =
    { text = text_
    , uiState = None
    , customAttributes = attributes
    , marked = marked
    , groupIndex = index
    , type_ = type_
    }


{-| Split a multi-word string into multiple single-word highlights.
This is to deal with the highlighter not doing line breaks within a highlight group,
which can look funny if a highlight contains longer text.
-}
initFragment : Maybe (Tool.MarkerModel marker) -> Int -> List ( List Attribute, String ) -> List (Highlightable marker)
initFragment marked index spans =
    let
        splitSpan ( classes, text_ ) =
            text_
                |> splitWords
                |> List.filter (not << String.isEmpty)
                |> List.map (Tuple.pair classes)
    in
    spans
        |> List.concatMap splitSpan
        |> List.map (init Interactive marked index)


whitespace : Regex
whitespace =
    Regex.fromString "\\s+"
        |> Maybe.withDefault Regex.never


{-| Similar to initFragment but each word is treated as a fragment,
instead of treating the whole string as a fragment.

Note that we're transforming all whitespace to spaces, so newlines are not preserved
as me move to and from fragments

-}
initFragments : Maybe (Tool.MarkerModel marker) -> String -> List (Highlightable marker)
initFragments marked text_ =
    let
        spaceOrInit index maybeWord =
            case maybeWord of
                Just word ->
                    init Interactive marked index ( [], word )

                Nothing ->
                    init Static Nothing index ( [], " " )
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
            init Static maybeMark -1 ( [], mapStrings c )

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
                Markdown.Inline.Text text ->
                    [ static maybeMark mapStrings text ]

                Markdown.Inline.HardLineBreak ->
                    [ static maybeMark mapStrings "\n" ]

                Markdown.Inline.CodeInline text ->
                    [ static maybeMark mapStrings text ]

                Markdown.Inline.Link "" maybeTitle inlines ->
                    -- empty links should be interpreted as content that's supposed to be highlighted!
                    List.concatMap (highlightableFromInline (Just defaultMark) mapStrings) inlines

                Markdown.Inline.Link url maybeTitle inlines ->
                    List.concatMap (highlightableFromInline maybeMark mapStrings) inlines

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
                Markdown.Block.BlankLine text ->
                    [ static Nothing identity text ]

                Markdown.Block.ThematicBreak ->
                    []

                Markdown.Block.Heading _ _ inlines ->
                    List.concatMap (highlightableFromInline Nothing identity) inlines

                Markdown.Block.CodeBlock _ text ->
                    [ static Nothing identity text ]

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
    { highlightable | marked = marked }


{-| -}
toggle : Tool.MarkerModel marker -> Highlightable marker -> Highlightable marker
toggle marker_ highlightable =
    { highlightable
        | marked =
            case highlightable.marked of
                Just oldMarker ->
                    if oldMarker /= marker_ then
                        Just marker_

                    else
                        Nothing

                Nothing ->
                    Just marker_
    }


{-| Split a highlightable into one highlightable per word.
-}
splitHighlightableOnWords : Highlightable marker -> List (Highlightable marker)
splitHighlightableOnWords highlightable =
    highlightable.text
        |> splitWords
        |> List.map (\chunk -> { highlightable | text = chunk })


{-| Create list of words intersperse with spaces.
-}
splitWords : String -> List String
splitWords string =
    string
        |> String.split " "
        |> List.intersperse " "
