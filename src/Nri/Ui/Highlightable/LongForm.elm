module Nri.Ui.Highlightable.LongForm exposing
    ( Highlightable, Type(..)
    , initStatic, initInteractive, initFragments
    , fromMarkdown, fromMarkdownInlines
    , set
    , joinAdjacentInteractiveHighlights
    , asFragmentTuples, usedMarkers, text
    )

{-| A Highlightable represents a span of text, typically a word, and its state.

Highlightable is the unit by which text-wrapping happens. Depending on how the
Highlighter is initialized, it's very possible for a Highlightable to consist of
just a single whitespace.


## Patch

  - add new syntax for highlight on markdown parse, which supports custom colors
  - add new initializer for markdown that receives the parsed markdown inlines instead of just the string


## Changes from V2

  - move the uIState out of the Highlightable
  - make the attribute modeling more flexible
  - replace init with initStatic and initInteractive, and remove attributes from the UI since they're seldom used
  - remove UIState: it only makes sense in the context of an interactive highlighter
  - joinAdjacentInteractiveHighlights now takes a sorter
  - initFragments no longer takes a list of markers


## Types

@docs Highlightable, Type


## Initializers

@docs initStatic, initInteractive, initFragments
@docs fromMarkdown, fromMarkdownInlines


## UIState and marker

@docs set
@docs joinAdjacentInteractiveHighlights


## Getters

@docs asFragmentTuples, usedMarkers, text, byId

-}

import List.Extra
import Markdown.Block
import Markdown.Config exposing (defaultOptions, defaultSanitizeOptions)
import Markdown.Inline
import Maybe.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Highlighter.Attribute exposing (Attribute(..))
import Nri.Ui.HighlighterTool.V1 as Tool
import Regex exposing (Regex)
import Sort exposing (Sorter)
import Sort.Set as Set exposing (Set)
import String.Extra


{-| A Highlightable comes in two flavors:

  - **Highlightable** | Interactive piece of text.

  - **Static** | Non-interactive piece of text.

    Data for a Highlightable:

  - **text**: String to display.

  - **customAttributes**: User-supplied, non-interactive HTML attributes.

  - **marked**: Current highlights, if any.

  - **index**: Unique highlightable index.

-}
type alias Highlightable marker =
    { text : String
    , customAttributes : List Attribute
    , marked : List (Tool.MarkerModel marker)
    , index : Int
    , type_ : Type
    , hintedWith : Maybe (Tool.Tool marker)
    , firstOrLastHintedWith : Maybe (Tool.Tool marker)
    , hoveredBy : Maybe (Tool.Tool marker)
    , isFocused : Bool
    , isClicked : Bool
    }


{-| -}
type Type
    = Interactive
    | Static


{-| -}
initStatic : List (Tool.MarkerModel marker) -> Int -> String -> Highlightable marker
initStatic marked index text_ =
    { text = text_
    , customAttributes = []
    , marked = marked
    , index = index
    , type_ = Static
    , hintedWith = Nothing
    , firstOrLastHintedWith = Nothing
    , hoveredBy = Nothing
    , isFocused = False
    , isClicked = False
    }


{-| -}
initInteractive : List (Tool.MarkerModel marker) -> Int -> String -> Highlightable marker
initInteractive marked index text_ =
    { text = text_
    , customAttributes = []
    , marked = marked
    , index = index
    , type_ = Interactive
    , hintedWith = Nothing
    , firstOrLastHintedWith = Nothing
    , hoveredBy = Nothing
    , isFocused = False
    , isClicked = False
    }


whitespace : Regex
whitespace =
    Regex.fromString "\\s+"
        |> Maybe.withDefault Regex.never


{-| Initialize highlightables from a string.

Note that we're transforming all whitespace to spaces, so newlines are not preserved
as me move to and from fragments. Spaces will be treated as static elements. Words will be interactive.

-}
initFragments : String -> List (Highlightable marker)
initFragments text_ =
    let
        spaceOrInit index maybeWord =
            case maybeWord of
                Just word ->
                    initInteractive [] index word

                Nothing ->
                    initStatic [] index " "
    in
    Regex.split whitespace text_
        |> List.map Just
        |> List.intersperse Nothing
        |> List.indexedMap spaceOrInit


{-| Initialize highlightables from a markdown string.
This will get all `nri-highlight` tags into markded elements, you can add a specific color
using the `color` attribute. The default color is yellow.

    fromMarkdown "for example, <nri-highlight>this phrase</nri-highlight> will show as highlighted"

will result in a list of highlightables where "this phrase" is marked with the default marker.

    fromMarkdown "for example, <nri-highlight color=" cyan ">this phrase</nri-highlight> will show as highlighted"

will result in a list of highlightables where "this phrase" is marked with the cyan marker.

The available highlight colors are listed in the [Nri.Colors docs](https://noredink-ui.netlify.app/#/doodad/Colors),
which are the following:

  - `magenta` -> `Colors.highlightMagenta`
  - `brown` -> `Colors.highlightBrown`
  - `purple` -> `Colors.highlightPurple`
  - `blue` -> `Colors.highlightBlue`
  - `yellow` -> `Colors.highlightYellow`
  - `green` -> `Colors.highlightGreen`
  - `cyan` -> `Colors.highlightCyan`

There is also the empty url syntax, which is currently being deprecated:

    fromMarkdown "for example, [this phrase]() will show as highlighted"

will result in a list of highlightables where "this phrase" is marked with the default marker.

-}
fromMarkdown : String -> List (Highlightable ())
fromMarkdown markdownString =
    let
        static maybeMark mapStrings c =
            initStatic (Maybe.Extra.toList maybeMark) -1 (mapStrings c)

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
        let
            parseOptions =
                { defaultOptions
                    | rawHtml =
                        Markdown.Config.Sanitize
                            { allowedHtmlElements = "nri-highlight" :: defaultSanitizeOptions.allowedHtmlElements
                            , allowedHtmlAttributes = "color" :: defaultSanitizeOptions.allowedHtmlElements
                            }
                }
        in
        Markdown.Block.parse (Just parseOptions)
            markdownString
            |> List.concatMap highlightableFromBlock
            |> List.foldr
                -- ensure that adjacent highlights are in a single mark element
                (\segment ( lastInteractiveHighlight, acc ) ->
                    ( segment.marked
                    , case acc of
                        last :: remainder ->
                            -- Since there's only 1 possible mark type here,
                            -- it's safe to assume that the list is either empty or
                            -- of length 1
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


{-| Same as [`fromMarkdown`](#fromMarkdown), but receives a list of inlines parsed markdown instead of a string.

You might want to use this if you are parsing highlightables out of lists or in other block contexts that [`fromMarkdown`](#fromMarkdown) does not support.

-}
fromMarkdownInlines : List (Markdown.Inline.Inline i) -> List (Highlightable ())
fromMarkdownInlines inlines =
    List.concatMap (highlightableFromInline Nothing identity) inlines


highlightableFromInline : Maybe (Tool.MarkerModel ()) -> (String -> String) -> Markdown.Inline.Inline i -> List (Highlightable ())
highlightableFromInline maybeMark mapStrings inline =
    let
        static curMaybeMark curMaybeString c =
            initStatic (Maybe.Extra.toList curMaybeMark) -1 (curMaybeString c)

        markFromColor color =
            Tool.buildMarker
                { highlightColor = color
                , hoverColor = color
                , hoverHighlightColor = color
                , kind = ()
                , name = Nothing
                }

        defaultMark =
            markFromColor Colors.highlightYellow
    in
    case inline of
        Markdown.Inline.Text text_ ->
            [ static maybeMark mapStrings text_ ]

        Markdown.Inline.HardLineBreak ->
            [ static maybeMark mapStrings "\n" ]

        Markdown.Inline.CodeInline text_ ->
            [ static maybeMark mapStrings text_ ]

        Markdown.Inline.Link "" _ inlines ->
            -- empty links should be interpreted as content that's supposed to be highlighted!
            List.concatMap (highlightableFromInline (Just defaultMark) mapStrings) inlines

        Markdown.Inline.Link url _ inlines ->
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

        Markdown.Inline.HtmlInline "nri-highlight" attrs inlines ->
            let
                color =
                    case
                        List.Extra.find (\( attrName, _ ) -> attrName == "color") attrs
                            |> Maybe.andThen Tuple.second
                    of
                        Just "magenta" ->
                            Colors.highlightMagenta

                        Just "brown" ->
                            Colors.highlightBrown

                        Just "purple" ->
                            Colors.highlightPurple

                        Just "blue" ->
                            Colors.highlightBlue

                        Just "yellow" ->
                            Colors.highlightYellow

                        Just "green" ->
                            Colors.highlightGreen

                        Just "cyan" ->
                            Colors.highlightCyan

                        -- Default color
                        Just _ ->
                            Colors.highlightYellow

                        Nothing ->
                            Colors.highlightYellow
            in
            List.concatMap (highlightableFromInline (Just (markFromColor color)) mapStrings) inlines

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


{-| -}
set : Maybe (Tool.MarkerModel marker) -> Highlightable marker -> Highlightable marker
set marked highlightable =
    { highlightable | marked = Maybe.Extra.toList marked }


{-| -}
joinAdjacentInteractiveHighlights : Sorter m -> List (Highlightable m) -> List (Highlightable m)
joinAdjacentInteractiveHighlights sorter highlightables =
    let
        markerSorter =
            Sort.by .kind sorter
    in
    highlightables
        |> List.foldr
            (\segment ( lastInteractiveHighlightMarkers, staticAcc, acc ) ->
                case segment.type_ of
                    Interactive ->
                        let
                            staticMarkers : List (Tool.MarkerModel m)
                            staticMarkers =
                                List.filter (\m -> List.member m segment.marked)
                                    lastInteractiveHighlightMarkers

                            static_ =
                                List.map
                                    (\s ->
                                        if
                                            -- careful here.. we want something like:
                                            -- [] [] True
                                            -- [1] [] False
                                            -- [] [1] False <- just List.all will return True
                                            -- [1] [1] True
                                            (List.length staticMarkers == List.length s.marked)
                                                && List.all (\m -> List.member m s.marked) staticMarkers
                                        then
                                            s

                                        else
                                            { s | marked = staticMarkers }
                                    )
                                    staticAcc
                        in
                        ( segment.marked, [], segment :: static_ ++ acc )

                    Static ->
                        ( lastInteractiveHighlightMarkers, segment :: staticAcc, acc )
            )
            ( [], [], [] )
        |> (\( _, static_, acc ) -> static_ ++ acc)


{-| Get unique markers that have been used. Note: ignores marks on whitespace.
-}
usedMarkers : Sorter marker -> List (Highlightable marker) -> Set marker
usedMarkers sorter highlightables =
    highlightables
        |> List.concatMap
            (\highlightable ->
                if String.Extra.isBlank highlightable.text then
                    []

                else
                    List.map .kind highlightable.marked
            )
        |> Set.fromList sorter


{-| Get a list of fragment texts and whether or not they are marked.
Useful for encoding answers.
-}
asFragmentTuples : List (Highlightable marker) -> List ( List marker, String )
asFragmentTuples highlightables =
    let
        asFragmentTuple highlightable =
            ( List.map .kind highlightable.marked, highlightable.text )
    in
    List.map asFragmentTuple highlightables


{-| Fetch the text from a series of highlightables.
-}
text : List (Highlightable marker) -> String
text highlightables =
    List.map .text highlightables
        |> String.concat
