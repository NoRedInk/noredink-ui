module Nri.Ui.Mark.V5 exposing
    ( Mark
    , view, viewWithInlineTags, viewWithBalloonTags
    , viewWithOverlaps
    )

{-|


### Changes from V4

    - Remove `LabelState` from API, the consumer will manage animations through `labelCss`

@docs Mark
@docs view, viewWithInlineTags, viewWithBalloonTags
@docs viewWithOverlaps
@docs LabelState

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Style exposing (invisibleStyle)
import Content
import Css exposing (Color, Style)
import Css.Animations
import Css.Global
import Css.Media
import Html.Styled as Html exposing (Html, span)
import Html.Styled.Attributes exposing (class, css)
import Markdown.Block
import Markdown.Inline
import Nri.Ui.Balloon.V2 as Balloon
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.Html.V3 exposing (viewJust)
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Sort exposing (Sorter)
import Sort.Set as Set exposing (Set)
import String.Extra


{-| -}
type alias Mark =
    { name : Maybe String
    , startStyles : List Css.Style
    , styles : List Css.Style
    , endStyles : List Css.Style
    }


{-| When elements are marked, wrap them in a single `mark` html node.
-}
view :
    (content -> List Style -> Html msg)
    -> List ( content, Maybe Mark )
    -> List (Html msg)
view =
    view_ HiddenTags


{-| When elements are marked, add ::before and ::after elements indicating the start and end of the highlight.

(We can't use a `mark` HTML element here because of the tree structure of HTML)

-}
viewWithOverlaps :
    (content -> List Style -> Html msg)
    -> List ( content, List Mark )
    -> List (Html msg)
viewWithOverlaps viewSegment segments =
    segments
        |> List.foldr
            (\( content, marks ) ( lastMarks, acc ) ->
                ( Set.fromList maybeStringSorter (List.map .name marks)
                , { content = content
                  , marks = marks
                  , after = ignoreRepeats lastMarks marks
                  }
                    :: acc
                )
            )
            ( Set.empty maybeStringSorter, [] )
        |> Tuple.second
        |> List.foldl
            (\{ content, marks, after } ( lastMarks, acc ) ->
                let
                    segment startingStyles =
                        viewSegment content
                            [ tagBeforeContent before
                            , tagAfterContent after
                            , Css.batch startingStyles
                            , Css.batch (List.concatMap .styles marks)
                            , Css.batch (List.concatMap .endStyles after)
                            ]

                    startStyles =
                        List.concatMap (\markedWith -> markedWith.styles ++ markedWith.startStyles) before

                    before =
                        ignoreRepeats lastMarks marks
                in
                ( Set.fromList maybeStringSorter (List.map .name marks)
                , acc
                    ++ (case List.filterMap .name before of
                            [] ->
                                [ segment startStyles ]

                            names ->
                                [ span [ css startStyles ]
                                    [ viewInlineTag
                                        [ Css.display Css.none
                                        , MediaQuery.highContrastMode
                                            [ Css.property "forced-color-adjust" "none"
                                            , Css.display Css.inline |> Css.important
                                            , Css.property "color" "initial" |> Css.important
                                            ]
                                        ]
                                        (String.Extra.toSentenceOxford names)
                                    ]
                                , segment []
                                ]
                       )
                )
            )
            ( Set.empty maybeStringSorter, [] )
        |> Tuple.second


ignoreRepeats : Set (Maybe String) -> List Mark -> List Mark
ignoreRepeats lastMarks list =
    List.filter (\x -> not (Set.memberOf lastMarks x.name)) list


maybeStringSorter : Sorter (Maybe String)
maybeStringSorter =
    Sort.by (Maybe.withDefault "") Sort.alphabetical


{-| When elements are marked, wrap them in a single `mark` html node.

Show the label for the mark, if present, in-line with the emphasized content.

-}
viewWithInlineTags :
    (content -> List Style -> Html msg)
    -> List ( content, Maybe Mark )
    -> List (Html msg)
viewWithInlineTags =
    view_ InlineTags


type alias LabelPosition =
    { totalHeight : Float
    , arrowHeight : Float
    , zIndex : Int
    , xOffset : Float
    }


{-| When elements are marked, wrap them in a single `mark` html node.

Show the label for the mark, if present, in a balloon centered above the emphasized content.

-}
viewWithBalloonTags :
    { renderSegment : c -> List Style -> Html msg
    , backgroundColor : Color
    , maybeMarker : Maybe Mark
    , labelPosition : Maybe LabelPosition
    , labelCss : List Css.Style
    , labelId : Maybe String
    , labelContentId : Maybe String
    }
    -> List c
    -> List (Html msg)
viewWithBalloonTags ({ renderSegment, maybeMarker } as config) contents =
    case maybeMarker of
        Just markedWith ->
            let
                lastIndex =
                    List.length contents - 1
            in
            [ viewMarkedByBalloon config
                markedWith
                (List.indexedMap
                    (\index content -> renderSegment content (markedWithBalloonStyles markedWith lastIndex index))
                    contents
                )
            ]

        Nothing ->
            List.map (\content -> renderSegment content []) contents


markedWithBalloonStyles : Mark -> Int -> Int -> List Css.Style
markedWithBalloonStyles marked lastIndex index =
    List.concat
        [ if index == 0 then
            -- if we're on the first highlighted element, we add
            -- a `before` content saying what kind of highlight we're starting
            tagBeforeContent [ marked ] :: marked.startStyles

          else
            []
        , marked.styles
        , if index == lastIndex then
            Css.after
                [ cssContent
                    ("end "
                        ++ (Maybe.map (\name -> stripMarkdownSyntax name) marked.name
                                |> Maybe.withDefault "highlight"
                           )
                    )
                , invisibleStyle
                ]
                :: marked.endStyles

          else
            []
        , -- display:inline-block is necessary for the balloon-spacer to actually
          -- take up vertical space.
          [ Css.display Css.inlineBlock
          , Css.lineHeight (Css.num 1.2)
          , Css.margin2 (Css.px 2) Css.zero
          ]
        ]


type TagStyle
    = HiddenTags
    | InlineTags


{-| When elements are marked, wrap them in a single `mark` html node.

Show the label for the mark, if present, in-line with the emphasized content when `showTagsInline` is True.

-}
view_ :
    TagStyle
    -> (content -> List Style -> Html msg)
    -> List ( content, Maybe Mark )
    -> List (Html msg)
view_ tagStyle viewSegment highlightables =
    case highlightables of
        [] ->
            []

        ( _, marked ) :: _ ->
            let
                segments =
                    List.indexedMap
                        (\index ( content, mark ) -> viewSegment content (markStyles tagStyle index mark))
                        highlightables
            in
            case marked of
                Just markedWith ->
                    [ viewMarked tagStyle markedWith segments ]

                Nothing ->
                    segments


viewMarkedByBalloon :
    { config
        | backgroundColor : Color
        , labelPosition : Maybe LabelPosition
        , labelCss : List Css.Style
        , labelId : Maybe String
        , labelContentId : Maybe String
    }
    -> Mark
    -> List (Html msg)
    -> Html msg
viewMarkedByBalloon config markedWith segments =
    Html.mark
        [ markedWith.name
            |> Maybe.map (\name -> Aria.roleDescription (stripMarkdownSyntax name ++ " highlight"))
            |> Maybe.withDefault AttributesExtra.none
        , css [ Css.backgroundColor Css.transparent, Css.position Css.relative ]
        ]
        -- the balloon should never end up on a line by itself, so we put it in the DOM
        -- after the first segment.
        (case segments of
            first :: remainder ->
                first
                    :: viewJust (viewBalloon config) markedWith.name
                    :: viewJust (\_ -> viewBalloonSpacer config) markedWith.name
                    :: remainder

            [] ->
                []
        )


viewMarked : TagStyle -> Mark -> List (Html msg) -> Html msg
viewMarked tagStyle markedWith segments =
    Html.mark
        [ markedWith.name
            |> Maybe.map (\name -> Aria.roleDescription (stripMarkdownSyntax name ++ " highlight"))
            |> Maybe.withDefault AttributesExtra.none
        , css
            [ Css.backgroundColor Css.transparent
            , Css.Global.children
                [ Css.Global.selector ":last-child"
                    (Css.after
                        [ Css.property "content" ("\" end " ++ (Maybe.map (\name -> stripMarkdownSyntax name) markedWith.name |> Maybe.withDefault "highlight") ++ " \"")
                        , invisibleStyle
                        ]
                        :: markedWith.endStyles
                    )
                ]
            ]
        ]
        (case markedWith.name of
            Just name ->
                viewStartHighlightTag tagStyle markedWith name :: segments

            Nothing ->
                segments
        )


viewStartHighlightTag : TagStyle -> Mark -> String -> Html msg
viewStartHighlightTag tagStyle marked name =
    span
        [ css
            (marked.styles
                ++ (-- start styles are attached to the first segment, unless there's
                    -- an inline tag to show, in which case we'll attach the styles to the start tag.
                    case tagStyle of
                        HiddenTags ->
                            if marked.name == Nothing then
                                []

                            else
                                [ MediaQuery.highContrastMode marked.startStyles ]

                        InlineTags ->
                            if marked.name == Nothing then
                                []

                            else
                                marked.startStyles
                   )
            )
        , class "highlighter-inline-tag highlighter-inline-tag-highlighted"
        ]
        [ viewTag tagStyle name ]


markStyles : TagStyle -> Int -> Maybe Mark -> List Css.Style
markStyles tagStyle index marked =
    case ( index == 0, marked ) of
        ( True, Just markedWith ) ->
            -- if we're on the first highlighted element, we add
            -- a `before` content saying what kind of highlight we're starting
            tagBeforeContent [ markedWith ]
                :: markedWith.styles
                ++ -- if we're on the first element, and the mark has a name,
                   -- there's an inline tag that we might need to show.
                   -- if we're not showing a visual tag, we can attach the start styles to the first segment
                   (case tagStyle of
                        HiddenTags ->
                            if markedWith.name == Nothing then
                                markedWith.startStyles

                            else
                                [ MediaQuery.notHighContrastMode
                                    (markedWith.startStyles
                                        ++ [ -- override for the left border that's typically
                                             -- added in Nri.Ui.HighlighterTool
                                             MediaQuery.highContrastMode
                                                [ Css.important (Css.borderLeftWidth Css.zero)
                                                ]
                                           ]
                                    )
                                ]

                        InlineTags ->
                            if markedWith.name == Nothing then
                                markedWith.startStyles

                            else
                                []
                   )

        _ ->
            Maybe.map .styles marked
                |> Maybe.withDefault []


tagBeforeContent : List Mark -> Css.Style
tagBeforeContent marks =
    if List.isEmpty marks then
        Css.batch []

    else
        Css.before
            [ cssContent (highlightDescription "start" marks)
            , invisibleStyle
            ]


tagAfterContent : List Mark -> Css.Style
tagAfterContent marks =
    if List.isEmpty marks then
        Css.batch []

    else
        Css.after
            [ cssContent (highlightDescription "end" marks)
            , invisibleStyle
            ]


highlightDescription : String -> List Mark -> String
highlightDescription prefix marks =
    let
        names =
            String.Extra.toSentenceOxford (List.filterMap (.name >> Maybe.map stripMarkdownSyntax) marks)
    in
    if names == "" then
        prefix ++ " highlight"

    else if List.length marks == 1 then
        prefix ++ " " ++ names ++ " highlight"

    else
        prefix ++ " " ++ names ++ " highlights"


cssContent : String -> Css.Style
cssContent content =
    Css.property "content" ("\" " ++ content ++ " \"")


viewTag : TagStyle -> String -> Html msg
viewTag tagStyle =
    case tagStyle of
        InlineTags ->
            viewInlineTag
                [ MediaQuery.highContrastMode
                    [ Css.property "forced-color-adjust" "none"
                    , Css.property "color" "initial" |> Css.important
                    ]
                ]

        HiddenTags ->
            viewInlineTag
                [ Css.display Css.none
                , MediaQuery.highContrastMode
                    [ Css.property "forced-color-adjust" "none"
                    , Css.display Css.inline |> Css.important
                    , Css.property "color" "initial" |> Css.important
                    ]
                ]


viewInlineTag : List Css.Style -> String -> Html msg
viewInlineTag customizations name =
    span
        [ css
            [ Fonts.baseFont
            , Css.backgroundColor Colors.white
            , Css.color Colors.navy
            , Css.padding2 (Css.px 2) (Css.px 4)
            , Css.borderRadius (Css.px 3)
            , Css.margin2 Css.zero (Css.px 5)
            , Css.boxShadow5 Css.zero (Css.px 1) (Css.px 1) Css.zero Colors.gray75
            , Css.batch customizations
            ]
        , -- we use the :before element to convey details about the start of the
          -- highlighter to screenreaders, so the visual label is redundant
          Aria.hidden True
        ]
        (Content.markdownInline name)


viewBalloon :
    { config
        | backgroundColor : Color
        , labelPosition : Maybe LabelPosition
        , labelCss : List Css.Style
        , labelId : Maybe String
        , labelContentId : Maybe String
    }
    -> String
    -> Html msg
viewBalloon config label =
    Balloon.view
        [ Balloon.onTop
        , Balloon.containerCss
            [ Css.position Css.absolute
            , Css.top (Css.px -6)
            , -- using position, 50% is wrt the parent container
              -- using transform & translate, 50% is wrt to the element itself
              -- combining these two properties, we can center the tag against the parent container
              -- for any arbitrary width element
              Css.left (Css.pct 50)
            , Css.property "transform" "translateX(-50%) translateY(-100%)"
            , case Maybe.map .zIndex config.labelPosition of
                Just zIndex ->
                    Css.zIndex (Css.int zIndex)

                Nothing ->
                    Css.batch []
            , Css.batch config.labelCss
            ]
        , Balloon.css
            [ Css.padding3 Css.zero (Css.px 6) (Css.px 1)
            , Css.property "box-shadow" "none"
            , Css.property "width" "max-content"
            , -- this min-width prevents the arrow from gapping at the corners where the balloon is rounded
              -- if the border radius of the balloon changes, this value might need to change as well
              Css.minWidth (Css.px 30)
            , Css.maxWidth (Css.px 150)
            , Css.textAlign Css.center
            , Css.whiteSpace Css.normal
            , Css.property "word-break" "break-word"
            , Css.batch <|
                case config.labelPosition of
                    Just { xOffset } ->
                        if xOffset /= 0 then
                            [ Css.property "transform" ("translateX(" ++ String.fromFloat xOffset ++ "px)") ]

                        else
                            []

                    Nothing ->
                        []
            ]
        , Balloon.custom
            [ -- we use the :before element to convey details about the start of the
              -- highlighter to screenreaders, so the visual label is redundant
              Aria.hidden True
            ]
        , Balloon.customTheme
            { backgroundColor = config.backgroundColor
            , color = Nri.Ui.Colors.Extra.highContrastColor config.backgroundColor
            }
        , Balloon.highContrastModeTheme
            { backgroundColor = "Mark"
            , color = "MarkText"
            }
        , case config.labelContentId of
            Just id_ ->
                Balloon.contentId id_

            Nothing ->
                Balloon.css []
        , Balloon.markdown label
        , case config.labelId of
            Just id_ ->
                Balloon.id id_

            Nothing ->
                Balloon.css []
        , case config.labelPosition of
            Just { arrowHeight } ->
                Balloon.arrowHeight arrowHeight

            Nothing ->
                Balloon.css []
        ]


viewBalloonSpacer : { config | labelPosition : Maybe { b | totalHeight : Float } } -> Html msg
viewBalloonSpacer config =
    span
        [ css
            [ Css.display Css.inlineBlock
            , Css.Media.withMediaQuery
                [ "(prefers-reduced-motion: no-preference)" ]
                [ Css.property "transition" "padding 0.8s ease" ]
            , config.labelPosition
                |> Maybe.map
                    (\{ totalHeight } ->
                        Css.paddingTop (Css.px totalHeight)
                    )
                |> Maybe.withDefault (Css.batch [])
            ]
        , AttributesExtra.nriDescription "balloon-spacer"
        ]
        [ -- this element should never be displayed to anyone.
          -- It's only present to take up vertical space in order to align the label.
          span
            [ css
                [ Css.visibility Css.hidden
                , Css.overflowX Css.hidden
                , Css.width (Css.px 0)
                , Css.display Css.inlineBlock
                , Css.Media.withMediaQuery
                    [ "(prefers-reduced-motion: no-preference)" ]
                    [ Css.property "transition" "line-height 0.8s ease"
                    , case config.labelPosition of
                        Nothing ->
                            Css.lineHeight (Css.num 0)

                        Just _ ->
                            Css.lineHeight Css.unset
                    ]
                ]
            ]
            [ Html.text "()" ]
        ]


stripMarkdownSyntax : String -> String
stripMarkdownSyntax markdown =
    case Markdown.Block.parse Nothing markdown of
        [ Markdown.Block.Paragraph _ inlines ] ->
            Markdown.Inline.extractText inlines

        _ ->
            markdown
