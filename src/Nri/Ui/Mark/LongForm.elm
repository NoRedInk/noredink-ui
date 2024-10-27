module Nri.Ui.Mark.LongForm exposing
    ( Mark
    , view, viewWithInlineTags, viewWithBalloonTags
    , viewWithOverlaps, overlappingStyles
    , renderStyles
    )

{-|


### Changes from V5

    -  Add `skipTagAnimation` for skipping balloon animations


### Patch changes

    - Factor `overlappingStyles` out of `viewWithOverlaps` to allow consumers finer grained control of rendering mark elements.

@docs Mark
@docs view, viewWithInlineTags, viewWithBalloonTags
@docs viewWithOverlaps, overlappingStyles

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Style exposing (invisibleStyle)
import Content
import Css exposing (Color, Style)
import Css.Global
import Css.Media
import Html as Unstyled
import Html.Attributes
import Html.Styled as Html exposing (Html, span)
import Html.Styled.Attributes exposing (css)
import Markdown.Block
import Markdown.Inline
import Nri.Ui.Balloon.V2 as Balloon
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Highlighter.Attribute exposing (Attribute(..), toHtmlAttribute)
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.Html.V3 exposing (viewJust)
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Regex
import Sort exposing (Sorter)
import Sort.Set as Set
import String.Extra


{-| -}
type alias Mark =
    { name : Maybe String
    , startStyles : List Css.Style
    , styles : List Css.Style
    , endStyles : List Css.Style
    }


renderStyles : Bool -> List Mark -> List Css.Global.Snippet
renderStyles showTagsInline marks =
    let
        tagStyle =
            if showTagsInline then
                InlineTags

            else
                HiddenTags
    in
    styleClassStyle (InlineTagContent tagStyle)
        :: List.concatMap
            (\mark ->
                [ styleClassStyle (MarkedMark tagStyle mark)
                , styleClassStyle (InlineTag tagStyle mark)
                , styleClassStyle (MarkedSegmentFirst mark)
                , styleClassStyle (MarkedSegmentLast mark)
                , styleClassStyle (MarkedSegment mark)
                ]
            )
            marks


{-| When elements are marked, wrap them in a single `mark` html node.
-}
view :
    (content -> List Attribute -> Unstyled.Html msg)
    -> List ( content, Maybe Mark )
    -> List (Unstyled.Html msg)
view viewSegment segments =
    view_ HiddenTags viewSegment segments


{-| When elements are marked, add ::before and ::after elements indicating the start and end of the highlight.

(We can't use a `mark` HTML element here because of the tree structure of HTML)

-}
viewWithOverlaps :
    (content -> List Attribute -> Unstyled.Html msg)
    -> List ( content, List Mark )
    -> List (Unstyled.Html msg)
viewWithOverlaps viewSegment segments =
    overlappingStyles segments
        |> List.concatMap
            (\( content, maybeLabel, styles ) ->
                case maybeLabel of
                    Nothing ->
                        [ viewSegment content styles ]

                    Just label ->
                        [ label, viewSegment content styles ]
            )


{-| Compute the styles required to mark the segments.

You can use this if you require more control over the structure of how marked elements are laid out than `viewWithOverlaps` provides.

The `Maybe (Html msg)` result is a label element that should be rendered before the current segment.

-}
overlappingStyles : List ( content, List Mark ) -> List ( content, Maybe (Unstyled.Html msg), List Attribute )
overlappingStyles segments =
    let
        -- We can't detect the end of a span of marks until after we are past it!
        -- So we need to go back to the last set of styles and add the ended styles there
        updateEndRevStyles patchRevStyles endedMarks =
            case ( patchRevStyles, endedMarks ) of
                ( [], _ ) ->
                    patchRevStyles

                ( _, [] ) ->
                    patchRevStyles

                ( ( prevContent, prevLabel, prevStyles ) :: otherPrevStyles, _ ) ->
                    ( prevContent, prevLabel, prevStyles ++ List.map (MarkedSegmentLast >> styleClassName >> Class) endedMarks ) :: otherPrevStyles

        { priorMarks, revStyles } =
            List.foldl
                (\( segmentContent, segmentMarks ) state ->
                    let
                        currentMarks =
                            Set.fromList markSorter segmentMarks

                        startedMarks =
                            Set.dropIf (Set.memberOf state.priorMarks) currentMarks
                                |> Set.toList

                        endedMarks =
                            Set.dropIf (Set.memberOf currentMarks) state.priorMarks
                                |> Set.toList

                        startStyles =
                            List.map (MarkedSegmentFirst >> styleClassName >> Class) startedMarks

                        currentStyles =
                            List.map (MarkedSegment >> styleClassName >> Class) segmentMarks

                        ( maybeStartLabels, styles ) =
                            case List.filterMap .name startedMarks of
                                [] ->
                                    ( Nothing, startStyles ++ currentStyles )

                                names ->
                                    ( Just <|
                                        Unstyled.span (List.map toHtmlAttribute startStyles)
                                            [ viewInlineTag
                                                HiddenTags
                                                (String.Extra.toSentenceOxford names)
                                            ]
                                    , currentStyles
                                    )
                    in
                    { priorMarks = currentMarks, revStyles = ( segmentContent, maybeStartLabels, styles ) :: updateEndRevStyles state.revStyles endedMarks }
                )
                { priorMarks = Set.empty markSorter, revStyles = [] }
                segments
    in
    updateEndRevStyles revStyles (Set.toList priorMarks)
        |> List.reverse


markSorter : Sorter Mark
markSorter =
    Sort.by (.name >> Maybe.withDefault "") Sort.alphabetical


{-| When elements are marked, wrap them in a single `mark` html node.

Show the label for the mark, if present, in-line with the emphasized content.

-}
viewWithInlineTags :
    (content -> List Attribute -> Unstyled.Html msg)
    -> List ( content, Maybe Mark )
    -> List (Unstyled.Html msg)
viewWithInlineTags viewSegment segments =
    view_ InlineTags viewSegment segments


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
    , skipTagAnimation : Bool
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
            tagBeforeContent [ marked ] ++ marked.startStyles

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
    -> (content -> List Attribute -> Unstyled.Html msg)
    -> List ( content, Maybe Mark )
    -> List (Unstyled.Html msg)
view_ tagStyle viewSegment highlightables =
    case highlightables of
        [] ->
            []

        ( _, marked ) :: _ ->
            let
                segments =
                    List.map
                        (\( content, _ ) -> viewSegment content [])
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
        , skipTagAnimation : Bool
    }
    -> Mark
    -> List (Html msg)
    -> Html msg
viewMarkedByBalloon config markedWith segments =
    Html.mark
        [ -- Drop the `mark` role as various screen readers interpret it differently.
          -- Instead we offer additional invisible and accessible content to denote the highlight.
          Role.presentation
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


styleClassStyle : StyleClass -> Css.Global.Snippet
styleClassStyle styleClass =
    case styleClass of
        MarkedMark tagStyle markedWith ->
            -- <mark> is not used in overlapping or foldable highlights
            -- it's safe to duplicate styles here that are also only created in the overlapping case
            Css.Global.selector ("mark." ++ styleClassName styleClass)
                [ Css.backgroundColor Css.transparent
                , Css.Global.children
                    [ Css.Global.selector ":first-child"
                        [ if tagStyle == InlineTags && markedWith.name == Nothing then
                            Css.batch []

                          else
                            Css.batch markedWith.startStyles
                        , if tagStyle == HiddenTags && markedWith.name /= Nothing then
                            -- TODO: check whether we should remove startStyles when in high contrast mode
                            MediaQuery.highContrastMode
                                [ -- override for the left border that's typically
                                  -- added in Nri.Ui.HighlighterTool
                                  Css.important (Css.borderLeftWidth Css.zero)
                                ]

                          else
                            Css.batch []
                        ]
                    , Css.Global.selector ":nth-child(2)"
                        [ -- if we're on the first highlighted element, inline/hidden tag wrapper
                          -- we add a `before` content saying what kind of highlight we're starting
                          Css.batch (tagBeforeContent [ markedWith ])
                        ]
                    , Css.Global.typeSelector "span" markedWith.styles
                    , Css.Global.selector ":last-child"
                        (tagAfterContent [ markedWith ]
                            ++ markedWith.endStyles
                        )
                    ]
                ]

        MarkedSegmentFirst markedWith ->
            Css.Global.class (styleClassName styleClass)
                [ Css.batch (tagBeforeContent [ markedWith ])
                , Css.batch markedWith.startStyles
                ]

        MarkedSegmentLast markedWith ->
            Css.Global.class (styleClassName styleClass)
                [ Css.batch (tagAfterContent [ markedWith ])
                , Css.batch markedWith.endStyles
                ]

        MarkedSegment markedWith ->
            Css.Global.class (styleClassName styleClass) [ Css.batch markedWith.styles ]

        InlineTag tagStyle marked ->
            Css.Global.selector ("span." ++ styleClassName styleClass)
                [ Css.batch marked.styles
                , -- start styles are attached to the first segment, unless there's
                  -- an inline tag to show, in which case we'll attach the styles to the start tag.
                  case tagStyle of
                    HiddenTags ->
                        if marked.name == Nothing then
                            Css.batch []

                        else
                            MediaQuery.highContrastMode marked.startStyles

                    InlineTags ->
                        if marked.name == Nothing then
                            Css.batch []

                        else
                            Css.batch marked.startStyles
                ]

        InlineTagContent tagStyle ->
            Css.Global.class (styleClassName styleClass)
                [ Fonts.baseFont
                , Css.backgroundColor Colors.white
                , Css.color Colors.navy
                , Css.padding2 (Css.px 2) (Css.px 4)
                , Css.borderRadius (Css.px 3)
                , Css.margin2 Css.zero (Css.px 5)
                , Css.boxShadow5 Css.zero (Css.px 1) (Css.px 1) Css.zero Colors.gray75
                , case tagStyle of
                    InlineTags ->
                        Css.batch
                            [ MediaQuery.highContrastMode
                                [ Css.property "forced-color-adjust" "none"
                                , Css.property "color" "initial" |> Css.important
                                ]
                            ]

                    HiddenTags ->
                        Css.batch
                            [ Css.display Css.none
                            , MediaQuery.highContrastMode
                                [ Css.property "forced-color-adjust" "none"
                                , Css.display Css.inline |> Css.important
                                , Css.property "color" "initial" |> Css.important
                                ]
                            ]
                ]


type StyleClass
    = MarkedMark TagStyle Mark
    | InlineTag TagStyle Mark
    | InlineTagContent TagStyle
    | MarkedSegmentFirst Mark
    | MarkedSegmentLast Mark
    | MarkedSegment Mark


styleClassName : StyleClass -> String
styleClassName styleClass =
    sanitizeCssClassName <|
        case styleClass of
            MarkedMark HiddenTags { name } ->
                "mark-hidden-" ++ Maybe.withDefault "highlight" name

            MarkedMark InlineTags { name } ->
                "mark-inline-" ++ Maybe.withDefault "highlight" name

            InlineTag HiddenTags { name } ->
                "highlighter-hidden-tag-wrapper-" ++ Maybe.withDefault "highlighted" name

            InlineTag InlineTags { name } ->
                "highlighter-inline-tag-wrapper-" ++ Maybe.withDefault "highlighted" name

            InlineTagContent HiddenTags ->
                "highlighter-hidden-tag-content"

            InlineTagContent InlineTags ->
                "highlighter-inline-tag-content"

            MarkedSegmentFirst { name } ->
                "marked-start-" ++ Maybe.withDefault "highlight" name

            MarkedSegmentLast { name } ->
                "marked-end-" ++ Maybe.withDefault "highlight" name

            MarkedSegment { name } ->
                "marked-" ++ Maybe.withDefault "highlight" name


sanitizeCssClassName : String -> String
sanitizeCssClassName =
    case Regex.fromString "[ \\(\\)\\[\\]]" of
        Nothing ->
            identity

        Just regex ->
            Regex.replace regex (\_ -> "_")


viewMarked : TagStyle -> Mark -> List (Unstyled.Html msg) -> Unstyled.Html msg
viewMarked tagStyle markedWith segments =
    Unstyled.mark
        [ Html.Attributes.class (styleClassName (MarkedMark tagStyle markedWith))
        , -- Drop the `mark` role as various screen readers interpret it differently.
          -- Instead we offer additional invisible and accessible content to denote the highlight.
          Html.Attributes.attribute "role" "presentation"
        ]
        (case markedWith.name of
            Just name ->
                viewStartHighlightTag tagStyle markedWith name
                    :: segments

            Nothing ->
                segments
        )


viewStartHighlightTag : TagStyle -> Mark -> String -> Unstyled.Html msg
viewStartHighlightTag tagStyle marked name =
    Unstyled.span
        [ Html.Attributes.class (styleClassName (InlineTag tagStyle marked)) ]
        [ viewInlineTag tagStyle name ]


tagBeforeContent : List Mark -> List Css.Style
tagBeforeContent marks =
    if List.isEmpty marks then
        []

    else
        [ Css.before
            [ cssContent (highlightDescription "start" marks)
            , invisibleStyle
            ]
        ]


tagAfterContent : List Mark -> List Css.Style
tagAfterContent marks =
    if List.isEmpty marks then
        []

    else
        [ Css.after
            [ cssContent (highlightDescription "end" marks)
            , invisibleStyle
            ]
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


viewInlineTag : TagStyle -> String -> Unstyled.Html msg
viewInlineTag tagStyle name =
    Unstyled.span
        [ -- we use the :before element to convey details about the start of the
          -- highlighter to screenreaders, so the visual label is redundant
          Html.Attributes.attribute "aria-hidden" "true"
        , Html.Attributes.class (styleClassName (InlineTagContent tagStyle))
        ]
        (Content.markdownInlineUnstyled name)


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


viewBalloonSpacer : { config | labelPosition : Maybe { b | totalHeight : Float }, skipTagAnimation : Bool } -> Html msg
viewBalloonSpacer config =
    span
        [ css
            [ Css.display Css.inlineBlock
            , if config.skipTagAnimation then
                Css.batch []

              else
                Css.Media.withMediaQuery
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
                            Css.lineHeight Css.initial
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