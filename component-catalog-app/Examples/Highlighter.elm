port module Examples.Highlighter exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Code
import Css exposing (Color)
import Css.Global
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Examples.Colors
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import List.Extra
import Maybe.Extra
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Highlightable.V3 as Highlightable exposing (Highlightable)
import Nri.Ui.Highlighter.V4 as Highlighter
import Nri.Ui.HighlighterTool.V1 as Tool
import Nri.Ui.Table.V7 as Table
import Nri.Ui.Text.V6 as Text
import Sort exposing (Sorter)
import String.Extra


moduleName : String
moduleName =
    "Highlighter"


version : Int
version =
    3


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.map HighlighterMsg subscriptions
    , preview =
        [ div [ css [ Fonts.baseFont, Css.lineHeight (Css.int 2), Css.Global.children [ Css.Global.p [ Css.margin Css.zero ] ] ] ]
            [ Highlighter.static
                { id = "highlight-preview"
                , highlightables =
                    [ ( "Sphinx", Nothing )
                    , ( "of", Nothing )
                    , ( "black", Just exampleMarker )
                    , ( "quartz,", Nothing )
                    , ( "judge", Nothing )
                    , ( "my", Nothing )
                    , ( "vow.", Nothing )
                    ]
                        |> List.intersperse ( " ", Nothing )
                        |> List.indexedMap (\i ( word, marker ) -> Highlightable.initStatic (Maybe.Extra.toList marker) i word)
                }
            ]
        ]
    , view =
        \ellieLinkConfig state ->
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControls
                , settings = state.settings
                , mainType = Nothing
                , extraCode =
                    [ "import Nri.Ui.Highlightable.V2 as Highlightable"
                    , "import Nri.Ui.HighlighterTool.V1 as Tool"
                    ]
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "Code"
                          , code =
                                -- view
                                Tuple.first (view state)
                                    ++ Code.newlines
                                    ++ -- model
                                       (initHighlighter (Control.currentValue state.settings) state.highlighter.highlightables
                                            |> Tuple.first
                                       )
                          }
                        ]
                }
            , Heading.h2 [ Heading.plaintext "Interactive example" ]
            , Text.mediumBody [ Text.plaintext "This example updates based on the settings you configure on this page." ]
            , Button.button "Clear all highlights"
                [ Button.onClick ClearHighlights
                , Button.secondary
                , Button.small
                ]
            , div
                [ css
                    [ Css.fontSize (Css.px 24)
                    , Css.lineHeight (Css.num 1.75)
                    , Fonts.quizFont
                    ]
                ]
                [ Tuple.second (view state)
                    |> map HighlighterMsg
                ]
            , Heading.h2 [ Heading.plaintext "Overlapping highlights example" ]
            , Text.mediumBody [ Text.plaintext "Supporting overlapping highlights, as in inline comments, requires a lot of extra set-up. Generally, you won't need this." ]
            , div
                [ css
                    [ Css.fontSize (Css.px 24)
                    , Css.lineHeight (Css.num 1.75)
                    , Fonts.ugFont
                    ]
                ]
                [ Highlighter.viewWithOverlappingHighlights state.overlappingHighlightsState
                    |> map OverlappingHighlighterMsg
                ]
            , Heading.h2 [ Heading.plaintext "Non-interactive examples" ]
            , Text.mediumBody [ Text.plaintext "These are examples of some different ways the highlighter can appear to users." ]
            , Table.view []
                [ Table.rowHeader
                    { header = text "Highlighter."
                    , view = .viewName >> text
                    , width = Css.zero
                    , cellStyles =
                        always
                            [ Css.padding2 (Css.px 14) (Css.px 7)
                            , Css.verticalAlign Css.middle
                            , Css.textAlign Css.left
                            , Css.fontWeight Css.normal
                            ]
                    , sort = Nothing
                    }
                , Table.string
                    { header = "HighlighterTool."
                    , value = .tool
                    , width = Css.pct 10
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
                    , sort = Nothing
                    }
                , Table.string
                    { header = "Highlightable."
                    , value = .highlightable
                    , width = Css.pct 10
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
                    , sort = Nothing
                    }
                , Table.string
                    { header = "Description"
                    , value = .description
                    , width = Css.pct 30
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Example"
                    , view = .example
                    , width = Css.pct 60
                    , cellStyles =
                        always
                            [ Css.padding2 (Css.px 14) (Css.px 7)
                            , Css.verticalAlign Css.middle
                            , Css.lineHeight (Css.num 2)
                            ]
                    , sort = Nothing
                    }
                ]
                [ { viewName = "static"
                  , tool = "buildMarker"
                  , highlightable = "init"
                  , description = "One word highlighted"
                  , example =
                        Highlighter.static
                            { id = "example-0"
                            , highlightables =
                                [ ( "Sphinx", Nothing )
                                , ( "of", Nothing )
                                , ( "black", Just exampleMarker )
                                , ( "quartz,", Nothing )
                                , ( "judge", Nothing )
                                , ( "my", Nothing )
                                , ( "vow.", Nothing )
                                ]
                                    |> List.intersperse ( " ", Nothing )
                                    |> List.indexedMap (\i ( word, marker ) -> Highlightable.initStatic (Maybe.Extra.toList marker) i word)
                            }
                  }
                , { viewName = "static"
                  , tool = "buildMarker"
                  , highlightable = "init"
                  , description = "Multiple words highlighted separately"
                  , example =
                        Highlighter.static
                            { id = "example-1"
                            , highlightables =
                                [ ( "Sphinx", Nothing )
                                , ( "of", Nothing )
                                , ( "black", Just exampleMarker )
                                , ( "quartz,", Just exampleMarker )
                                , ( "judge", Nothing )
                                , ( "my", Nothing )
                                , ( "vow.", Nothing )
                                ]
                                    |> List.intersperse ( " ", Nothing )
                                    |> List.indexedMap (\i ( word, marker ) -> Highlightable.initStatic (Maybe.Extra.toList marker) i word)
                            }
                  }
                , { viewName = "static"
                  , tool = "buildMarker"
                  , highlightable = "init"
                  , description = "Multiple words highlighted & joined"
                  , example =
                        Highlighter.static
                            { id = "example-2"
                            , highlightables =
                                [ ( "Sphinx", Nothing )
                                , ( "of", Nothing )
                                , ( "black quartz,", Just exampleMarker )
                                , ( "judge", Nothing )
                                , ( "my", Nothing )
                                , ( "vow.", Nothing )
                                ]
                                    |> List.intersperse ( " ", Nothing )
                                    |> List.indexedMap (\i ( word, marker ) -> Highlightable.initStatic (Maybe.Extra.toList marker) i word)
                            }
                  }
                , { viewName = "static"
                  , tool = "buildMarker"
                  , highlightable = "init"
                  , description = "Multiple kinds of highlights without overlaps"
                  , example = Highlighter.static { id = "example-3a", highlightables = multipleHighlightsHighlightables }
                  }
                , { viewName = "staticMarkdown"
                  , tool = "buildMarker"
                  , highlightable = "init"
                  , description = "Multiple kinds of highlights without overlaps and with interpreted Markdown"
                  , example = Highlighter.staticMarkdown { id = "example-3b", highlightables = multipleHighlightsHighlightables }
                  }
                , { viewName = "staticWithTags"
                  , tool = "buildMarkerWithBorder"
                  , highlightable = "init"
                  , description = "Multiple kinds of highlights without overlaps"
                  , example = Highlighter.staticWithTags { id = "example-4a", highlightables = multipleHighlightsHighlightablesWithBorder }
                  }
                , { viewName = "staticMarkdownWithTags"
                  , tool = "buildMarkerWithBorder"
                  , highlightable = "init"
                  , description = "Multiple kinds of highlights without overlaps and with interpreted Markdown"
                  , example = Highlighter.staticMarkdownWithTags { id = "example-4b", highlightables = multipleHighlightsHighlightablesWithBorder }
                  }
                , { viewName = "staticMarkdown"
                  , tool = "buildMarker"
                  , highlightable = "fromMarkdown"
                  , description = "Interpreting empty markdown anchor tags as highlights."
                  , example =
                        Highlighter.staticMarkdown
                            { id = "example-5"
                            , highlightables = Highlightable.fromMarkdown "Select your [favorite phrase]() in **your** writing."
                            }
                  }
                ]
            ]
    , categories = [ Instructional ]
    , keyboardSupport = []
    }


multipleHighlightsHighlightables : List (Highlightable ())
multipleHighlightsHighlightables =
    [ ( "Waltz, bad nymph, for quick jigs vex.", Just claimMarker )
    , ( "Glib jocks quiz nymph to vex dwarf.", Just evidenceMarker )
    , ( "Sphinx of _black_ quartz, judge my vow.", Just reasoningMarker )
    , ( "How *vexingly* quick daft zebras jump!", Nothing )
    ]
        |> List.intersperse ( " ", Nothing )
        |> List.indexedMap (\i ( word, marker ) -> Highlightable.initStatic (Maybe.Extra.toList marker) i word)


exampleMarker : Tool.MarkerModel ()
exampleMarker =
    Tool.buildMarker
        { highlightColor = Colors.highlightYellow
        , hoverColor = Colors.highlightMagenta
        , hoverHighlightColor = Colors.highlightPurpleDark
        , kind = ()
        , name = Nothing
        }


claimMarker : Tool.MarkerModel ()
claimMarker =
    Tool.buildMarker
        { highlightColor = Colors.highlightYellow
        , hoverColor = Colors.highlightMagenta
        , hoverHighlightColor = Colors.highlightPurpleDark
        , kind = ()
        , name = Just "Claim"
        }


evidenceMarker : Tool.MarkerModel ()
evidenceMarker =
    Tool.buildMarker
        { highlightColor = Colors.highlightCyan
        , hoverColor = Colors.highlightMagenta
        , hoverHighlightColor = Colors.highlightPurpleDark
        , kind = ()
        , name = Just "Evidence"
        }


reasoningMarker : Tool.MarkerModel ()
reasoningMarker =
    Tool.buildMarker
        { highlightColor = Colors.highlightPurple
        , hoverColor = Colors.highlightMagenta
        , hoverHighlightColor = Colors.highlightPurpleDark
        , kind = ()
        , name = Just "Reasoning"
        }


inlineCommentMarker : String -> Tool.MarkerModel String
inlineCommentMarker name =
    Tool.buildMarkerWithoutRounding
        { highlightColor = toLightColor Colors.highlightYellow
        , hoverColor = Colors.highlightYellow
        , hoverHighlightColor = Colors.highlightYellow
        , kind = name
        , name = Just name
        }


toLightColor : Color -> Color
toLightColor color =
    let
        tint value =
            value + floor (toFloat (255 - value) * 0.5)
    in
    Css.rgb (tint color.red) (tint color.green) (tint color.blue)


multipleHighlightsHighlightablesWithBorder : List (Highlightable ())
multipleHighlightsHighlightablesWithBorder =
    [ ( "Waltz, bad nymph, for quick jigs vex.", Just claimMarkerWithBorder )
    , ( "Glib jocks quiz nymph to vex dwarf.", Just evidenceMarkerWithBorder )
    , ( "Sphinx of _black_ quartz, judge my vow.", Just reasoningMarkerWithBorder )
    , ( "How *vexingly* quick daft zebras jump!", Nothing )
    ]
        |> List.intersperse ( " ", Nothing )
        |> List.indexedMap (\i ( word, marker ) -> Highlightable.initStatic (Maybe.Extra.toList marker) i word)


claimMarkerWithBorder : Tool.MarkerModel ()
claimMarkerWithBorder =
    Tool.buildMarkerWithBorder
        { highlightColor = Colors.highlightYellow
        , kind = ()
        , name = Just "Claim"
        }


evidenceMarkerWithBorder : Tool.MarkerModel ()
evidenceMarkerWithBorder =
    Tool.buildMarkerWithBorder
        { highlightColor = Colors.highlightCyan
        , kind = ()
        , name = Just "Evidence"
        }


reasoningMarkerWithBorder : Tool.MarkerModel ()
reasoningMarkerWithBorder =
    Tool.buildMarkerWithBorder
        { highlightColor = Colors.highlightPurple
        , kind = ()
        , name = Just "Reasoning"
        }


view : State -> ( String, Html (Highlighter.Msg ()) )
view state =
    let
        viewStr =
            Code.var "view" 1
    in
    case (Control.currentValue state.settings).highlighterType of
        Markdown ->
            ( viewStr "Highlighter.viewMarkdown"
            , Highlighter.viewMarkdown state.highlighter
            )

        Standard ->
            ( viewStr "Highlighter.view"
            , Highlighter.view state.highlighter
            )


{-| -}
type alias State =
    { settings : Control Settings
    , highlighter : Highlighter.Model ()
    , overlappingHighlightsState : Highlighter.Model String
    , overlappingHighlightsIndex : Int
    }


{-| -}
init : State
init =
    let
        settings =
            controlSettings
    in
    { settings = settings
    , highlighter = initHighlighter (Control.currentValue settings) [] |> Tuple.second
    , overlappingHighlightsState =
        Highlighter.init
            { id = "student-writing"
            , highlightables = Highlightable.initFragments "Letter grades have a variety of effects on students. Alfie Kohn, an American author who specializes in education issues, explains that students who are graded “tend to lose interest in the learning itself [and] avoid challenging tasks whenever possible.” Kohn’s argument illustrates how letter grades can become a source of stress for students and distract them from the joys of learning."
            , marker = Tool.Marker (inlineCommentMarker "Comment 1")
            , sorter = Sort.alphabetical
            , joinAdjacentInteractiveHighlights = True
            }
    , overlappingHighlightsIndex = 1
    }


initHighlighter : Settings -> List (Highlightable ()) -> ( String, Highlighter.Model () )
initHighlighter settings previousHighlightables =
    let
        highlightables : ( String, List (Highlightable ()) )
        highlightables =
            if settings.splitOnSentences then
                exampleParagraph
                    |> List.map
                        (\text i ->
                            ( "Highlightable.initInteractive [] " ++ String.fromInt i ++ Code.string text
                            , Highlightable.initInteractive [] i text
                            )
                        )
                    |> List.intersperse
                        (\i ->
                            ( "Highlightable.initStatic [] " ++ String.fromInt i ++ Code.string " "
                            , Highlightable.initStatic [] i " "
                            )
                        )
                    |> List.indexedMap (\i f -> f i)
                    |> List.unzip
                    |> Tuple.mapFirst (\c -> Code.listMultiline c 3)

            else
                ( "Highlightable.initFragments " ++ Code.string joinedExampleParagraph
                , Highlightable.initFragments joinedExampleParagraph
                )

        joinedExampleParagraph =
            String.join " " exampleParagraph
    in
    ( Code.var "model" 1 <|
        Code.fromModule moduleName "init"
            ++ Code.recordMultiline
                [ ( "id", Code.string "example-romeo-and-juliet" )
                , ( "highlightables", Tuple.first highlightables )
                , ( "marker", Code.newlineWithIndent 3 ++ Tuple.first settings.tool )
                , ( "joinAdjacentInteractiveHighlights", Code.bool settings.joinAdjacentInteractiveHighlights )
                ]
                2
    , Highlighter.init
        { id = "example-romeo-and-juliet"
        , highlightables =
            if List.map .text previousHighlightables == List.map .text (Tuple.second highlightables) then
                previousHighlightables

            else
                Tuple.second highlightables
        , marker = Tuple.second settings.tool
        , sorter = sorter
        , joinAdjacentInteractiveHighlights = settings.joinAdjacentInteractiveHighlights
        }
    )


exampleParagraph : List String
exampleParagraph =
    [ "Taking notes by hand is better for students' overall academic performance than taking notes on a computer."
    , "A study published in the journal *Psychological Science* found that students who handwrote their notes during class gained a deeper understanding of new material than students who typed their notes."
    , "This study suggests that students are better served by writing out their notes rather than typing them."
    ]


type alias Settings =
    { splitOnSentences : Bool
    , joinAdjacentInteractiveHighlights : Bool
    , highlighterType : HighlighterType
    , tool : ( String, Tool.Tool () )
    }


type HighlighterType
    = Markdown
    | Standard


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "splitOnSentences" (Control.bool True)
        |> Control.field "joinAdjacentInteractiveHighlights" (Control.bool False)
        |> Control.field "type"
            (Control.choice
                [ ( "Markdown", Control.value Markdown )
                , ( "Standard", Control.value Standard )
                ]
            )
        |> Control.field "tool"
            (Control.choice
                [ ( "Marker", Control.map (\( c, v ) -> ( "Tool.Marker" ++ c, Tool.Marker v )) controlMarker )
                , ( "Eraser", Control.value ( "Tool.Eraser Tool.buildEraser", Tool.Eraser Tool.buildEraser ) )
                ]
            )


controlMarker : Control ( String, Tool.MarkerModel () )
controlMarker =
    Control.record
        (\a b c d ->
            ( Code.fromModule "Tool" "buildMarker"
                ++ Code.recordMultiline
                    [ ( "highlightColor", Tuple.first a )
                    , ( "hoverColor", Tuple.first b )
                    , ( "hoverHighlightColor", Tuple.first c )
                    , ( "kind", "()" )
                    , ( "name", Code.maybeString d )
                    ]
                    4
            , Tool.buildMarker
                { highlightColor = Tuple.second a
                , hoverColor = Tuple.second b
                , hoverHighlightColor = Tuple.second c
                , kind = ()
                , name = d
                }
            )
        )
        |> Control.field "highlightColor" (backgroundHighlightColors 0)
        |> Control.field "hoverColor" (backgroundHighlightColors 2)
        |> Control.field "hoverHighlightColor" (backgroundHighlightColors 4)
        |> Control.field "name" (Control.maybe True (Control.string "Claim"))


backgroundHighlightColors : Int -> Control ( String, Color )
backgroundHighlightColors rotateWith =
    Examples.Colors.backgroundHighlightColors
        |> List.map (\( name, value, _ ) -> ( name, Control.value ( Code.fromModule "Colors" name, value ) ))
        |> ControlExtra.rotatedChoice rotateWith


{-| -}
type Msg
    = UpdateControls (Control Settings)
    | HighlighterMsg (Highlighter.Msg ())
    | OverlappingHighlighterMsg (Highlighter.Msg String)
    | ClearHighlights


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControls settings ->
            ( { state
                | settings = settings
                , highlighter =
                    initHighlighter (Control.currentValue settings) state.highlighter.highlightables
                        |> Tuple.second
              }
            , Cmd.none
            )

        HighlighterMsg highlighterMsg ->
            let
                ( newHighlighter, effect, Highlighter.Intent intent ) =
                    Highlighter.update highlighterMsg state.highlighter
            in
            ( { state | highlighter = newHighlighter }
            , Cmd.batch
                [ Cmd.map HighlighterMsg effect
                , case intent.listenTo of
                    Just listenTo ->
                        highlighterListen listenTo

                    Nothing ->
                        Cmd.none
                ]
            )

        ClearHighlights ->
            ( { state | highlighter = Highlighter.removeHighlights state.highlighter }
            , Cmd.none
            )

        OverlappingHighlighterMsg highlighterMsg ->
            -- This code is extracted with minimal updates from Nri.Writing.GuidedDrafts.WritingSamples.Highlighters
            case Highlighter.update highlighterMsg state.overlappingHighlightsState of
                ( newHighlighter, effect, intent ) ->
                    let
                        clickedCommentId =
                            Highlighter.selectShortest Highlighter.clickedHighlightable state.overlappingHighlightsState

                        hoveredCommentId =
                            Highlighter.selectShortest Highlighter.hoveredHighlightable newHighlighter

                        maybePreviousMouseDownIndex =
                            highlighterState.mouseDownIndex

                        mouseOverIndex =
                            newHighlighter.mouseOverIndex

                        withAllCommentIds =
                            { newHighlighter
                                | highlightables =
                                    List.map2
                                        (\newHighlightable oldHighlightable ->
                                            { newHighlightable | marked = List.Extra.unique (newHighlightable.marked ++ oldHighlightable.marked) }
                                        )
                                        newHighlighter.highlightables
                                        highlighterState.highlightables
                            }

                        highlighterState =
                            state.overlappingHighlightsState

                        newComment =
                            { state
                                | overlappingHighlightsState =
                                    { withAllCommentIds
                                        | marker = Tool.Marker (inlineCommentMarker ("Comment " ++ String.fromInt (state.overlappingHighlightsIndex + 1)))
                                    }
                                , overlappingHighlightsIndex = state.overlappingHighlightsIndex + 1
                            }
                    in
                    -- The Changed action will be triggered on the Highlighter Up event and
                    -- when there is an actual change in the highlightable elements. Note
                    -- that when we click an existing highlight, from the point of view of
                    -- the highlighter, this is just the same as starting a new highlight
                    -- (instead we want to intercept that click and do something else).
                    case Highlighter.hasChanged intent of
                        Highlighter.Changed ->
                            case ( clickedCommentId, maybePreviousMouseDownIndex, mouseOverIndex ) of
                                ( Just commentId, Just previousMouseDownIndex, Just currentHover ) ->
                                    if previousMouseDownIndex == currentHover then
                                        -- User is clicking an existing highlight without dragging
                                        -- we don't want to use the entire updated highlighter state, since we don't want to remove the focused highlight!
                                        -- However, we do want to ensure that we don't end up in the middle of highlighting, where the user has to click around more to get out of the highlighting state.
                                        ( { state
                                            | overlappingHighlightsState =
                                                { highlighterState
                                                    | mouseDownIndex = Nothing
                                                    , mouseOverIndex = Nothing
                                                    , selectionStartIndex = Nothing
                                                    , selectionEndIndex = Nothing
                                                }
                                          }
                                        , Cmd.none
                                        )

                                    else
                                        ( newComment
                                        , Cmd.batch
                                            [ Cmd.map OverlappingHighlighterMsg effect
                                            , perform intent
                                            ]
                                        )

                                _ ->
                                    ( newComment
                                    , Cmd.batch
                                        [ Cmd.map OverlappingHighlighterMsg effect
                                        , perform intent
                                        ]
                                    )

                        Highlighter.NotChanged ->
                            case maybePreviousMouseDownIndex of
                                Just _ ->
                                    -- User is dragging and highlighting. We don't want to show
                                    -- any hover effect in case the current highlight starts to
                                    -- overlaps with an existing highlight.
                                    ( { state | overlappingHighlightsState = withAllCommentIds }
                                    , Cmd.batch
                                        [ Cmd.map OverlappingHighlighterMsg effect
                                        , perform intent
                                        ]
                                    )

                                Nothing ->
                                    -- User is just hovering around the page
                                    ( { state | overlappingHighlightsState = withAllCommentIds }
                                    , Cmd.batch
                                        [ Cmd.map OverlappingHighlighterMsg effect
                                        , perform intent
                                        ]
                                    )


perform : Highlighter.Intent -> Cmd msg
perform (Highlighter.Intent intent) =
    case intent.listenTo of
        Just listenTo ->
            highlighterListen listenTo

        Nothing ->
            Cmd.none


sorter : Sorter ()
sorter =
    Sort.custom
        (\a b ->
            case ( a, b ) of
                ( (), () ) ->
                    EQ
        )



-- SUBSCRIPTIONS


subscriptions : Sub (Highlighter.Msg marker)
subscriptions =
    Sub.batch [ onDocumentUp, onTouch ]


{-| Subscribe to mouseup/touchend events on the document.
-}
onDocumentUp : Sub (Highlighter.Msg marker)
onDocumentUp =
    highlighterOnDocumentUp (Highlighter.Pointer << Highlighter.Up << Just)


{-| Subscribe to touch events
-}
onTouch : Sub (Highlighter.Msg marker)
onTouch =
    highlighterOnTouch <|
        \( type_, targetId, index ) ->
            Highlighter.Pointer <|
                case type_ of
                    "move" ->
                        Highlighter.Move (Just targetId) index

                    "end" ->
                        Highlighter.Up (Just targetId)

                    _ ->
                        Highlighter.Ignored


{-| Start listening to events on a highlighter
-}
port highlighterListen : String -> Cmd msg


{-| Listen to documentup events, to stop highlighting.
-}
port highlighterOnDocumentUp : (String -> msg) -> Sub msg


{-| Listen to touch events, and get the element under the finger.
-}
port highlighterOnTouch : (( String, String, Int ) -> msg) -> Sub msg
