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
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Highlightable.V3 as Highlightable exposing (Highlightable)
import Nri.Ui.Highlighter.V6 as Highlighter
import Nri.Ui.HighlighterTool.V1 as Tool
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Table.V7 as Table
import Nri.Ui.Text.V6 as Text
import Sort exposing (Sorter)


moduleName : String
moduleName =
    "Highlighter"


version : Int
version =
    6


type ReadOnly
    = -- a `ReadOnly` value isn't used here, but we keep a custom type for clarity
      Interactive


type ScrollFriendly
    = ScrollFriendly
    | NotScrollFriendly


isScrollFriendly : Highlighter.Model marker -> ScrollFriendly
isScrollFriendly model =
    if model.scrollFriendly then
        ScrollFriendly

    else
        NotScrollFriendly


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , init = init
    , update = update
    , subscriptions =
        \_ ->
            Sub.batch
                [ Sub.map HighlighterMsg subscriptions
                , Sub.map OverlappingHighlighterMsg subscriptions
                , Sub.map FoldHighlighterMsg subscriptions
                ]
    , preview =
        [ div [ css [ Fonts.baseFont, Css.lineHeight (Css.num 2), Css.Global.children [ Css.Global.p [ Css.margin Css.zero ] ] ] ]
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
    , about =
        [ Text.smallBody
            [ Text.html
                [ text "Learn more about this component from "
                , ClickableText.link "Tessa's demo"
                    [ ClickableText.linkExternal "https://www.dropbox.com/s/tchb0t9lgdyzw01/2022-11-10%20-%20Tessa%20Kelly%20-%20Nri.Ui.Highlighter.V1.mp4?dl=0&unfurl=slack_gen"
                    , ClickableText.appearsInline
                    ]
                , text " and "
                , ClickableText.link "the highlighter doc"
                    [ ClickableText.linkExternal "https://paper.dropbox.com/doc/Highlighter-component-documentation-mmPuvnWVKcxpb7r3FjkZn"
                    , ClickableText.appearsInline
                    ]
                , text ", which covers use of the component throughout the NoRedInk app."
                ]
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
                , mainType = Just "Program () (Highlighter.Model ()) (Highlighter.Msg ())"
                , extraCode =
                    [ "import Browser"
                    , "import Nri.Ui.Highlightable.V3 as Highlightable"
                    , "import Nri.Ui.HighlighterTool.V1 as Tool"
                    , "import Sort"
                    ]
                , renderExample = identity
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "Code"
                          , code =
                                Code.browserElement
                                    { init =
                                        Code.always
                                            (Code.tuple
                                                "init"
                                                "Cmd.none"
                                            )
                                    , update =
                                        Code.newlineWithIndent 2
                                            ++ Code.anonymousFunction
                                                "msg model"
                                                (Code.pipelineMultiline
                                                    [ Code.fromModule moduleName "update msg model"
                                                    , Code.anonymousFunction "(newModel, cmd, intent)" <|
                                                        Code.tuple "newModel" "cmd"
                                                    ]
                                                    3
                                                )
                                    , view = "view >> toUnstyled"
                                    , subscriptions =
                                        Code.newlineWithIndent 2
                                            ++ Code.anonymousFunction "model"
                                                (Code.newlineWithIndent 3
                                                    ++ " -- for the highlighter to work correctly for touch users,"
                                                    ++ Code.newlineWithIndent 3
                                                    ++ "-- you will need to wire up subscriptions."
                                                    ++ Code.newlineWithIndent 3
                                                    ++ "-- See the Highlighter example source code."
                                                    ++ Code.newlineWithIndent 3
                                                    ++ "Sub.none "
                                                )
                                    }
                                    ++ Code.newlines
                                    ++ -- view
                                       Tuple.first (view state)
                                    ++ Code.newlines
                                    ++ -- init
                                       (initHighlighter (Control.currentValue state.settings) state.highlighter.highlightables
                                            |> Tuple.first
                                       )
                          }
                        ]
                }
            , Heading.h2
                [ Heading.plaintext "Customizable example"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
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
            , Heading.h2
                [ Heading.plaintext "Overlapping highlights example"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , Text.mediumBody [ Text.plaintext "Supporting overlapping highlights, as in inline comments, requires a lot of extra set-up. Generally, you won't need this." ]
            , Text.mediumBody [ Text.plaintext "This example does not support removing highlights. This is to enable you to create highlights that overlap." ]
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
            , Heading.h2
                [ Heading.plaintext "Structured Highlighter example"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , Text.mediumBody [ Text.plaintext "If you have text that is structured in such a way that a simple paragraph won't work, you can use the Highlighter folding functions to render highlightables one by one inside of your HTML structure." ]
            , div
                [ css
                    [ Css.fontSize (Css.px 24)
                    , Css.lineHeight (Css.num 1.75)
                    , Fonts.ugFont
                    ]
                , Html.Styled.Attributes.id state.foldHighlightsState.id
                , Html.Styled.Attributes.class "highlighter-container"
                ]
                [ viewFoldHighlights state.foldHighlightsState
                    |> map FoldHighlighterMsg
                ]
            , Heading.h2
                [ Heading.plaintext "Non-interactive examples"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
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
                , { viewName = "staticMarkdown"
                  , tool = "buildMarker"
                  , highlightable = "fromMarkdown"
                  , description = "Interpreting <nri-highlight> tags with a custom color."
                  , example =
                        Highlighter.staticMarkdown
                            { id = "example-6"
                            , highlightables = Highlightable.fromMarkdown "Select your <nri-highlight color=\"cyan\">favorite phrase with favorite color</nri-highlight> in **your** writing."
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
    case (Control.currentValue state.settings).textSettings.highlighterType of
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
    , foldHighlightsState : Highlighter.Model String
    }


foldHighlightsSource : List (List String)
foldHighlightsSource =
    [ [ "This", " ", "is", " ", "a", " ", "list", " ", "item" ]
    , [ "Here", " ", "is", " ", "another" ]
    , [ "Finally", " ", "one", " ", "more" ]
    ]


viewFoldHighlights : Highlighter.Model String -> Html (Highlighter.Msg String)
viewFoldHighlights model =
    List.Extra.mapAccuml
        (\state sourceRow ->
            List.Extra.mapAccuml (\innerState _ -> Highlighter.viewFoldHighlighter [] innerState) state sourceRow
                |> Tuple.mapSecond (List.concat >> li [])
        )
        (Highlighter.initFoldState model)
        foldHighlightsSource
        |> Tuple.second
        |> ul []


{-| -}
init : ( State, Cmd msg )
init =
    let
        settings =
            controlSettings

        highlighterState =
            initHighlighter (Control.currentValue settings) [] |> Tuple.second

        overlappingHighlightsState =
            Highlighter.init
                { id = "student-writing"
                , highlightables = Highlightable.initFragments "Letter grades have a variety of effects on students. Alfie Kohn, an American author who specializes in education issues, explains that students who are graded “tend to lose interest in the learning itself [and] avoid challenging tasks whenever possible.” Kohn’s argument illustrates how letter grades can become a source of stress for students and distract them from the joys of learning."
                , marker = Tool.Marker (inlineCommentMarker "Comment 1")
                , sorter = Sort.alphabetical
                , joinAdjacentInteractiveHighlights = True
                , scrollFriendly = False
                }

        foldHighlightsState =
            Highlighter.init
                { id = "student-writing-fold"
                , highlightables =
                    List.concat foldHighlightsSource
                        |> List.indexedMap (Highlightable.initInteractive [])
                , marker = Tool.Marker (inlineCommentMarker "Comment 1")
                , sorter = Sort.alphabetical
                , joinAdjacentInteractiveHighlights = True
                , scrollFriendly = False
                }
    in
    ( { settings = settings
      , highlighter = highlighterState
      , overlappingHighlightsState = overlappingHighlightsState
      , foldHighlightsState = foldHighlightsState
      , overlappingHighlightsIndex = 1
      }
    , Cmd.batch
        [ initHighlighterPort highlighterState Interactive
        , initHighlighterPort overlappingHighlightsState Interactive
        , initHighlighterPort foldHighlightsState Interactive
        ]
    )


initHighlighter : Settings -> List (Highlightable ()) -> ( String, Highlighter.Model () )
initHighlighter settings previousHighlightables =
    let
        highlightables : ( String, List (Highlightable ()) )
        highlightables =
            if settings.textSettings.splitOnSentences then
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
                    |> Tuple.mapFirst (\c -> Code.listMultiline (List.take 2 c) 3)

            else
                ( "Highlightable.initFragments " ++ Code.string joinedExampleParagraph
                , Highlightable.initFragments joinedExampleParagraph
                )

        joinedExampleParagraph =
            String.join " " exampleParagraph
    in
    ( Code.var "init" 1 <|
        Code.fromModule moduleName "init"
            ++ Code.recordMultiline
                [ ( "id", Code.string "example-romeo-and-juliet" )
                , ( "highlightables", Tuple.first highlightables )
                , ( "marker", Code.newlineWithIndent 3 ++ Tuple.first settings.tool.tool )
                , ( "joinAdjacentInteractiveHighlights", Code.bool settings.textSettings.joinAdjacentInteractiveHighlights )
                , ( "scrollFriendly", Code.bool settings.textSettings.scrollFriendly )
                , ( "sorter", "Sort.custom (\\() () -> EQ)" )
                ]
                2
    , Highlighter.init
        { id = "example-romeo-and-juliet"
        , highlightables =
            if List.map .text previousHighlightables == List.map .text (Tuple.second highlightables) then
                previousHighlightables

            else
                Tuple.second highlightables
        , marker = Tuple.second settings.tool.tool
        , sorter = sorter
        , joinAdjacentInteractiveHighlights = settings.textSettings.joinAdjacentInteractiveHighlights
        , scrollFriendly = settings.textSettings.scrollFriendly
        }
    )


exampleParagraph : List String
exampleParagraph =
    [ "Taking notes by hand is better for students' overall academic performance than taking notes on a computer."
    , "A study published in the journal *Psychological Science* found that students who handwrote their notes during class gained a deeper understanding of new material than students who typed their notes."
    , "This study suggests that students are better served by writing out their notes rather than typing them."
    ]


type alias Settings =
    { textSettings :
        { splitOnSentences : Bool
        , joinAdjacentInteractiveHighlights : Bool
        , scrollFriendly : Bool
        , highlighterType : HighlighterType
        }
    , tool : { tool : ( String, Tool.Tool () ) }
    }


type HighlighterType
    = Markdown
    | Standard


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "Text settings"
            (Control.record
                (\a b c d ->
                    { splitOnSentences = a
                    , joinAdjacentInteractiveHighlights = b
                    , scrollFriendly = c
                    , highlighterType = d
                    }
                )
                |> Control.field "splitOnSentences" (Control.bool True)
                |> Control.field "joinAdjacentInteractiveHighlights" (Control.bool False)
                |> Control.field "scrollFriendly" (Control.bool False)
                |> Control.field "type"
                    (Control.choice
                        [ ( "Markdown", Control.value Markdown )
                        , ( "Standard", Control.value Standard )
                        ]
                    )
            )
        |> Control.field "Highlighter or Eraser tool settings"
            (Control.record (\tool -> { tool = tool })
                |> Control.field "tool"
                    (Control.choice
                        [ ( "Marker", Control.map (\( c, v ) -> ( "Tool.Marker" ++ Code.withParensMultiline c 4, Tool.Marker v )) controlMarker )
                        , ( "Eraser", Control.value ( "Tool.Eraser Tool.buildEraser", Tool.Eraser Tool.buildEraser ) )
                        ]
                    )
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
                    5
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
        |> Control.field "hoverColor" (backgroundHighlightColors 5)
        |> Control.field "hoverHighlightColor" (backgroundHighlightColors 10)
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
    | FoldHighlighterMsg (Highlighter.Msg String)
    | ClearHighlights


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControls settings ->
            let
                highlighterState =
                    initHighlighter (Control.currentValue settings) state.highlighter.highlightables
                        |> Tuple.second
            in
            ( { state
                | settings = settings
                , highlighter = highlighterState
              }
            , initHighlighterPort highlighterState Interactive
            )

        HighlighterMsg highlighterMsg ->
            let
                ( newHighlighter, effect, intent ) =
                    Highlighter.update highlighterMsg state.highlighter
            in
            ( { state | highlighter = newHighlighter }
            , Cmd.batch
                [ Cmd.map HighlighterMsg effect
                , perform intent (isScrollFriendly state.highlighter) Interactive
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
                        maybePreviousMouseDownIndex =
                            highlighterState.mouseDownIndex

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
                    -- when there is an actual change in the highlightable elements.
                    case Highlighter.hasChanged intent of
                        Highlighter.Changed _ ->
                            ( newComment
                            , Cmd.batch
                                [ Cmd.map OverlappingHighlighterMsg effect
                                , perform intent (isScrollFriendly state.overlappingHighlightsState) Interactive
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
                                        , perform intent (isScrollFriendly state.overlappingHighlightsState) Interactive
                                        ]
                                    )

                                Nothing ->
                                    -- User is just hovering around the page
                                    ( { state | overlappingHighlightsState = withAllCommentIds }
                                    , Cmd.batch
                                        [ Cmd.map OverlappingHighlighterMsg effect
                                        , perform intent (isScrollFriendly state.overlappingHighlightsState) Interactive
                                        ]
                                    )

        FoldHighlighterMsg highlighterMsg ->
            let
                ( highlighterModel, highlighterCmd, intent ) =
                    Highlighter.update highlighterMsg state.foldHighlightsState
            in
            ( { state | foldHighlightsState = highlighterModel }
            , Cmd.batch
                [ Cmd.map FoldHighlighterMsg highlighterCmd
                , perform intent (isScrollFriendly state.foldHighlightsState) Interactive
                ]
            )


sorter : Sorter ()
sorter =
    Sort.custom (\() () -> EQ)



-- PORT SETUP


initHighlighterPort : Highlighter.Model marker -> ReadOnly -> Cmd msg
initHighlighterPort model readonly =
    highlighterInit ( model.id, model.scrollFriendly, readonly /= Interactive )


perform : Highlighter.Intent marker -> ScrollFriendly -> ReadOnly -> Cmd msg
perform (Highlighter.Intent intent) scrollFriendly readonly =
    case intent.listenTo of
        Just listenTo ->
            highlighterInit ( listenTo, scrollFriendly == ScrollFriendly, readonly /= Interactive )

        Nothing ->
            Cmd.none



-- SUBSCRIPTIONS


subscriptions : Sub (Highlighter.Msg marker)
subscriptions =
    Sub.batch [ onDocumentUp, onTouch ]


{-| Subscribe to mouseup/touchend events on the document.
-}
onDocumentUp : Sub (Highlighter.Msg marker)
onDocumentUp =
    highlighterTouchPointerRelease <|
        \( id, device ) ->
            case device of
                "mouse" ->
                    Highlighter.Pointer <| Highlighter.Up <| Just id

                "touch" ->
                    Highlighter.Touch <| Highlighter.TouchEnd <| Just id

                _ ->
                    Highlighter.Pointer Highlighter.Ignored


{-| Subscribe to touch events
-}
onTouch : Sub (Highlighter.Msg marker)
onTouch =
    highlighterOnTouchEvent <|
        \( type_, targetId, index ) ->
            Highlighter.Touch <|
                case type_ of
                    "move" ->
                        Highlighter.TouchMove (Just targetId) index

                    "end" ->
                        Highlighter.TouchEnd (Just targetId)

                    "longpress" ->
                        Highlighter.LongPress (Just targetId) index

                    _ ->
                        Highlighter.TouchIgnored


{-| Start listening to events on a highlighter
-}
port highlighterInit : ( String, Bool, Bool ) -> Cmd msg


{-| Listen to mouseup/touchend events on the whole document, to stop highlighting.
-}
port highlighterTouchPointerRelease : (( String, String ) -> msg) -> Sub msg


{-| Listen to touch events, and get the element under the finger.
-}
port highlighterOnTouchEvent : (( String, String, Int ) -> msg) -> Sub msg
