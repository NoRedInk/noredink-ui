port module Examples.Highlighter exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Code
import CommonControls
import Css exposing (Color)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Examples.Colors
import Html.Styled exposing (..)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Highlightable.V1 as Highlightable exposing (Highlightable)
import Nri.Ui.Highlighter.V1 as Highlighter
import Nri.Ui.HighlighterTool.V1 as Tool
import Nri.Ui.Table.V6 as Table


moduleName : String
moduleName =
    "Highlighter"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.map HighlighterMsg subscriptions
    , preview =
        []
    , view =
        \ellieLinkConfig state ->
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControls
                , settings = state.settings
                , mainType = Nothing
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode = \_ -> []
                }
            , Heading.h2 [ Heading.plaintext "Interactive example" ]
            , Heading.h3 [ Heading.plaintext "This example updates based on the settings you configure on this page." ]
            , Button.button "Clear all highlights"
                [ Button.onClick ClearHighlights
                , Button.secondary
                , Button.small
                , Button.css [ Css.marginTop (Css.px 10) ]
                ]
            , Highlighter.view state.highlighter
                |> map HighlighterMsg
            , Heading.h2 [ Heading.plaintext "Non-interactive examples" ]
            , Heading.h3 [ Heading.plaintext "These are examples of some different ways the highlighter can appear to users." ]
            , Table.view
                [ Table.string
                    { header = "View"
                    , value = .viewName
                    , width = Css.zero
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
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
                    , sort = Nothing
                    }
                ]
                [ { viewName = "Highlighter.static"
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
                                    |> List.indexedMap (\i ( word, marker ) -> Highlightable.init Highlightable.Static marker i ( [], word ))
                            }
                  }
                , { viewName = "Highlighter.static"
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
                                    |> List.indexedMap (\i ( word, marker ) -> Highlightable.init Highlightable.Static marker i ( [], word ))
                            }
                  }
                , { viewName = "Highlighter.static"
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
                                    |> List.indexedMap (\i ( word, marker ) -> Highlightable.init Highlightable.Static marker i ( [], word ))
                            }
                  }
                , { viewName = "Highlighter.static"
                  , description = "Multiple kinds of highlights without overlaps"
                  , example =
                        Highlighter.static
                            { id = "example-3"
                            , highlightables =
                                [ ( "Waltz, bad nymph, for quick jigs vex.", Just claimMarker )
                                , ( "Glib jocks quiz nymph to vex dwarf.", Just evidenceMarker )
                                , ( "Sphinx of black quartz, judge my vow.", Just reasoningMarker )
                                , ( "How vexingly quick daft zebras jump!", Nothing )
                                ]
                                    |> List.intersperse ( " ", Nothing )
                                    |> List.indexedMap (\i ( word, marker ) -> Highlightable.init Highlightable.Static marker i ( [], word ))
                            }
                  }
                , { viewName = "Highlighter.staticWithTags"
                  , description = "Multiple kinds of highlights without overlaps"
                  , example =
                        Highlighter.staticWithTags
                            { id = "example-3"
                            , highlightables =
                                [ ( "Waltz, bad nymph, for quick jigs vex.", Just claimMarker )
                                , ( "Glib jocks quiz nymph to vex dwarf.", Just evidenceMarker )
                                , ( "Sphinx of black quartz, judge my vow.", Just reasoningMarker )
                                , ( "How vexingly quick daft zebras jump!", Nothing )
                                ]
                                    |> List.intersperse ( " ", Nothing )
                                    |> List.indexedMap (\i ( word, marker ) -> Highlightable.init Highlightable.Static marker i ( [], word ))
                            }
                  }
                ]
            ]
    , categories = [ Text, Interactions ]
    , keyboardSupport = []
    }


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


{-| -}
type alias State =
    { settings : Control Settings
    , highlighter : Highlighter.Model ()
    }


{-| -}
init : State
init =
    let
        settings =
            controlSettings
    in
    { settings = settings
    , highlighter = initHighlighter (Control.currentValue settings) []
    }


initHighlighter : Settings -> List (Highlightable ()) -> Highlighter.Model ()
initHighlighter settings previousHighlightables =
    let
        highlightables =
            if settings.splitOnSentences then
                let
                    segments =
                        List.filter (\x -> x /= "") (String.split "." (String.trim CommonControls.romeoAndJulietQuotation))
                in
                List.indexedMap
                    (\index sentence ->
                        Highlightable.init Highlightable.Interactive
                            Nothing
                            index
                            ( []
                            , sentence ++ "."
                            )
                    )
                    segments

            else
                Highlightable.initFragments Nothing (String.trim CommonControls.romeoAndJulietQuotation)
    in
    Highlighter.init
        { id = "example-romeo-and-juliet"
        , highlightables =
            if List.map .text previousHighlightables == List.map .text highlightables then
                previousHighlightables

            else
                highlightables
        , marker = settings.tool
        }


type alias Settings =
    { splitOnSentences : Bool
    , tool : Tool.Tool ()
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "splitOnSentences" (Control.bool False)
        |> Control.field "tool"
            (Control.choice
                [ ( "Marker", Control.map Tool.Marker controlMarker )
                , ( "Eraser"
                  , Control.map Tool.Eraser controlEraser
                  )
                ]
            )


controlMarker : Control (Tool.MarkerModel ())
controlMarker =
    Control.record
        (\a b c d ->
            Tool.buildMarker
                { highlightColor = a
                , hoverColor = b
                , hoverHighlightColor = c
                , kind = ()
                , name = d
                }
        )
        |> Control.field "highlightColor" (backgroundHighlightColors 0)
        |> Control.field "hoverColor" (backgroundHighlightColors 2)
        |> Control.field "hoverHighlightColor" (backgroundHighlightColors 4)
        |> Control.field "name" (Control.maybe True (Control.string "Claim"))


controlEraser : Control Tool.EraserModel
controlEraser =
    Control.record Tool.EraserModel
        |> Control.field "hoverClass"
            (Control.choice [ ( "[ Css.border3 (Css.px 4) Css.dashed Colors.red ]", Control.value [ Css.border3 (Css.px 4) Css.dashed Colors.red ] ) ])
        |> Control.field "hintClass"
            (Control.choice [ ( "[ Css.border3 (Css.px 4) Css.dotted Colors.orange ]", Control.value [ Css.border3 (Css.px 4) Css.dotted Colors.orange ] ) ])
        |> Control.field "startGroupClass"
            (Control.choice [ ( "[ Css.opacity (Css.num 0.4) ]", Control.value [ Css.opacity (Css.num 0.4) ] ) ])
        |> Control.field "endGroupClass"
            (Control.choice [ ( "[ Css.opacity (Css.num 0.4) ]", Control.value [ Css.opacity (Css.num 0.4) ] ) ])


backgroundHighlightColors : Int -> Control Color
backgroundHighlightColors rotateWith =
    Examples.Colors.backgroundHighlightColors
        |> List.map (\( name, value, _ ) -> ( name, Control.value value ))
        |> ControlExtra.rotatedChoice rotateWith


{-| -}
type Msg
    = UpdateControls (Control Settings)
    | HighlighterMsg (Highlighter.Msg ())
    | ClearHighlights


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControls settings ->
            ( { state
                | settings = settings
                , highlighter = initHighlighter (Control.currentValue settings) state.highlighter.highlightables
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
