module Examples.Highlighter exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Code
import CommonControls
import Css exposing (Color)
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Examples.Colors
import Html.Styled exposing (..)
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
    , subscriptions = \_ -> Sub.none
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
            , Heading.h2 [ Heading.plaintext "Example" ]
            , Highlighter.view state.highlighter
                |> map HighlighterMsg
            , Table.view
                [ Table.string
                    { header = "Description"
                    , value = .description
                    , width = Css.pct 20
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.top ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Example"
                    , view = .example
                    , width = Css.px 150
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.top ]
                    , sort = Nothing
                    }
                ]
                [ { description = "Non-interactive sentence with one word highlighted"
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
                , { description = "Non-interactive sentence with multiple words highlighted separately"
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
                , { description = "Non-interactive sentence with multiple words highlighted & joined"
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
                , { description = "Multiple kinds of highlights without overlaps"
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
    , highlighter =
        initHighlighter (Control.currentValue settings)
            (Highlightable.initFragments Nothing CommonControls.romeoAndJulietQuotation)
    }


initHighlighter : Settings -> List (Highlightable ()) -> Highlighter.Model ()
initHighlighter settings highlightables =
    Highlighter.init
        { id = "example-romeo-and-juliet"
        , highlightables = highlightables
        , marker = settings.tool
        }


type alias Settings =
    { tool : Tool.Tool ()
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "tool"
            (Control.choice
                [ ( "Marker", Control.map Tool.Marker controlMarker )
                , ( "Eraser"
                  , Tool.Eraser
                        { hoverClass = [ Css.opacity (Css.num 0.4) ]
                        , hintClass = [ Css.opacity (Css.num 0.4) ]
                        , startGroupClass = [ Css.opacity (Css.num 0.4) ]
                        , endGroupClass = [ Css.opacity (Css.num 0.4) ]
                        }
                        |> Control.value
                  )
                ]
            )


controlMarker : Control (Tool.MarkerModel ())
controlMarker =
    Control.record
        (\a b c d e ->
            Tool.buildMarker
                { highlightColor = a
                , hoverColor = b
                , hoverHighlightColor = c
                , kind = d
                , name = e
                }
        )
        |> Control.field "highlightColor" backgroundHighlightColors
        |> Control.field "hoverColor" backgroundHighlightColors
        |> Control.field "hoverHighlightColor" backgroundHighlightColors
        |> Control.field "kind" (Control.value ())
        |> Control.field "name" (Control.maybe True (Control.string "Claim"))


backgroundHighlightColors : Control Color
backgroundHighlightColors =
    Examples.Colors.backgroundHighlightColors
        |> List.map (\( name, value, _ ) -> ( name, Control.value value ))
        |> Control.choice


{-| -}
type Msg
    = UpdateControls (Control Settings)
    | HighlighterMsg (Highlighter.Msg ())


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
                ( newHighlighter, _ ) =
                    Highlighter.update highlighterMsg state.highlighter
            in
            ( { state | highlighter = newHighlighter }
            , Cmd.none
            )
