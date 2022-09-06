module Examples.Highlighter exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled.Key as Key
import Category exposing (Category(..))
import Code
import CommonControls
import Css exposing (middle, verticalAlign)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Highlightable.V1 as Highlightable
import Nri.Ui.Highlighter.V1 as Highlighter
import Nri.Ui.HighlighterTool.V1 as Tool
import Nri.Ui.Table.V6 as Table
import Nri.Ui.UiIcon.V1 as UiIcon
import Set exposing (Set)


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
                , toExampleCode = \_ -> []
                }
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
        , rounded = True
        }


claimMarker : Tool.MarkerModel ()
claimMarker =
    Tool.buildMarker
        { highlightColor = Colors.highlightYellow
        , hoverColor = Colors.highlightMagenta
        , hoverHighlightColor = Colors.highlightPurpleDark
        , kind = ()
        , rounded = True
        }


evidenceMarker : Tool.MarkerModel ()
evidenceMarker =
    Tool.buildMarker
        { highlightColor = Colors.highlightCyan
        , hoverColor = Colors.highlightMagenta
        , hoverHighlightColor = Colors.highlightPurpleDark
        , kind = ()
        , rounded = True
        }


reasoningMarker : Tool.MarkerModel ()
reasoningMarker =
    Tool.buildMarker
        { highlightColor = Colors.highlightPurple
        , hoverColor = Colors.highlightMagenta
        , hoverHighlightColor = Colors.highlightPurpleDark
        , kind = ()
        , rounded = True
        }


{-| -}
type alias State =
    { settings : Control Settings
    }


{-| -}
init : State
init =
    { settings = controlSettings
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
    Control.record (\a b c d e -> Tool.buildMarker { highlightColor = a, hoverColor = b, hoverHighlightColor = c, kind = d, rounded = e })
        |> Control.field "highlightColor" (Control.value Colors.highlightPurple)
        |> Control.field "hoverColor" (Control.value Colors.highlightMagenta)
        |> Control.field "hoverHighlightColor" (Control.value Colors.highlightPurpleDark)
        |> Control.field "kind" (Control.value ())
        |> Control.field "rounded" (Control.bool True)


{-| -}
type Msg
    = UpdateControls (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControls newDebugControlsState ->
            ( { state | settings = newDebugControlsState }
            , Cmd.none
            )
