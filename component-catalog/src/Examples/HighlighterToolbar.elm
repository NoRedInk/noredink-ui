module Examples.HighlighterToolbar exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Code
import Css exposing (Color)
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, id)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.HighlighterToolbar.V3 as HighlighterToolbar
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V6 as Text
import Nri.Ui.UiIcon.V1 as UiIcon


moduleName : String
moduleName =
    "HighlighterToolbar"


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
    , subscriptions = \_ -> Sub.none
    , preview =
        [ highlighterPreview Colors.mustard "Claim"
        , highlighterPreview Colors.magenta "Evidence"
        , highlighterPreview Colors.cyan "Reasoning"
        , toolPreview Colors.white Colors.gray75 UiIcon.eraser "Remove highlight"
        ]
    , about = []
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
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "Example"
                          , code =
                                Code.fromModule moduleName "view"
                                    ++ Code.recordMultiline
                                        [ ( "onSelect", "identity -- msg for selecting the tag or eraser" )
                                        , ( "getNameAndColor", "identity" )
                                        , ( "highlighterId", Code.string "highlighter-id" )
                                        ]
                                        2
                                    ++ Code.recordMultiline
                                        [ ( "currentTool", "Nothing" )
                                        , ( "tags", "[]" )
                                        ]
                                        2
                          }
                        ]
                }
            , Heading.h2 [ Heading.plaintext "Example" ]
            , HighlighterToolbar.view
                { onSelect = SelectTag
                , getNameAndColor = identity
                , highlighterId = "highlighter"
                }
                { currentTool = state.currentTool
                , tags = tags
                }
            , div [ id "highlighter" ] []
            ]
    , categories = [ Instructional ]
    , keyboardSupport =
        [ { keys = [ Arrow KeyboardSupport.Left ]
          , result = "Select the tool to the left of the currently-selected tool. If the first tool is selected, select the last tool."
          }
        , { keys = [ Arrow KeyboardSupport.Right ]
          , result = "Select the tool to the right of the currently-selected tool. If the last tool is selected, select the first tool."
          }
        ]
    }


highlighterPreview : Color -> String -> Html msg
highlighterPreview color =
    toolPreview color color (Svg.withColor Colors.white UiIcon.highlighter)


toolPreview : Color -> Color -> Svg.Svg -> String -> Html msg
toolPreview color border icon name =
    div [ css [ Css.displayFlex, Css.alignItems Css.center, Css.marginBottom (Css.px 4) ] ]
        [ span
            [ css
                [ Css.backgroundColor color
                , Css.width (Css.px 35)
                , Css.height (Css.px 35)
                , Css.borderRadius (Css.pct 50)
                , Css.padding (Css.px 6)
                , Css.border3 (Css.px 1) Css.solid border
                , Css.marginRight (Css.px 4)
                ]
            ]
            [ Svg.toHtml icon
            ]
        , Text.smallBody [ Text.plaintext name, Text.css [ Css.color Colors.navy ] ]
        ]


type alias Tag =
    { name : String
    , colorSolid : Css.Color
    , colorLight : Css.Color
    }


tags : List Tag
tags =
    [ { name = "Claim"
      , colorSolid = Colors.mustard
      , colorLight = Colors.highlightYellow
      }
    , { name = "Evidence"
      , colorSolid = Colors.magenta
      , colorLight = Colors.highlightMagenta
      }
    , { name = "Reasoning"
      , colorSolid = Colors.cyan
      , colorLight = Colors.highlightCyan
      }
    ]


{-| -}
type alias State =
    { settings : Control Settings
    , currentTool : Maybe Tag
    }


{-| -}
init : State
init =
    let
        settings =
            controlSettings
    in
    { settings = settings
    , currentTool = Nothing
    }


type alias Settings =
    {}


controlSettings : Control Settings
controlSettings =
    Control.record Settings


{-| -}
type Msg
    = UpdateControls (Control Settings)
    | SelectTag (Maybe Tag)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControls settings ->
            ( { state | settings = settings }, Cmd.none )

        SelectTag newTool ->
            ( { state | currentTool = newTool }
            , Cmd.none
            )
