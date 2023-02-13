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
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.HighlighterToolbar.V1 as HighlighterToolbar
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon


moduleName : String
moduleName =
    "HighlighterToolbar"


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
        [ div [ css [ Css.displayFlex, Css.justifyContent Css.spaceBetween ] ]
            [ highlighterPreview Colors.mustard
            , highlighterPreview Colors.magenta
            , highlighterPreview Colors.cyan
            , toolPreview Colors.white Colors.gray75 UiIcon.eraser
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
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode = \_ -> []
                }
            , Heading.h2 [ Heading.plaintext "Example" ]
            , HighlighterToolbar.view
                { onSetEraser = SetTool Nothing
                , onChangeTag = SetTool << Just
                , getColor = getColor
                , getName = getName
                }
                { currentTool = state.currentTool
                , tags = tags
                }
            ]
    , categories = [ Buttons, Interactions ]
    , keyboardSupport = []
    }


highlighterPreview : Color -> Html msg
highlighterPreview color =
    toolPreview color color (Svg.withColor Colors.white UiIcon.highlighter)


toolPreview : Color -> Color -> Svg.Svg -> Html msg
toolPreview color border icon =
    span
        [ css
            [ Css.backgroundColor color
            , Css.width (Css.px 35)
            , Css.height (Css.px 35)
            , Css.borderRadius (Css.pct 50)
            , Css.padding (Css.px 6)
            , Css.border3 (Css.px 1) Css.solid border
            ]
        ]
        [ Svg.toHtml icon
        ]


type Tag
    = Claim
    | Evidence
    | Reasoning


tags : List Tag
tags =
    [ Claim, Evidence, Reasoning ]


getName : Tag -> String
getName tag =
    case tag of
        Claim ->
            "Claim"

        Evidence ->
            "Evidence"

        Reasoning ->
            "Reasoning"


getColor : Tag -> { colorSolid : Color, colorLight : Color }
getColor tag =
    case tag of
        Claim ->
            { colorSolid = Colors.mustard
            , colorLight = Colors.highlightYellow
            }

        Evidence ->
            { colorSolid = Colors.magenta
            , colorLight = Colors.highlightMagenta
            }

        Reasoning ->
            { colorSolid = Colors.cyan
            , colorLight = Colors.highlightCyan
            }


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
    | SetTool (Maybe Tag)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControls settings ->
            ( { state | settings = settings }, Cmd.none )

        SetTool tag ->
            ( { state | currentTool = tag }, Cmd.none )
