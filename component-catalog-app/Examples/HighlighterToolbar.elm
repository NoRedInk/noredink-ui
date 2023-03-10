module Examples.HighlighterToolbar exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Browser.Dom as Dom
import Category exposing (Category(..))
import Code
import Css exposing (Color)
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, id)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.HighlighterToolbar.V2 as HighlighterToolbar
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V6 as Text
import Nri.Ui.UiIcon.V1 as UiIcon
import Task


moduleName : String
moduleName =
    "HighlighterToolbar"


version : Int
version =
    2


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
                { focusAndSelect = FocusAndSelectTag
                , getColor = getColor
                , getName = getName
                , highlighterId = "highlighter"
                }
                { currentTool = state.currentTool
                , tags = tags
                }
            , div [ id "highlighter" ] []
            ]
    , categories = [ Instructional ]
    , keyboardSupport = []
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
    | FocusAndSelectTag { select : Maybe Tag, focus : Maybe String }
    | Focused (Result Dom.Error ())


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControls settings ->
            ( { state | settings = settings }, Cmd.none )

        FocusAndSelectTag { select, focus } ->
            ( { state | currentTool = select }
            , focus
                |> Maybe.map (Dom.focus >> Task.attempt Focused)
                |> Maybe.withDefault Cmd.none
            )

        Focused error ->
            ( state, Cmd.none )
