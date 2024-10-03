module Examples.Confetti exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Accessibility.Styled.Role as Role
import Browser.Events
import Category exposing (Category(..))
import Code
import Css exposing (Color)
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.Extra as ColorsExtra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Confetti.V2 as Confetti
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Html.V3 exposing (viewJust)
import Nri.Ui.Spacing.V1 as Spacing
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as SvgAttrs


moduleName : String
moduleName =
    "Confetti"


version : Int
version =
    2


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Animations ]
    , keyboardSupport = []
    , init = ( init, Cmd.none )
    , update = update
    , subscriptions =
        \state ->
            case state.model of
                Just model ->
                    Sub.batch
                        [ Browser.Events.onResize WindowResized
                        , Confetti.subscriptions ConfettiMsg model
                        ]

                Nothing ->
                    Sub.none
    , preview = preview
    , about = []
    , view =
        \ellieLinkConfig state ->
            [ viewJust Confetti.view state.model
            , ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state.settings
                , mainType = Just "Program () Confetti.Model Msg"
                , extraCode =
                    [ "import Browser"
                    , "import Browser.Events"
                    , Code.newlines
                    , Code.unionType "Msg"
                        [ "ConfettiMsg Confetti.Msg"
                        , "WindowResized Int Int"
                        ]
                    ]
                , renderExample = identity
                , toExampleCode =
                    \settings ->
                        [ { sectionName = "Code"
                          , code =
                                Code.browserElement
                                    { init =
                                        Code.always
                                            (Code.tuple
                                                ("Confetti.burst (Confetti.init " ++ String.fromFloat settings.center ++ ")")
                                                "Cmd.none"
                                            )
                                    , update =
                                        Code.newlineWithIndent 2
                                            ++ Code.anonymousFunction
                                                "msg model"
                                                (Code.caseExpression "msg"
                                                    [ ( "ConfettiMsg confettiMsg", Code.tuple "Confetti.update confettiMsg model" "Cmd.none" )
                                                    , ( "WindowResized width _", Code.tuple "Confetti.updatePageWidth width model" "Cmd.none" )
                                                    ]
                                                    3
                                                )
                                    , view = moduleName ++ ".view >> toUnstyled"
                                    , subscriptions =
                                        Code.newlineWithIndent 2
                                            ++ Code.anonymousFunction "model"
                                                (Code.newlineWithIndent 3
                                                    ++ "Sub.batch "
                                                    ++ Code.listMultiline
                                                        [ "Browser.Events.onResize WindowResized"
                                                        , "Confetti.subscriptions ConfettiMsg model"
                                                        ]
                                                        4
                                                )
                                    }
                          }
                        ]
                }
            , Heading.h2
                [ Heading.plaintext "Example"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , Button.button "Launch confetti!"
                [ Button.onClick LaunchConfetti
                , Button.small
                , Button.secondary
                ]
            ]
    }


preview : List (Svg msg)
preview =
    [ Svg.svg
        [ SvgAttrs.viewBox "0 0 100 100"
        , SvgAttrs.width "100%"
        , SvgAttrs.height "120px"
        , SvgAttrs.fill (ColorsExtra.toCssString Colors.white)
        , Role.img
        ]
        (Svg.rect
            [ SvgAttrs.x "0"
            , SvgAttrs.y "0"
            , SvgAttrs.width "100"
            , SvgAttrs.height "100"
            ]
            []
            :: List.map3
                (\( x, y ) index color ->
                    Svg.rect
                        [ SvgAttrs.x (String.fromInt x)
                        , SvgAttrs.y (String.fromInt y)
                        , SvgAttrs.width "8"
                        , SvgAttrs.height "8"
                        , SvgAttrs.fill (ColorsExtra.toCssString color)
                        , SvgAttrs.transform <|
                            "rotate("
                                ++ ((if modBy 3 index == 0 then
                                        "45"

                                     else if modBy 2 index == 0 then
                                        "20"

                                     else
                                        "0"
                                    )
                                        ++ ","
                                        ++ String.fromInt x
                                        ++ ","
                                        ++ String.fromInt y
                                        ++ ")"
                                   )
                        ]
                        []
                )
                confettiPositions
                (List.range 0 10)
                confettiColors
        )
    ]


confettiPositions : List ( Int, Int )
confettiPositions =
    [ ( 40, 10 )
    , ( 7, 15 )
    , ( 80, 30 )
    , ( 4, 80 )
    , ( 23, 70 )
    , ( 50, 69 )
    , ( 19, 52 )
    , ( 70, 90 )
    , ( 60, 90 )
    , ( 94, 60 )
    , ( 80, 5 )
    ]


confettiColors : List Color
confettiColors =
    [ Colors.highlightMagentaDark
    , Colors.highlightCyan
    , Colors.highlightGreen
    , Colors.highlightYellowDark
    , Colors.highlightBlue
    , Colors.highlightGreenDark
    , Colors.highlightBlueDark
    , Colors.highlightYellow
    , Colors.highlightCyanDark
    , Colors.highlightCyan
    , Colors.highlightMagenta
    ]


{-| -}
type alias State =
    { settings : Control Settings
    , model : Maybe Confetti.Model
    }


init : State
init =
    { settings = initSettings
    , model = Nothing
    }


type alias Settings =
    { center : Float
    }


initSettings : Control Settings
initSettings =
    Control.record Settings
        |> Control.field "center" (Control.float 700)


{-| -}
type Msg
    = LaunchConfetti
    | ConfettiMsg Confetti.Msg
    | WindowResized Int Int
    | UpdateControl (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    ( case msg of
        LaunchConfetti ->
            { state | model = Just (Confetti.burst (Confetti.init (Control.currentValue state.settings).center)) }

        ConfettiMsg confettiMsg ->
            { state | model = Maybe.map (Confetti.update confettiMsg) state.model }

        WindowResized width _ ->
            { state | model = Maybe.map (Confetti.updatePageWidth width) state.model }

        UpdateControl newControl ->
            { state | settings = newControl }
    , Cmd.none
    )
