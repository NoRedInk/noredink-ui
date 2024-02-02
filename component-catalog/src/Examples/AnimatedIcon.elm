module Examples.AnimatedIcon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Code
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled exposing (..)
import IconExamples as IconExamples
import Nri.Ui.AnimatedIcon.V1 as AnimatedIcon
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Table.V7 as Table


moduleName : String
moduleName =
    "AnimatedIcon"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Animations, Icons ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        IconExamples.preview
            [ AnimatedIcon.mobileOpenClose False
            , AnimatedIcon.mobileOpenClose True
            , AnimatedIcon.arrowRightDown False
            , AnimatedIcon.arrowRightDown True
            ]
    , about = []
    , view =
        \ellieLinkConfig state ->
            let
                attributes =
                    Control.currentValue state.settings
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateSettings
                , settings = state.settings
                , mainType = Just "RootHtml.Html msg"
                , extraCode = [ "import Nri.Ui.Svg.V1 as Svg" ]
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \settings ->
                        let
                            toCode viewName =
                                Code.pipelineMultiline
                                    [ Code.fromModule moduleName viewName
                                        ++ " "
                                        ++ Tuple.first settings.isOpen
                                    , "Svg.withCss [ Css.maxWidth (Css.px 30) ]"
                                    , "Svg.toHtml"
                                    ]
                                    0
                        in
                        [ { sectionName = "mobileOpenClose"
                          , code = toCode "mobileOpenClose"
                          }
                        , { sectionName = "arrowRightDown"
                          , code = toCode "arrowRightDown"
                          }
                        , { sectionName = "arrowDownUp"
                          , code = toCode "arrowDownUp"
                          }
                        ]
                }
            , Heading.h2
                [ Heading.plaintext "Examples"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , Table.view []
                [ Table.custom
                    { header = text "Rendered"
                    , view =
                        \{ render } ->
                            render (Tuple.second attributes.isOpen)
                                |> Svg.withCss
                                    [ Css.maxWidth (Css.px 30)
                                    , Css.border3 (Css.px 2) Css.solid Colors.red
                                    , Css.boxSizing Css.borderBox
                                    ]
                                |> Svg.toHtml
                    , width = Css.px 10
                    , cellStyles = always []
                    , sort = Nothing
                    }
                , Table.string
                    { header = "Name"
                    , value = .name
                    , width = Css.px 10
                    , cellStyles = always []
                    , sort = Nothing
                    }
                ]
                [ { name = "mobileOpenClose", render = AnimatedIcon.mobileOpenClose }
                , { name = "arrowRightDown", render = AnimatedIcon.arrowRightDown }
                , { name = "arrowDownUp", render = AnimatedIcon.arrowDownUp }
                ]
            ]
    }


{-| -}
type alias State =
    { settings : Control Settings
    }


{-| -}
init : State
init =
    { settings = initSettings
    }


type alias Settings =
    { isOpen : ( String, Bool )
    }


initSettings : Control Settings
initSettings =
    Control.record Settings
        |> Control.field "isOpen" (ControlExtra.bool False)


{-| -}
type Msg
    = UpdateSettings (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateSettings settings ->
            ( { state | settings = settings }, Cmd.none )
