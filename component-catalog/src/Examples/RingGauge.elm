module Examples.RingGauge exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
import IconExamples
import Nri.Ui.Colors.Extra exposing (fromCssColor)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.RingGauge.V1 as RingGauge
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Table.V7 as Table
import Round
import SolidColor.Accessibility


moduleName : String
moduleName =
    "RingGauge"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , init = controlSettings
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Progress, Icons ]
    , keyboardSupport = []
    , preview =
        [ 25, 50, 75, 99 ]
            |> List.map
                (\percentage ->
                    RingGauge.view
                        { backgroundColor = Colors.gray96
                        , emptyColor = Colors.gray96
                        , filledColor = Colors.gray45
                        , percentage = percentage
                        }
                )
            |> IconExamples.preview
    , about = []
    , view =
        \ellieLinkConfig state ->
            let
                settings =
                    Control.currentValue state
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state
                , mainType = Just "RootHtml.Html msg"
                , extraCode = [ "import Nri.Ui.Colors.V1 as Colors" ]
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "Example"
                          , code =
                                Code.pipelineMultiline
                                    [ "RingGauge.view"
                                        ++ Code.record
                                            [ ( "backgroundColor", Tuple.first settings.backgroundColor )
                                            , ( "emptyColor", Tuple.first settings.emptyColor )
                                            , ( "filledColor", Tuple.first settings.filledColor )
                                            , ( "percentage", String.fromFloat settings.percentage )
                                            ]
                                    , "Svg.withWidth (Css.px 200)"
                                    , "Svg.withHeight (Css.px 200)"
                                    , "Svg.toHtml"
                                    ]
                                    0
                          }
                        ]
                }
            , Heading.h2
                [ Heading.plaintext "Customizable example"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , RingGauge.view
                { backgroundColor = Tuple.second settings.backgroundColor
                , emptyColor = Tuple.second settings.emptyColor
                , filledColor = Tuple.second settings.filledColor
                , percentage = settings.percentage
                }
                |> Svg.withWidth (Css.px 200)
                |> Svg.withHeight (Css.px 200)
                |> Svg.toHtml
            , Table.view []
                [ Table.string
                    { header = "Color contrast against"
                    , value = .name
                    , width = Css.px 50
                    , cellStyles = always []
                    , sort = Nothing
                    }
                , Table.string
                    { header = "backgroundColor"
                    , value = .value >> contrast settings.backgroundColor >> Round.floor 2
                    , width = Css.px 50
                    , cellStyles = always []
                    , sort = Nothing
                    }
                , Table.string
                    { header = "emptyColor"
                    , value = .value >> contrast settings.emptyColor >> Round.floor 2
                    , width = Css.px 50
                    , cellStyles = always []
                    , sort = Nothing
                    }
                , Table.string
                    { header = "filledColor"
                    , value = .value >> contrast settings.filledColor >> Round.floor 2
                    , width = Css.px 50
                    , cellStyles = always []
                    , sort = Nothing
                    }
                ]
                [ { name = "backgroundColor", value = settings.backgroundColor }
                , { name = "emptyColor", value = settings.emptyColor }
                , { name = "filledColor", value = settings.filledColor }
                ]
            ]
    }


contrast : ( a, Css.Color ) -> ( a, Css.Color ) -> Float
contrast ( _, a ) ( _, b ) =
    SolidColor.Accessibility.contrast (fromCssColor a) (fromCssColor b)


{-| -}
type Msg
    = UpdateControl (Control Settings)


update : Msg -> State -> ( State, Cmd msg )
update msg state =
    case msg of
        UpdateControl control ->
            ( control, Cmd.none )


{-| -}
type alias State =
    Control Settings


type alias Settings =
    { backgroundColor : ( String, Css.Color )
    , emptyColor : ( String, Css.Color )
    , filledColor : ( String, Css.Color )
    , percentage : Float
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "backgroundColor" (CommonControls.specificColor "white")
        |> Control.field "emptyColor" (CommonControls.specificColor "gray92")
        |> Control.field "filledColor" (CommonControls.specificColor "cornflower")
        |> Control.field "percentage" (Control.float 15)
