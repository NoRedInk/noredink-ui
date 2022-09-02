module Examples.RingGauge exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled exposing (..)
import Category exposing (Category(..))
import Code
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.RingGauge.V1 as RingGauge


{-| -}
type alias State =
    Control Settings


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
    , state = controlSettings
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
    , view =
        \ellieLinkConfig state ->
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state
                , mainType = Just "RootHtml.Html msg"
                , extraCode = []
                , toExampleCode = \_ -> []
                }
            , Heading.h2 [ Heading.plaintext "Example" ]
            ]
    }


{-| -}
type Msg
    = UpdateControl (Control Settings)


update : Msg -> State -> ( State, Cmd msg )
update msg state =
    case msg of
        UpdateControl control ->
            ( control, Cmd.none )


type alias Settings =
    {}


controlSettings : Control Settings
controlSettings =
    Control.record Settings
