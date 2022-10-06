module Examples.Panel exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Code
import CommonControls
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Panel.V1 as Panel


moduleName : String
moduleName =
    "Panel"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Layout ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = []
    , view =
        \ellieLinkConfig state ->
            let
                settings =
                    Control.currentValue state.control

                attributes =
                    List.map Tuple.second settings
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state.control
                , mainType = Just "RootHtml.Html msg"
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \_ ->
                        []
                }
            , Heading.h2 [ Heading.plaintext "Example" ]
            , Panel.view attributes
            ]
    }


{-| -}
type alias State =
    { control : Control (Settings Msg)
    }


init : State
init =
    { control =
        ControlExtra.list
            |> ControlExtra.listItem "header"
                (Control.map
                    (\v ->
                        ( Code.fromModule moduleName "header " ++ Code.string v
                        , Panel.header v
                        )
                    )
                    (Control.string "Header")
                )
    }


type alias Settings msg =
    List ( String, Panel.Attribute msg )


{-| -}
type Msg
    = UpdateControl (Control (Settings Msg))


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControl settings ->
            ( { state | control = settings }, Cmd.none )
