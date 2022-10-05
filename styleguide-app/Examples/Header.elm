module Examples.Header exposing (example, State, Msg)

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
import Nri.Ui.Header.V1 as Header
import ViewHelpers exposing (viewExamples)


moduleName : String
moduleName =
    "Header"


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
                examples =
                    []

                attributes =
                    List.map Tuple.second (Control.currentValue state.control)
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
                    \settings ->
                        let
                            toExampleCode ( name, _ ) =
                                { sectionName = name
                                , code =
                                    moduleName
                                        ++ "."
                                        ++ name
                                        ++ "\n    [ "
                                        ++ String.join "\n    , " (List.map Tuple.first settings)
                                        ++ "\n    ]"
                                }
                        in
                        List.map toExampleCode examples
                }
            , examples
                |> List.map (\( name, view ) -> ( name, view attributes ))
                |> viewExamples
            ]
    }


{-| -}
type alias State =
    { control : Control Settings
    }


init : State
init =
    { control = ControlExtra.list
    }


type alias Settings =
    List ( String, Header.Attribute () Msg )


{-| -}
type Msg
    = UpdateControl (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControl settings ->
            ( { state | control = settings }, Cmd.none )
