module Examples.Outline exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Outline.V1 as Outline


moduleName : String
moduleName =
    "Outline"


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
    , preview =
        []
    , about = []
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
                        [ { sectionName = "Example"
                          , code =
                                Code.fromModule moduleName "view"
                                    ++ Code.list (List.map Tuple.first settings)
                          }
                        ]
                }
            , Heading.h2
                [ Heading.plaintext "Example"
                , Heading.css [ Css.marginTop (Css.px 30) ]
                ]
            , Outline.view []
            ]
    }


{-| -}
type alias State =
    { control : Control Settings
    }


init : State
init =
    { control =
        ControlExtra.list
    }


type alias Settings =
    List ( String, () )


{-| -}
type Msg
    = UpdateControl (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControl settings ->
            ( { state | control = settings }, Cmd.none )
