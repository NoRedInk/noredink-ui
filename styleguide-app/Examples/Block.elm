module Examples.Block exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Code
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Nri.Ui.Block.V1 as Block
import Nri.Ui.Heading.V3 as Heading


moduleName : String
moduleName =
    "Block"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Interactions ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        -- TODO: add a useful preview!
        []
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
                , mainType = Nothing
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "view"
                          , code =
                                Code.fromModule moduleName "view"
                                    ++ Code.list (List.map Tuple.first attributes)
                          }
                        ]
                }
            , Heading.h2 [ Heading.plaintext "Customizable example" ]
            , Block.view (List.map Tuple.second attributes)
            ]
    }


{-| -}
type alias State =
    { settings : Control Settings
    }


{-| -}
init : State
init =
    { settings = initControl
    }


type alias Settings =
    List ( String, Block.Attribute Msg )


initControl : Control Settings
initControl =
    ControlExtra.list
        |> ControlExtra.listItem "plaintext"
            (Control.string "Bananas"
                |> Control.map
                    (\str ->
                        ( moduleName ++ ".plaintext \"" ++ str ++ "\""
                        , Block.plaintext str
                        )
                    )
            )


{-| -}
type Msg
    = UpdateSettings (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateSettings newControl ->
            ( { state | settings = newControl }
            , Cmd.none
            )
