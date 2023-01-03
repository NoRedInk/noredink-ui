module Examples.Pagination exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Code
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Pagination.V1 as Pagination


moduleName : String
moduleName =
    "Pagination"


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
        [-- TODO
        ]
    , view =
        \ellieLinkConfig model ->
            let
                settings =
                    Control.currentValue model.settings
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControls
                , settings = model.settings
                , mainType = Nothing
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode = \_ -> []
                }
            , Heading.h2 [ Heading.plaintext "Example" ]
            , Pagination.view SelectPage
                model.currentPage
                (List.range 1 settings.pages
                    |> List.map (\i -> "#page" ++ String.fromInt i)
                )
            ]
    }


{-| -}
type alias State =
    { currentPage : Int
    , settings : Control Settings
    }


init : State
init =
    { currentPage = 0
    , settings = controlSettings
    }


type alias Settings =
    { pages : Int
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "Page count" (ControlExtra.int 6)


{-| -}
type Msg
    = UpdateControls (Control Settings)
    | SelectPage Int


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        UpdateControls settings ->
            ( { model | settings = settings }, Cmd.none )

        SelectPage i ->
            ( { model | currentPage = i }, Cmd.none )
