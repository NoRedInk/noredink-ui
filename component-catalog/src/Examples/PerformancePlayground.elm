module Examples.PerformancePlayground exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Code
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Html.Styled exposing (..)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Text.V6 as Text


moduleName : String
moduleName =
    "PerformancePlayground"


version : Int
version =
    1


example : Example State Msg
example =
    { name = moduleName
    , version = version
    , init = ( init, Cmd.none )
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ Text.mediumBody [ Text.plaintext "Performance playground" ]
        ]
    , about =
        [ Text.mediumBody [ Text.plaintext "Page to check performance when rendering inline css" ]
        ]
    , view = view
    , categories =
        []
    , keyboardSupport =
        []
    }


view : EllieLink.Config -> State -> List (Html Msg)
view ellieLinkConfig state =
    let
        currentValue =
            Control.currentValue state
    in
    [ ControlView.view
        { ellieLinkConfig = ellieLinkConfig
        , name = moduleName
        , version = version
        , update = UpdateSettings
        , settings = state
        , mainType = Nothing
        , extraCode = []
        , renderExample = Code.unstyledView
        , toExampleCode = \_ -> []
        }
    , Heading.h2
        [ Heading.plaintext "Playground"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , div []
        (List.repeat currentValue.buttonCount ()
            |> List.indexedMap
                (\i _ ->
                    Button.button ("A button " ++ String.fromInt i) []
                )
        )
    ]


type alias Config =
    { buttonCount : Int
    }


{-| -}
type alias State =
    Control Config


init : State
init =
    Control.record Config
        |> Control.field "button count" (Control.int 0)


{-| -}
type Msg
    = UpdateSettings State


update : Msg -> State -> ( State, Cmd Msg )
update msg _ =
    case msg of
        UpdateSettings newState ->
            ( newState
            , Cmd.none
            )
