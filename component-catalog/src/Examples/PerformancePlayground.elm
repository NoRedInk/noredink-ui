module Examples.PerformancePlayground exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled.Key as Key
import Category
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Html.Styled exposing (..)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Table.V8 as Table
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
            Control.currentValue state.count
    in
    [ ControlView.view
        { ellieLinkConfig = ellieLinkConfig
        , name = moduleName
        , version = version
        , update = UpdateSettings
        , settings = state.count
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
        [ text (String.fromInt currentValue)
        ]
    ]


{-| -}
type alias State =
    { count : Control Int
    }


init : State
init =
    { count = Control.int 0
    }


{-| -}
type Msg
    = UpdateSettings (Control Int)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateSettings count ->
            ( { state | count = count }
            , Cmd.none
            )

