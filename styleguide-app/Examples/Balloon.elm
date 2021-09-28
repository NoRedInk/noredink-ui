module Examples.Balloon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import Html.Styled exposing (Html, div, fromUnstyled, text)
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Balloon.V1 as Balloon
import Nri.Ui.Colors.V1 as Colors


{-| -}
example : Example State Msg
example =
    { name = "Balloon"
    , version = 1
    , categories = [ Messaging ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }


{-| -}
type alias State =
    Control Settings


type alias Settings =
    { copy : String
    , theme : Maybe ( String, Balloon.Attribute Css.Px Css.Px Css.Px )
    , position : Maybe ( String, Balloon.Attribute Css.Px Css.Px Css.Px )
    }


init : Control Settings
init =
    Control.record Settings
        |> Control.field "copy" (Control.string "Hello, world!")
        |> Control.field "theme" (Control.maybe False themeOptions)
        |> Control.field "position" (Control.maybe False positionOptions)


themeOptions : Control ( String, Balloon.Attribute width padding paddingUnits )
themeOptions =
    Control.choice
        [ ( "green", Control.value ( "Balloon.green", Balloon.green ) )
        , ( "purple", Control.value ( "Balloon.purple", Balloon.purple ) )
        , ( "orange", Control.value ( "Balloon.orange", Balloon.orange ) )
        , ( "white", Control.value ( "Balloon.white", Balloon.white ) )
        , ( "navy", Control.value ( "Balloon.navy", Balloon.navy ) )
        ]


positionOptions : Control ( String, Balloon.Attribute width padding paddingUnits )
positionOptions =
    Control.choice
        [ ( "onBottom", Control.value ( "Balloon.onBottom", Balloon.onBottom ) )
        , ( "onLeft", Control.value ( "Balloon.onLeft", Balloon.onLeft ) )
        , ( "onRight", Control.value ( "Balloon.onRight", Balloon.onRight ) )
        , ( "onTop", Control.value ( "Balloon.onTop", Balloon.onTop ) )
        ]


{-| -}
type Msg
    = SetDebugControlsState (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetDebugControlsState newDebugControlsState ->
            ( newDebugControlsState
            , Cmd.none
            )


view : State -> List (Html Msg)
view state =
    let
        settings =
            Control.currentValue state

        attributes =
            List.filterMap identity [ settings.theme, settings.position ]
    in
    [ Control.view SetDebugControlsState state |> fromUnstyled
    , Html.Styled.code [ css [ Css.display Css.block ] ]
        [ text <|
            "Balloon.balloon [ "
                ++ String.join ", " (List.map Tuple.first attributes)
                ++ " ] "
                ++ "\""
                ++ settings.copy
                ++ "\""
        ]
    , Balloon.balloon (List.map Tuple.second attributes) (text settings.copy)
    ]
