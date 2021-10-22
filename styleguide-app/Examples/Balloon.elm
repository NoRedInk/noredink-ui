module Examples.Balloon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
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
    , theme : Maybe ( String, Balloon.Attribute )
    , position : Maybe ( String, Balloon.Attribute )
    , width : Maybe ( String, Balloon.Attribute )
    , padding : Maybe ( String, Balloon.Attribute )
    }


init : Control Settings
init =
    Control.record Settings
        |> Control.field "copy" (Control.string "Hello, world!")
        |> Control.field "theme" (Control.maybe False themeOptions)
        |> Control.field "position" (Control.maybe False positionOptions)
        |> Control.field "width" (Control.maybe False widthOptions)
        |> Control.field "padding" (Control.maybe False paddingOptions)


themeOptions : Control ( String, Balloon.Attribute )
themeOptions =
    Control.choice
        [ ( "green", Control.value ( "Balloon.green", Balloon.green ) )
        , ( "purple", Control.value ( "Balloon.purple", Balloon.purple ) )
        , ( "orange", Control.value ( "Balloon.orange", Balloon.orange ) )
        , ( "white", Control.value ( "Balloon.white", Balloon.white ) )
        , ( "navy", Control.value ( "Balloon.navy", Balloon.navy ) )
        ]


positionOptions : Control ( String, Balloon.Attribute )
positionOptions =
    Control.choice
        [ ( "onBottom", Control.value ( "Balloon.onBottom", Balloon.onBottom ) )
        , ( "onLeft", Control.value ( "Balloon.onLeft", Balloon.onLeft ) )
        , ( "onRight", Control.value ( "Balloon.onRight", Balloon.onRight ) )
        , ( "onTop", Control.value ( "Balloon.onTop", Balloon.onTop ) )
        ]


widthOptions : Control ( String, Balloon.Attribute )
widthOptions =
    Control.choice
        [ ( "px"
          , Control.map
                (\w -> ( "Balloon.widthPx " ++ String.fromFloat w, Balloon.widthPx w ))
                (ControlExtra.float 50)
          )
        , ( "%"
          , Control.map
                (\w -> ( "Balloon.widthPct " ++ String.fromFloat w, Balloon.widthPct w ))
                (ControlExtra.float 50)
          )
        ]


paddingOptions : Control ( String, Balloon.Attribute )
paddingOptions =
    Control.map
        (\w -> ( "Balloon.paddingPx " ++ String.fromFloat w, Balloon.paddingPx w ))
        (ControlExtra.float 10)


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
            List.filterMap identity
                [ settings.theme
                , settings.position
                , settings.width
                , settings.padding
                ]
    in
    [ Control.view SetDebugControlsState state |> fromUnstyled
    , Html.Styled.code [ css [ Css.display Css.block, Css.margin2 (Css.px 20) Css.zero ] ]
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
