module Examples.Balloon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Html.Styled exposing (Html, text)
import Nri.Ui.Balloon.V1 as Balloon


moduleName : String
moduleName =
    "Balloon"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Messaging ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ Balloon.balloon
            [ Balloon.onTop
            , Balloon.navy
            , Balloon.paddingPx 15
            ]
            (text "This is a balloon.")
        ]
    , view = view
    }


{-| -}
type alias State =
    Control Settings


init : State
init =
    controlSettings


type alias Settings =
    { copy : String
    , attributes : List ( String, Balloon.Attribute )
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "copy" (Control.string "Hello, world!")
        |> Control.field "attributes" controlAttributes


controlAttributes : Control (List ( String, Balloon.Attribute ))
controlAttributes =
    ControlExtra.list
        |> ControlExtra.optionalListItem "theme" themeOptions
        |> ControlExtra.optionalListItem "position" positionOptions
        |> ControlExtra.optionalListItem "width" widthOptions
        |> ControlExtra.optionalListItem "padding" paddingOptions


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
    = SetAttributes (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetAttributes attributes ->
            ( attributes
            , Cmd.none
            )


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
        , update = SetAttributes
        , settings = state
        , mainType = "RootHtml.Html msg"
        , extraCode = []
        , toExampleCode =
            \{ copy, attributes } ->
                [ { sectionName = "Balloon"
                  , code =
                        "Balloon.balloon\n    [ "
                            ++ String.join "\n    , " (List.map Tuple.first attributes)
                            ++ "\n    ] "
                            ++ "\n    (text \""
                            ++ copy
                            ++ "\")"
                  }
                ]
        }
    , Balloon.balloon
        (List.map Tuple.second currentValue.attributes)
        (text currentValue.copy)
    ]
