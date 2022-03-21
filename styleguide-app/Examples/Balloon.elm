module Examples.Balloon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled exposing (Html, fromUnstyled, text)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Balloon.V1 as Balloon


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
    { copy : Control String
    , attributes : Control (List ( String, Balloon.Attribute ))
    }


init : State
init =
    { copy = Control.string "Hello, world!"
    , attributes = controlAttributes
    }


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
    = SetCopy (Control String)
    | SetAttributes (Control (List ( String, Balloon.Attribute )))


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetCopy copy ->
            ( { state | copy = copy }
            , Cmd.none
            )

        SetAttributes attributes ->
            ( { state | attributes = attributes }
            , Cmd.none
            )


view : State -> List (Html Msg)
view state =
    let
        copy =
            Control.currentValue state.copy
    in
    [ Control.view SetCopy state.copy |> fromUnstyled
    , ControlView.view
        { update = SetAttributes
        , settings = state.attributes
        , toExampleCode =
            \attrs ->
                [ { sectionName = "Balloon"
                  , code =
                        "Balloon.balloon [ "
                            ++ String.join ", " (List.map Tuple.first attrs)
                            ++ " ] "
                            ++ "(text \""
                            ++ copy
                            ++ "\")"
                  }
                ]
        }
    , Balloon.balloon
        (List.map Tuple.second (Control.currentValue state.attributes))
        (text copy)
    ]
