module Examples.BorderlessButton exposing (Msg, State, example, init, update)

{- \
   @docs Msg, State, example, init, update,
-}

import Css exposing (middle, verticalAlign)
import Debug.Control as Control exposing (Control)
import Headings
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, id)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample, ModuleMessages)
import Nri.Ui.AssetPath exposing (Asset)
import Nri.Ui.BorderlessButton.V1 as BorderlessButton exposing (Size(..))
import Nri.Ui.Icon.V4 as Icon
import Nri.Ui.Text.V2 as Text


{-| -}
type Msg
    = SetState State


{-| -}
type State
    = State (Control Model)


{-| -}
type ButtonType
    = Button
    | Link


{-| -}
example :
    { r | teach_assignments_copyWhite_svg : Asset, x : String }
    -> (String -> ModuleMessages Msg parentMsg)
    -> State
    -> ModuleExample parentMsg
example assets unnamedMessages state =
    let
        messages =
            unnamedMessages "BorderlessButtonExample"
    in
    { filename = "Nri.Ui.BorderlessButton.V1"
    , category = Buttons
    , content =
        [ viewExamples assets messages state ]
    }


{-| -}
init : { r | performance : String, lock : String } -> State
init assets =
    Control.record Model
        |> Control.field "label" (Control.string "Borderless Button")
        |> Control.field "icon"
            (Control.maybe False <|
                Control.choice
                    ( "Performance", Control.value (Icon.performance assets) )
                    [ ( "Lock", Control.value (Icon.lock assets) )
                    ]
            )
        |> Control.field "button type"
            (Control.choice
                ( "Nri.Ui.BorderlessButton.V1.button", Control.value Button )
                [ ( "Nri.Ui.BorderlessButton.V1.link", Control.value Link )
                ]
            )
        |> State


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetState newState ->
            ( newState, Cmd.none )



-- INTERNAL


type alias Model =
    { label : String
    , icon : Maybe Icon.IconType
    , buttonType : ButtonType
    }


viewExamples :
    { r | teach_assignments_copyWhite_svg : Asset, x : String }
    -> ModuleMessages Msg parentMsg
    -> State
    -> Html parentMsg
viewExamples assets messages (State control) =
    let
        model =
            Control.currentValue control
    in
    [ Control.view (State >> SetState >> messages.wrapper) control
        |> fromUnstyled
    , buttons assets messages model
    ]
        |> div []


sizes : List Size
sizes =
    [ Small
    , Medium
    , Large
    ]


buttons :
    { r | teach_assignments_copyWhite_svg : Asset }
    -> ModuleMessages Msg parentMsg
    -> Model
    -> Html parentMsg
buttons assets messages model =
    let
        exampleCell size =
            (case model.buttonType of
                Link ->
                    BorderlessButton.link
                        { size = size
                        , label = model.label
                        , icon = model.icon
                        , url = "#"
                        }
                        []

                Button ->
                    BorderlessButton.button
                        { size = size
                        , onClick = messages.showItWorked (Debug.toString size)
                        , label = model.label
                        , icon = model.icon
                        }
            )
                |> List.singleton
                |> td
                    [ css
                        [ verticalAlign middle
                        , Css.width (Css.px 200)
                        ]
                    ]
    in
    [ sizes
        |> List.map (\size -> th [] [ text <| Debug.toString size ])
        |> tr []
    , sizes
        |> List.map exampleCell
        |> tr []
    ]
        |> table []
