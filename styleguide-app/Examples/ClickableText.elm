module Examples.ClickableText exposing (Msg, State, example, init, update)

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
import Nri.Ui.ClickableText.V2 as ClickableText exposing (Size(..))
import Nri.Ui.Icon.V4 as Icon
import Nri.Ui.Text.V2 as Text


{-| -}
type Msg
    = SetState State


{-| -}
type State
    = State (Control Model)


{-| -}
example :
    (String -> ModuleMessages Msg parentMsg)
    -> State
    -> ModuleExample parentMsg
example unnamedMessages state =
    let
        messages =
            unnamedMessages "ClickableTextExample"
    in
    { filename = "Nri.Ui.ClickableText.V1"
    , category = Buttons
    , content =
        [ viewExamples messages state ]
    }


{-| -}
init : { r | performance : String, lock : String } -> State
init assets =
    Control.record Model
        |> Control.field "label" (Control.string "Clickable Text")
        |> Control.field "icon"
            (Control.maybe False <|
                Control.choice
                    ( "Performance", Control.value (Icon.performance assets) )
                    [ ( "Lock", Control.value (Icon.lock assets) )
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
    }


viewExamples :
    ModuleMessages Msg parentMsg
    -> State
    -> Html parentMsg
viewExamples messages (State control) =
    let
        model =
            Control.currentValue control
    in
    [ Control.view (State >> SetState >> messages.wrapper) control
        |> fromUnstyled
    , buttons messages model
    ]
        |> div []


sizes : List Size
sizes =
    [ Small
    , Medium
    , Large
    ]


buttons :
    ModuleMessages Msg parentMsg
    -> Model
    -> Html parentMsg
buttons messages model =
    let
        linkView size =
            ClickableText.link
                { size = size
                , label = model.label
                , icon = model.icon
                , url = "#"
                }
                []

        buttonView size =
            ClickableText.button
                { size = size
                , onClick = messages.showItWorked (Debug.toString size)
                , label = model.label
                , icon = model.icon
                }

        exampleCell view =
            view
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
        |> (\sizeHeadings -> tr [] (th [] [ td [] [] ] :: sizeHeadings))
    , sizes
        |> List.map (linkView >> exampleCell)
        |> (\linkViews -> tr [] (td [] [ text ".link" ] :: linkViews))
    , sizes
        |> List.map (buttonView >> exampleCell)
        |> (\buttonViews -> tr [] (td [] [ text ".button" ] :: buttonViews))
    ]
        |> table []
