module Examples.ClickableText exposing (Msg, State, example, init, update)

{-|

@docs Msg, State, example, init, update

-}

import Css exposing (middle, verticalAlign)
import Debug.Control as Control exposing (Control)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, id)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample, ModuleMessages)
import Nri.Ui.ClickableText.V2 as ClickableText exposing (Size(..))
import Nri.Ui.Icon.V5 as Icon
import Nri.Ui.Svg.V1 as NriSvg exposing (Svg)
import Nri.Ui.Text.V4 as Text


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
    { name = "Nri.Ui.ClickableText.V2"
    , category = Buttons
    , content =
        [ viewExamples messages state ]
    }


{-| -}
init : { r | performance : String, lock : String, help : String } -> State
init assets =
    Control.record Model
        |> Control.field "label" (Control.string "Clickable Text")
        |> Control.field "icon"
            (Control.maybe True <|
                Control.choice
                    ( "Help"
                    , Icon.helpSvg assets
                        |> Icon.decorativeIcon
                        |> NriSvg.fromHtml
                        |> Control.value
                    )
                    [ ( "Performance"
                      , Icon.performance assets
                            |> Icon.decorativeIcon
                            |> NriSvg.fromHtml
                            |> Control.value
                      )
                    , ( "Lock"
                      , Icon.lock assets
                            |> Icon.decorativeIcon
                            |> NriSvg.fromHtml
                            |> Control.value
                      )
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
    , icon : Maybe Svg
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
    , Text.smallBody
        [ text "Sometimes, we'll want our clickable links: "
        , linkView model Small
        , text " and clickable buttons: "
        , buttonView messages model Small
        , text " to show up in-line."
        ]
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
        |> List.map (linkView model >> exampleCell)
        |> (\linkViews -> tr [] (td [] [ text ".link" ] :: linkViews))
    , sizes
        |> List.map (buttonView messages model >> exampleCell)
        |> (\buttonViews -> tr [] (td [] [ text ".button" ] :: buttonViews))
    ]
        |> table []


linkView : Model -> ClickableText.Size -> Html msg
linkView model size =
    ClickableText.link
        { size = size
        , label = model.label
        , icon = model.icon
        , url = "#"
        }
        []


buttonView : ModuleMessages Msg parentMsg -> Model -> ClickableText.Size -> Html parentMsg
buttonView messages model size =
    ClickableText.button
        { size = size
        , onClick = messages.showItWorked (Debug.toString size)
        , label = model.label
        , icon = model.icon
        }
