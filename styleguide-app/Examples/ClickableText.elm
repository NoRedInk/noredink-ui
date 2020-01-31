module Examples.ClickableText exposing (Msg, State, example, init, update)

{-|

@docs Msg, State, example, init, update

-}

import Css exposing (middle, verticalAlign)
import Debug.Control as Control exposing (Control)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, id)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample, ModuleMessages)
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Text.V4 as Text
import Nri.Ui.UiIcon.V1 as UiIcon


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
    { name = "Nri.Ui.ClickableText.V3"
    , category = Buttons
    , content =
        [ viewExamples messages state ]
    }


{-| -}
init : State
init =
    Control.record Model
        |> Control.field "label" (Control.string "Clickable Text")
        |> Control.field "icon"
            (Control.maybe True <|
                Control.choice
                    [ ( "Preview", Control.value UiIcon.preview )
                    , ( "Performance", Control.value UiIcon.performance )
                    , ( "Edit", Control.value UiIcon.edit )
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
        , ClickableText.link model.label
            [ ClickableText.small
            , Maybe.map ClickableText.icon model.icon
                |> Maybe.withDefault (ClickableText.custom [])
            ]
        , text " and clickable buttons: "
        , ClickableText.button model.label
            [ ClickableText.small
            , ClickableText.onClick (messages.showItWorked "in-line button")
            , Maybe.map ClickableText.icon model.icon
                |> Maybe.withDefault (ClickableText.custom [])
            ]
        , text " to show up in-line."
        ]
    ]
        |> div []


sizes : List ( ClickableText.Attribute msg, String )
sizes =
    [ ( ClickableText.small, "small" )
    , ( ClickableText.medium, "medium" )
    , ( ClickableText.large, "large" )
    ]


buttons :
    ModuleMessages Msg parentMsg
    -> Model
    -> Html parentMsg
buttons messages model =
    let
        sizeRow label render =
            row label (List.map render sizes)
    in
    table []
        [ sizeRow "" (\( size, sizeLabel ) -> th [] [ text sizeLabel ])
        , sizeRow ".link"
            (\( size, sizeLabel ) ->
                ClickableText.link model.label
                    [ size
                    , Maybe.map ClickableText.icon model.icon
                        |> Maybe.withDefault (ClickableText.custom [])
                    ]
                    |> exampleCell
            )
        , sizeRow ".button"
            (\( size, sizeLabel ) ->
                ClickableText.button model.label
                    [ size
                    , ClickableText.onClick (messages.showItWorked sizeLabel)
                    , Maybe.map ClickableText.icon model.icon
                        |> Maybe.withDefault (ClickableText.custom [])
                    ]
                    |> exampleCell
            )
        ]


row : String -> List (Html msg) -> Html msg
row label tds =
    tr [] (th [] [ td [] [ text label ] ] :: tds)


exampleCell : Html msg -> Html msg
exampleCell view =
    td [ css [ verticalAlign middle, Css.width (Css.px 200) ] ] [ view ]
