module Examples.ClickableText exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Css exposing (middle, verticalAlign)
import Debug.Control as Control exposing (Control)
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, id)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Text.V6 as Text
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
type State
    = State (Control Model)


{-| -}
example : Example State Msg
example =
    { name = "ClickableText"
    , version = 3
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = []
    , view = \state -> [ viewExamples state ]
    , categories = [ Buttons ]
    , keyboardSupport = []
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
type Msg
    = SetState State
    | ShowItWorked String String


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetState newState ->
            ( newState, Cmd.none )

        ShowItWorked group message ->
            let
                _ =
                    Debug.log group message
            in
            ( state, Cmd.none )



-- INTERNAL


type alias Model =
    { label : String
    , icon : Maybe Svg
    }


viewExamples : State -> Html Msg
viewExamples (State control) =
    let
        model =
            Control.currentValue control
    in
    [ Control.view (State >> SetState) control
        |> fromUnstyled
    , buttons model
    , Text.smallBody
        [ Text.html
            [ text "Sometimes, we'll want our clickable links: "
            , ClickableText.link model.label
                [ ClickableText.small
                , Maybe.map ClickableText.icon model.icon
                    |> Maybe.withDefault (ClickableText.custom [])
                ]
            , text " and clickable buttons: "
            , ClickableText.button model.label
                [ ClickableText.small
                , ClickableText.onClick (ShowItWorked "ClickableText" "in-line button")
                , Maybe.map ClickableText.icon model.icon
                    |> Maybe.withDefault (ClickableText.custom [])
                ]
            , text " to show up in-line."
            ]
        ]
    ]
        |> div []


sizes : List ( ClickableText.Attribute msg, String )
sizes =
    [ ( ClickableText.small, "small" )
    , ( ClickableText.medium, "medium" )
    , ( ClickableText.large, "large" )
    ]


buttons : Model -> Html Msg
buttons model =
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
                    , ClickableText.onClick (ShowItWorked "ClickableText" sizeLabel)
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
