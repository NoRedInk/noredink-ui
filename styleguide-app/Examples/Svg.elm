module Examples.Svg exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import AtomicDesignType exposing (AtomicDesignType(..))
import Category exposing (Category(..))
import Color exposing (Color)
import Css
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import KeyboardShortcuts exposing (Direction(..), Key(..))
import Nri.Ui.Colors.Extra exposing (fromCssColor, toCssColor)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Select.V7 as Select
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.Svg.V1"
    , categories = [ Icons ]
    , atomicDesignType = Atom
    , keyboardShortcuts = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view =
        \state ->
            [ viewSettings state
            , viewResults state
            ]
    }


viewSettings : State -> Html.Html Msg
viewSettings state =
    Html.div
        [ Attributes.css
            [ Css.displayFlex
            , Css.justifyContent Css.spaceBetween
            ]
        ]
        [ Html.label []
            [ Html.text "Color: "
            , Html.input
                [ Attributes.type_ "color"
                , Attributes.value (Color.toHex state.color)
                , Events.onInput (SetColor << Color.fromHex)
                ]
                []
            ]
        , Html.label []
            [ Html.text "Width: "
            , Html.input
                [ Attributes.type_ "range"
                , Attributes.min "0"
                , Attributes.max "200"
                , Attributes.value (String.fromFloat state.width)
                , Events.onInput (SetWidth << String.toFloat)
                ]
                []
            ]
        , Html.label []
            [ Html.text "Height: "
            , Html.input
                [ Attributes.type_ "range"
                , Attributes.min "0"
                , Attributes.max "200"
                , Attributes.value (String.fromFloat state.height)
                , Events.onInput (SetHeight << String.toFloat)
                ]
                []
            ]
        , Html.label []
            [ Html.text "Aria-label: "
            , Html.input
                [ Attributes.value state.label
                , Events.onInput SetLabel
                ]
                []
            ]
        ]


viewResults : State -> Html.Html Msg
viewResults state =
    let
        ( red, green, blue ) =
            Color.toRGB state.color
    in
    Html.div [ Attributes.css [ Css.displayFlex ] ]
        [ Html.pre
            [ Attributes.css
                [ Css.width (Css.px 400)
                , Css.marginRight (Css.px 20)
                ]
            ]
            [ [ "color : Css.Color"
              , "color ="
              , "    Css.rgb " ++ String.fromFloat red ++ " " ++ String.fromFloat green ++ " " ++ String.fromFloat blue
              , ""
              , ""
              , "renderedSvg : Svg "
              , "renderedSvg = "
              , "   UiIcon.newspaper"
              , "       |> Svg.withColor color"
              , "       |> Svg.withWidth (Css.px " ++ String.fromFloat state.width ++ ")"
              , "       |> Svg.withHeight (Css.px " ++ String.fromFloat state.height ++ ")"
              , "       |> Svg.withLabel \"" ++ state.label ++ "\""
              , "       |> Svg.toHtml"
              ]
                |> String.join "\n"
                |> Html.text
            ]
        , Html.div
            [ Attributes.css
                [ Css.backgroundColor Colors.gray92
                , Css.flexGrow (Css.int 2)
                ]
            ]
            [ UiIcon.newspaper
                |> Svg.withColor (toCssColor state.color)
                |> Svg.withWidth (Css.px state.width)
                |> Svg.withHeight (Css.px state.height)
                |> Svg.withLabel state.label
                |> Svg.toHtml
            ]
        ]


{-| -}
type alias State =
    { color : Color
    , width : Float
    , height : Float
    , label : String
    }


{-| -}
init : State
init =
    { color = fromCssColor Colors.blue
    , width = 30
    , height = 30
    , label = "Newspaper"
    }


{-| -}
type Msg
    = SetColor (Result String Color)
    | SetWidth (Maybe Float)
    | SetHeight (Maybe Float)
    | SetLabel String


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetColor (Ok color) ->
            ( { state | color = color }
            , Cmd.none
            )

        SetColor (Err err) ->
            ( state, Cmd.none )

        SetWidth (Just width) ->
            ( { state | width = width }, Cmd.none )

        SetWidth Nothing ->
            ( state, Cmd.none )

        SetHeight (Just height) ->
            ( { state | height = height }, Cmd.none )

        SetHeight Nothing ->
            ( state, Cmd.none )

        SetLabel label ->
            ( { state | label = label }, Cmd.none )
