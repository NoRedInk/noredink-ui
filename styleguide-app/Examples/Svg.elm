module Examples.Svg exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Examples.UiIcon as UiIcons
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Nri.Ui.Colors.Extra exposing (fromCssColor, toCssColor)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Select.V8 as Select
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon
import SolidColor exposing (SolidColor)


{-| -}
example : Example State Msg
example =
    { name = "Svg"
    , version = 1
    , categories = [ Icons ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = []
    , view =
        \ellieLinkConfig state ->
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
        [ Select.view "Icon"
            [ Select.groupedChoices Tuple.first (List.map svgGroupedChoices UiIcons.all)
            , Select.value (Just state.icon)
            ]
            |> Html.map SetIcon
        , Html.label []
            [ Html.text "Color: "
            , Html.input
                [ Attributes.type_ "color"
                , Attributes.value (SolidColor.toHex state.color)
                , Events.onInput (SetColor << SolidColor.fromHex)
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
            [ Html.text "Title: "
            , Html.input
                [ Attributes.value state.label
                , Events.onInput SetLabel
                ]
                []
            ]
        ]


svgGroupedChoices ( groupName, items ) =
    let
        toEntry ( name, icon ) =
            Select.Choice name ( name, icon )
    in
    Select.ChoicesGroup groupName (List.map toEntry items)


viewResults : State -> Html.Html Msg
viewResults state =
    let
        ( red, green, blue ) =
            SolidColor.toRGB state.color
    in
    Html.div [ Attributes.css [ Css.displayFlex ] ]
        [ Html.pre
            [ Attributes.css
                [ Css.width (Css.px 400)
                , Css.marginRight (Css.px 20)
                ]
            ]
            [ [ "color : Css.Color\n"
              , "color =\n"
              , "    Css.rgb " ++ String.fromFloat red ++ " " ++ String.fromFloat green ++ " " ++ String.fromFloat blue
              , "\n\n\n"
              , "renderedSvg : Svg\n"
              , "renderedSvg =\n"
              , "   UiIcon." ++ Tuple.first state.icon ++ "\n"
              , "       |> Svg.withColor color\n"
              , "       |> Svg.withWidth (Css.px " ++ String.fromFloat state.width ++ ")\n"
              , "       |> Svg.withHeight (Css.px " ++ String.fromFloat state.height ++ ")\n"
              , if String.isEmpty state.label then
                    ""

                else
                    "       |> Svg.withLabel \"" ++ state.label ++ "\"\n"
              , "       |> Svg.toHtml\n"
              ]
                |> String.join ""
                |> Html.text
            ]
        , Html.div
            [ Attributes.css
                [ Css.backgroundColor Colors.gray92
                , Css.flexGrow (Css.int 2)
                ]
            ]
            [ Tuple.second state.icon
                |> Svg.withColor (toCssColor state.color)
                |> Svg.withWidth (Css.px state.width)
                |> Svg.withHeight (Css.px state.height)
                |> (\svg ->
                        if String.isEmpty state.label then
                            svg

                        else
                            Svg.withLabel state.label svg
                   )
                |> Svg.toHtml
            ]
        ]


{-| -}
type alias State =
    { iconSelectorExpanded : Bool
    , icon : ( String, Svg )
    , color : SolidColor
    , width : Float
    , height : Float
    , label : String
    }


{-| -}
init : State
init =
    { iconSelectorExpanded = False
    , icon = ( "newspaper", UiIcon.newspaper )
    , color = fromCssColor Colors.azure
    , width = 30
    , height = 30
    , label = "Newspaper"
    }


{-| -}
type Msg
    = SetIcon ( String, Svg )
    | SetColor (Result String SolidColor)
    | SetWidth (Maybe Float)
    | SetHeight (Maybe Float)
    | SetLabel String


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetIcon svg ->
            ( { state | icon = svg }
            , Cmd.none
            )

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
