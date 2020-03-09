module Examples.Svg exposing
    ( Msg
    , State
    , example
    , init
    , update
    )

{-|

@docs Msg
@docs State
@docs example
@docs init
@docs update

-}

import Color exposing (Color)
import Css
import Examples.IconExamples as IconExamples
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import ModuleExample exposing (Category(..), ModuleExample, ModuleMessages)
import Nri.Ui.ClickableSvg.V1 as ClickableSvg
import Nri.Ui.Colors.Extra exposing (fromCssColor, toCssColor)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Select.V6 as Select
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
example : (String -> ModuleMessages Msg msg) -> State -> ModuleExample msg
example unnamedMessages state =
    let
        parentMessages =
            unnamedMessages "Nri.Ui.Svg.V1"
    in
    { name = "Nri.Ui.Svg.V1"
    , category = Icons
    , content =
        [ viewSettings state
            |> Html.map parentMessages.wrapper
        , viewResults parentMessages.showItWorked state
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
        [ Html.fieldset []
            [ Html.legend [] [ Html.text "Render strategy" ]
            , Html.label [ Attributes.css [ Css.display Css.block ] ]
                [ Html.input
                    [ Attributes.type_ "radio"
                    , Attributes.id "render-with-toHtml"
                    , Attributes.name "render-with"
                    , Attributes.value toHtml
                    , Attributes.checked (state.renderStrategy == toHtml)
                    , Events.onInput SetRenderStrategy
                    ]
                    []
                , Html.span [] [ Html.text "toHtml" ]
                ]
            , Html.label [ Attributes.css [ Css.display Css.block ] ]
                [ Html.input
                    [ Attributes.type_ "radio"
                    , Attributes.id "render-with-toHtmlAsButton"
                    , Attributes.name "render-with"
                    , Attributes.value toHtmlAsButton
                    , Attributes.checked (state.renderStrategy == toHtmlAsButton)
                    , Events.onInput SetRenderStrategy
                    ]
                    []
                , Html.span [] [ Html.text "toHtmlAsButton" ]
                ]
            , Html.label [ Attributes.css [ Css.display Css.block ] ]
                [ Html.input
                    [ Attributes.type_ "radio"
                    , Attributes.id "render-with-toHtmlAsLink"
                    , Attributes.name "render-with"
                    , Attributes.value toHtmlAsLink
                    , Attributes.checked (state.renderStrategy == toHtmlAsLink)
                    , Events.onInput SetRenderStrategy
                    ]
                    []
                , Html.span [] [ Html.text "toHtmlAsLink" ]
                ]
            ]
        , Html.label []
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


viewResults : (String -> msg) -> State -> Html.Html msg
viewResults msg state =
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
              , "       |> Svg." ++ state.renderStrategy
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
                |> render state.renderStrategy msg
            ]
        ]


{-| -}
type alias State =
    { color : Color
    , width : Float
    , height : Float
    , label : String
    , renderStrategy : String
    }


{-| -}
init : State
init =
    { color = fromCssColor Colors.blue
    , width = 30
    , height = 30
    , label = "Newspaper"
    , renderStrategy = toHtml
    }


{-| -}
type Msg
    = SetColor (Result String Color)
    | SetWidth (Maybe Float)
    | SetHeight (Maybe Float)
    | SetLabel String
    | SetRenderStrategy String


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

        SetRenderStrategy renderStrategy ->
            ( { state | renderStrategy = renderStrategy }, Cmd.none )


render : String -> (String -> msg) -> Svg.Svg -> Html.Html msg
render strategy msg =
    if strategy == toHtml then
        Svg.toHtml

    else if strategy == toHtmlAsButton then
        ClickableSvg.button (msg "You clicked the 'toHtmlAsButton' button!")

    else
        ClickableSvg.link "/you_clicked_the_toHtmlAsLink_link"


toHtml : String
toHtml =
    "toHtml"


toHtmlAsButton : String
toHtmlAsButton =
    "toHtmlAsButton"


toHtmlAsLink : String
toHtmlAsLink =
    "toHtmlAsLink"
