module Examples.ClickableSvg exposing
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
            unnamedMessages "Nri.Ui.ClickableSvg.V1"
    in
    { name = "Nri.Ui.ClickableSvg.V1"
    , category = Buttons
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
                    , Attributes.id "render-with-ClickableSvg.button"
                    , Attributes.name "render-with"
                    , Attributes.value "ClickableSvg.button"
                    , Attributes.checked (state.renderStrategy == "ClickableSvg.button")
                    , Events.onInput SetRenderStrategy
                    ]
                    []
                , Html.span [] [ Html.text "ClickableSvg.button" ]
                ]
            , Html.label [ Attributes.css [ Css.display Css.block ] ]
                [ Html.input
                    [ Attributes.type_ "radio"
                    , Attributes.id "render-with-ClickableSvg.link"
                    , Attributes.name "render-with"
                    , Attributes.value "ClickableSvg.link"
                    , Attributes.checked (state.renderStrategy == "ClickableSvg.link")
                    , Events.onInput SetRenderStrategy
                    ]
                    []
                , Html.span [] [ Html.text "ClickableSvg.link" ]
                ]
            ]
        ]


viewResults : (String -> msg) -> State -> Html.Html msg
viewResults msg state =
    Html.div [ Attributes.css [ Css.displayFlex ] ]
        [ Html.pre
            [ Attributes.css
                [ Css.width (Css.px 400)
                , Css.marginRight (Css.px 20)
                ]
            ]
            [ [ "renderedSvg : Svg "
              , "renderedSvg = "
              , "   UiIcon.arrowLeft"
              , "       |> Svg.withWidth (Css.px 100)"
              , "       |> Svg.withHeight (Css.px 100)"
              , "       |> " ++ state.renderStrategy
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
            [ UiIcon.arrowLeft
                |> Svg.withWidth (Css.px 100)
                |> Svg.withHeight (Css.px 100)
                |> render state.renderStrategy msg
            ]
        ]


{-| -}
type alias State =
    { renderStrategy : String
    }


{-| -}
init : State
init =
    { renderStrategy = "ClickableSvg.button"
    }


{-| -}
type Msg
    = SetRenderStrategy String


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetRenderStrategy renderStrategy ->
            ( { state | renderStrategy = renderStrategy }, Cmd.none )


render : String -> (String -> msg) -> Svg.Svg -> Html.Html msg
render strategy msg =
    if strategy == "ClickableSvg.button" then
        ClickableSvg.button (msg "You clicked the button!")

    else
        ClickableSvg.link "#you_clicked_the_link"
