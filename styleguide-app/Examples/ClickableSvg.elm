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
        [ Html.div [ Attributes.css [ Css.displayFlex, Css.alignItems Css.center ] ]
            [ viewCode
                [ "UiIcon.arrowLeft"
                , "    |>  ClickableSvg.button \"Back\" \n\t[ ClickableSvg.onClick OnClickMsg ]"
                ]
            , UiIcon.arrowLeft
                |> ClickableSvg.button "Back"
                    [ ClickableSvg.onClick (parentMessages.showItWorked "You clicked the button!") ]
            ]
        , Html.div [ Attributes.css [ Css.displayFlex, Css.alignItems Css.center ] ]
            [ viewCode
                [ "UiIcon.arrowLeft"
                , "    |> ClickableSvg.link \"Back\" \n\t[ ClickableSvg.href \"#some_link\" ]"
                ]
            , UiIcon.arrowLeft
                |> ClickableSvg.link "Back" [ ClickableSvg.href "#some_link" ]
            ]
        ]
    }


viewCode : List String -> Html.Html msg
viewCode renderStrategy =
    Html.code
        [ Attributes.css
            [ Css.width (Css.px 400)
            , Css.marginRight (Css.px 20)
            ]
        ]
        [ Html.pre [] [ Html.text (String.join "\n" renderStrategy) ] ]


{-| -}
type alias State =
    {}


{-| -}
init : State
init =
    {}


{-| -}
type Msg
    = SetRenderStrategy String


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    ( state, Cmd.none )
