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
import Examples.IconExamples as IconExamples
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Colors.Extra exposing (fromCssColor, toCssColor)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Select.V6 as Select
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
type Msg
    = SetColor (Result String Color)


{-| -}
type alias State =
    { color : Color
    }


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { name = "Nri.Ui.Svg.V1"
    , category = Icons
    , content =
        [ Html.label []
            [ Html.text "Color: "
            , Html.input
                [ Attributes.type_ "color"
                , Attributes.value (Color.toHex state.color)
                , Events.onInput (SetColor << Color.fromHex)
                ]
                []
                |> Html.map parentMessage
            ]
        , UiIcon.newspaper
            |> Svg.withColor (toCssColor state.color)
            |> Svg.toHtml
        ]
    }


{-| -}
init : State
init =
    { color = fromCssColor Colors.blue
    }


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
