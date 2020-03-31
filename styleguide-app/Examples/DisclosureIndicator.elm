module Examples.DisclosureIndicator exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Css
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Nri.Ui.Button.V8 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.DisclosureIndicator.V2 as DisclosureIndicator
import Nri.Ui.Text.V2 as Text


{-| -}
type alias State =
    { largeState : Bool
    , mediumState : Bool
    }


{-| -}
example =
    { name = "Nri.Ui.DisclosureIndicator.V2"
    , categories = [ Widgets ]
    , state = init
    , update = update
    , view =
        \state ->
            [ Text.smallBodyGray [ Html.text "The disclosure indicator is only the caret. It is NOT a button -- you must create a button or clickabletext yourself!" ]
            , Html.div [ css [ Css.displayFlex, Css.padding (Css.px 8) ] ]
                [ toggleButton ToggleLarge "Toggle large indicator"
                , toggleButton ToggleMedium "Toggle medium indicator"
                ]
            , Html.div [ css [ Css.displayFlex, Css.alignItems Css.center, Css.marginBottom (Css.px 8) ] ]
                [ DisclosureIndicator.large [ Css.marginRight (Css.px 10) ] state.largeState
                , Html.text "I'm a 17px caret icon."
                ]
            , Html.div [ css [ Css.displayFlex, Css.alignItems Css.center, Css.marginBottom (Css.px 8) ] ]
                [ DisclosureIndicator.medium [ Css.paddingRight (Css.px 8) ] state.mediumState
                , Html.text "I'm a 15px caret icon."
                ]
            ]
    }


toggleButton : msg -> String -> Html.Html msg
toggleButton msg label =
    Button.button
        { onClick = msg
        , size = Button.Small
        , style = Button.Secondary
        , width = Button.WidthUnbounded
        }
        { label = label
        , state = Button.Enabled
        , icon = Nothing
        }


{-| -}
init : State
init =
    { largeState = False
    , mediumState = False
    }


{-| -}
type Msg
    = ToggleLarge
    | ToggleMedium


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ToggleLarge ->
            ( { state | largeState = not state.largeState }, Cmd.none )

        ToggleMedium ->
            ( { state | mediumState = not state.mediumState }, Cmd.none )
