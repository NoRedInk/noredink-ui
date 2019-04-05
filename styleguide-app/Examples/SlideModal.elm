module Examples.SlideModal exposing (Msg, State, example, init, update)

{-|

@docs Msg, State, example, init, update

-}

import Accessibility.Styled as Html exposing (Html, div, h3, p, text)
import Assets
import Css exposing (..)
import Html.Styled.Attributes exposing (css)
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Button.V8 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.SlideModal.V1 as SlideModal


{-| -}
type Msg
    = DismissModal
    | ShowModal


{-| -}
type alias State =
    { modal : Bool }


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { filename = "Nri.Ui.SlideModal.V1.elm"
    , category = Modals
    , content =
        [ if state.modal then
            viewModal

          else
            text ""
        , modalLaunchButton
        ]
            |> List.map (Html.map parentMessage)
    }


{-| -}
init : State
init =
    { modal = False }


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        DismissModal ->
            ( { state | modal = False }, Cmd.none )

        ShowModal ->
            ( { state | modal = True }, Cmd.none )



-- INTERNAL


modalLaunchButton : Html Msg
modalLaunchButton =
    Button.button
        { onClick = ShowModal
        , size = Button.Small
        , style = Button.Secondary
        , width = Button.WidthUnbounded
        }
        { label = "Launch Modal"
        , state = Button.Enabled
        , icon = Nothing
        }


viewModal : Html Msg
viewModal =
    SlideModal.view
        { panels =
            [ { icon = grayBox
              , title = "Welcome to Self-Review, FirstName!"
              , content = text "This is where the content goes!"
              , button = { label = "Continue", msg = DismissModal }
              }
            ]
        }


grayBox : Html msg
grayBox =
    div
        [ css
            [ Css.backgroundColor Colors.gray45
            , Css.height (pct 100)
            , Css.width (pct 100)
            ]
        ]
        []
