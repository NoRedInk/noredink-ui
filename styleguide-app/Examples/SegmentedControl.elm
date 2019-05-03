module Examples.SegmentedControl exposing
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

import Accessibility.Styled
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.SegmentedControl.V6 exposing (Width(..))


{-| -}
type Msg
    = Select Id
    | SetFillContainer Bool


{-| -}
type alias State =
    Nri.Ui.SegmentedControl.V6.Config Id Msg


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { filename = "Nri/Ui/SegmentedControl/V6.elm"
    , category = Widgets
    , content =
        List.map (Html.map parentMessage)
            [ fillContainerCheckbox state.width
            , Nri.Ui.SegmentedControl.V6.view state
            ]
    }


{-| -}
init : State
init =
    { onClick = Select
    , options =
        [ { icon = Nothing
          , id = "a"
          , label = "Option A"
          , value = "a"
          }
        , { icon = Nothing
          , id = "b"
          , label = "Option B"
          , value = "b"
          }
        ]
    , selected = "a"
    , width = FitContent
    }


fillContainerCheckbox : Width -> Html Msg
fillContainerCheckbox currentOption =
    let
        id =
            "SegmentedControl-fill-container-checkbox"

        isChecked =
            case currentOption of
                FitContent ->
                    Just False

                FillContainer ->
                    Just True
    in
    Html.div []
        [ Accessibility.Styled.checkbox "Fill container"
            isChecked
            [ Attr.id id
            , Events.onCheck SetFillContainer
            ]
        , Html.label
            [ Attr.for id
            ]
            [ Html.text "Fill Container" ]
        ]


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Select id ->
            ( { state | selected = id }, Cmd.none )

        SetFillContainer fillContainer ->
            ( { state
                | width =
                    if fillContainer then
                        FillContainer

                    else
                        FitContent
              }
            , Cmd.none
            )



-- INTERNAL


type alias Id =
    String
