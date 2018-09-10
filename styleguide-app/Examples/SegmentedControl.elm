module Examples.SegmentedControl
    exposing
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
@docs styles
@docs update

-}

import Css
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.SegmentedControl.V6 exposing (Width(..))


{-| -}
type Msg
    = Select Id


{-| -}
type alias State =
    Nri.Ui.SegmentedControl.V6.Config Id Msg


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { filename = "Nri/Ui/SegmentedControl/V6.elm"
    , category = Behaviors
    , content =
        [ Nri.Ui.SegmentedControl.V6.view state
            |> Html.map parentMessage
            |> Html.toUnstyled
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


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Select id ->
            ( { state | selected = id }, Cmd.none )



-- INTERNAL


type alias Id =
    String
