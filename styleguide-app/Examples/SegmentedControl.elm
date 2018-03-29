module Examples.SegmentedControl exposing (Msg, State, example, init, update)

{-|

@docs Msg, State, example, init, update,

-}

import Html
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Icon.V1
import Nri.Ui.SegmentedControl.V3


{-| -}
type Msg
    = Select Id


{-| -}
type alias State =
    Nri.Ui.SegmentedControl.V3.Config Id Msg


{-| -}
example : Nri.Ui.Icon.V1.Assets r -> (Msg -> msg) -> State -> ModuleExample msg
example assets parentMessage state =
    { filename = "Nri/Ui/SegmentedControl/V3.elm"
    , category = Behaviors
    , content =
        [ Html.map parentMessage (Nri.Ui.SegmentedControl.V3.view assets state)
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
