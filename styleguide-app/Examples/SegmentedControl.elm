module Examples.SegmentedControl exposing (Msg, State, example, init, update)

{-|

@docs Msg, State, example, init, update,

-}

import Html
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.SegmentedControl.V1


{-| -}
type Msg
    = Select Id


{-| -}
type alias State =
    Nri.Ui.SegmentedControl.V1.Config Id Msg


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { filename = "Nri/Ui/SegmentedControl/V1.elm"
    , category = Behaviors
    , content =
        [ Html.map parentMessage (Nri.Ui.SegmentedControl.V1.view state)
        ]
    }


{-| -}
init : State
init =
    { onClick = Select
    , options =
        [ { label = "Option A", id = "a", value = "a" }
        , { label = "Option B", id = "b", value = "b" }
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
