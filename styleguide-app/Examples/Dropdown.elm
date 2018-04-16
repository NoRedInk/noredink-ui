module Examples.Dropdown exposing (Msg, State, Value, example, init, update)

{-|

@docs Msg, State, Value, example, init, update,

-}

import Html
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Dropdown.V1


{-| -}
type alias Value =
    String


{-| -}
type Msg
    = ConsoleLog String


{-| -}
type alias State value =
    List (Nri.Ui.Dropdown.V1.ViewOptionEntry value)


{-| -}
example : (Msg -> msg) -> State Value -> ModuleExample msg
example parentMessage state =
    { filename = "Nri/Ui/Dropdown/V1.elm"
    , category = Inputs
    , content =
        [ Html.map parentMessage (Nri.Ui.Dropdown.V1.view "All the foods!" state ConsoleLog)
        ]
    }


{-| -}
init : State Value
init =
    [ { isSelected = False, val = "Burrito", displayText = "Burrito" }
    , { isSelected = False, val = "Nacho", displayText = "Nacho" }
    , { isSelected = False, val = "Horchata", displayText = "Horchata" }
    ]


{-| -}
update : Msg -> State Value -> ( State Value, Cmd Msg )
update msg state =
    case msg of
        ConsoleLog message ->
            let
                _ =
                    Debug.log "DropdownExample" message
            in
            ( state, Cmd.none )
