module Examples.Dropdown exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Html.Styled
import ModuleExample exposing (ModuleExample)
import Nri.Ui.Dropdown.V2
import Sort.Set as Set exposing (Set)


{-| -}
type Msg
    = ConsoleLog String


{-| -}
type alias State =
    List (Nri.Ui.Dropdown.V2.ViewOptionEntry String)


{-| -}
example =
    { name = "Nri.Ui.Dropdown.V2"
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view =
        \state ->
            [ Nri.Ui.Dropdown.V2.view "All the foods!" state ConsoleLog
            ]
    , categories = [ Inputs ]
    }


{-| -}
init : State
init =
    [ { isSelected = False, val = "Burrito", displayText = "Burrito" }
    , { isSelected = False, val = "Nacho", displayText = "Nacho" }
    , { isSelected = False, val = "Horchata", displayText = "Horchata" }
    ]


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ConsoleLog message ->
            let
                _ =
                    Debug.log "DropdownExample" message
            in
            ( state, Cmd.none )
