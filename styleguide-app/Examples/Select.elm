module Examples.Select
    exposing
        ( Msg
        , State
        , Value
        , example
        , init
        , update
        )

{-|

@docs Msg
@docs State
@docs Value
@docs example
@docs init
@docs update

-}

import Html
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Select.V1


{-| -}
type alias Value =
    String


{-| -}
type Msg
    = ConsoleLog String


{-| -}
type alias State value =
    Nri.Ui.Select.V1.Config value


{-| -}
example : (Msg -> msg) -> State Value -> ModuleExample msg
example parentMessage state =
    { filename = "ui/src/Nri/Select.elm"
    , category = Inputs
    , content =
        [ Html.map (parentMessage << ConsoleLog) (Nri.Ui.Select.V1.view state)
        ]
    }


{-| -}
init : State Value
init =
    { current = ""
    , choices =
        [ { label = "Tacos", value = "Tacos" }
        , { label = "Burritos", value = "Burritos" }
        , { label = "Enchiladas", value = "Enchiladas" }
        ]
    }


{-| -}
update : Msg -> State Value -> ( State Value, Cmd Msg )
update msg state =
    case msg of
        ConsoleLog message ->
            let
                _ =
                    Debug.log "SelectExample" message
            in
            ( state, Cmd.none )
