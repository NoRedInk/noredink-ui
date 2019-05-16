module Examples.Select exposing
    ( Msg
    , State
    , Value
    , example
    , init
    , update
    , State2, example2, init2, update2
    )

{-|

@docs Msg
@docs State
@docs Value
@docs example
@docs init
@docs update

-}

import Html.Styled
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Select.V5 as Select
import Nri.Ui.Select.V6 as Select6


{-| -}
type alias Value =
    String


{-| -}
type Msg
    = ConsoleLog String


{-| -}
type alias State value =
    Select.Config value


type alias State2 value =
    Select6.Config value


{-| -}
example : (Msg -> msg) -> State Value -> ModuleExample msg
example parentMessage state =
    { name = "Nri.Ui.Select.V5"
    , category = Inputs
    , content =
        [ Html.Styled.map (parentMessage << ConsoleLog) (Select.view state)
        ]
    }


{-| -}
example2 : (Msg -> msg) -> State2 Value -> ModuleExample msg
example2 parentMessage state =
    { name = "Nri.Ui.Select.V6"
    , category = Inputs
    , content =
        [ Html.Styled.map (parentMessage << ConsoleLog) (Select6.view state)
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
    , id = Nothing
    , valueToString = identity
    }


{-| -}
init2 : State2 Value
init2 =
    { current = ""
    , choices =
        [ { label = "Tacos", value = "Tacos" }
        , { label = "Burritos", value = "Burritos" }
        , { label = "Enchiladas", value = "Enchiladas" }
        ]
    , id = Nothing
    , valueToString = identity
    , defaultDisplayText = Just "Select a tortilla based joy"
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


{-| -}
update2 : Msg -> State2 Value -> ( State2 Value, Cmd Msg )
update2 msg state =
    case msg of
        ConsoleLog message ->
            let
                _ =
                    Debug.log "SelectExample" message
            in
            ( state, Cmd.none )
