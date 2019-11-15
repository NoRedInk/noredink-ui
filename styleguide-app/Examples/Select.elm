module Examples.Select exposing
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

import Html.Styled
import Html.Styled.Attributes
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Select.V6 as Select


{-| -}
type alias Value =
    String


{-| -}
type Msg
    = ConsoleLog String


{-| -}
type alias State value =
    Select.Config value


{-| -}
example : (Msg -> msg) -> State Value -> ModuleExample msg
example parentMessage state =
    { name = "Nri.Ui.Select.V6"
    , category = Inputs
    , content =
        [ Html.Styled.label
            [ Html.Styled.Attributes.for "tortilla-selector" ]
            [ Heading.h3 [] [ Html.Styled.text "Tortilla Selector" ] ]
        , Html.Styled.map (parentMessage << ConsoleLog) (Select.view state)
        ]
    }


{-| -}
init : State Value
init =
    { current = Nothing
    , choices =
        [ { label = "Tacos", value = "Tacos" }
        , { label = "Burritos", value = "Burritos" }
        , { label = "Enchiladas", value = "Enchiladas" }
        ]
    , id = Just "tortilla-selector"
    , valueToString = identity
    , defaultDisplayText = Just "Select a tasty tortilla based treat!"
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
