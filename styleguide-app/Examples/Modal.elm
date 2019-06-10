module Examples.Modal exposing (Msg, State, example, init, update, subscriptions)

{-|

@docs Msg, State, example, init, update, subscriptions

-}

import Accessibility.Styled as Html exposing (Html, div, h3, p, text)
import Css exposing (..)
import Html.Styled.Attributes exposing (css)
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Modal.V5 as Modal


{-| -}
type Msg
    = InfoModalMsg Modal.Msg
    | WarningModalMsg Modal.Msg


{-| -}
type alias State =
    { infoModal : Modal.Model
    , warningModal : Modal.Model
    }


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { name = "Nri.Ui.Modal.V5"
    , category = Modals
    , content =
        [ Modal.info
            { launchButton = Modal.launchButton [] "Launch Info Modal"
            , title = "Modal.info"
            , visibleTitle = True
            , content = text "This is where the content goes!"
            , parentMsg = InfoModalMsg
            }
            state.infoModal
        , Modal.warning
            { launchButton = Modal.launchButton [] "Launch Warning Modal"
            , title = "Modal.warning"
            , visibleTitle = True
            , content = text "This is where the content goes!"
            , parentMsg = WarningModalMsg
            }
            state.warningModal
        ]
            |> List.map (Html.map parentMessage)
    }


{-| -}
init : State
init =
    { infoModal = Modal.init
    , warningModal = Modal.init
    }


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        InfoModalMsg modalMsg ->
            case Modal.update modalMsg state.infoModal of
                ( newState, cmds ) ->
                    ( { state | infoModal = newState }
                    , Cmd.map InfoModalMsg cmds
                    )

        WarningModalMsg modalMsg ->
            case Modal.update modalMsg state.warningModal of
                ( newState, cmds ) ->
                    ( { state | warningModal = newState }
                    , Cmd.map WarningModalMsg cmds
                    )


{-| -}
subscriptions : State -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map InfoModalMsg (Modal.subscriptions model.infoModal)
        , Sub.map WarningModalMsg (Modal.subscriptions model.warningModal)
        ]
