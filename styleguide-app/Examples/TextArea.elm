module Examples.TextArea exposing (Msg, State, example, init, update)

{-|

@docs Msg, State, example, init, update,

-}

import Css
import Dict exposing (Dict)
import Html
import Html.Styled
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Checkbox.V2 as Checkbox
import Nri.Ui.Text.V2 as Text
import Nri.Ui.TextArea.V2 as TextArea


{-| -}
type Msg
    = InputGiven Id String
    | ToggleLabel Bool
    | ToggleAutoResize Bool
    | ToggleErrorState Bool
    | NoOp


{-| -}
type alias State =
    { textValues : Dict Int String
    , showLabel : Bool
    , isInError : Bool
    , autoResize : Bool
    }


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { filename = "Nri/TextArea.elm"
    , category = Inputs
    , content =
        [ Text.heading [ Html.Styled.text "Textarea controls" ]
            |> Html.Styled.toUnstyled
        , Html.div []
            [ Checkbox.viewWithLabel
                { identifier = "show-textarea-label"
                , label = "Show Label"
                , setterMsg = ToggleLabel
                , isChecked = Just state.showLabel
                , disabled = False
                , theme = Checkbox.Square Checkbox.Default
                , noOpMsg = NoOp
                }
            , Checkbox.viewWithLabel
                { identifier = "textarea-autoresize"
                , label = "Autoresize"
                , setterMsg = ToggleAutoResize
                , isChecked = Just state.autoResize
                , disabled = False
                , theme = Checkbox.Square Checkbox.Default
                , noOpMsg = NoOp
                }
            , Checkbox.viewWithLabel
                { identifier = "textarea-isInError"
                , label = "Show Error State"
                , setterMsg = ToggleErrorState
                , isChecked = Just state.isInError
                , disabled = False
                , theme = Checkbox.Square Checkbox.Default
                , noOpMsg = NoOp
                }
            ]
        , TextArea.view
            { value = Maybe.withDefault "" <| Dict.get 1 state.textValues
            , autofocus = False
            , onInput = InputGiven 1
            , isInError = state.isInError
            , label = "TextArea.view"
            , autoResize = state.autoResize
            , placeholder = "Placeholder"
            , showLabel = state.showLabel
            , minimumHeight = Css.px 100
            }
        , TextArea.writing
            { value = Maybe.withDefault "" <| Dict.get 2 state.textValues
            , autofocus = False
            , onInput = InputGiven 2
            , isInError = state.isInError
            , label = "TextArea.writing"
            , autoResize = state.autoResize
            , placeholder = "Placeholder"
            , showLabel = state.showLabel
            , minimumHeight = Css.px 100
            }
        , TextArea.contentCreation
            { value = Maybe.withDefault "" <| Dict.get 3 state.textValues
            , autofocus = False
            , onInput = InputGiven 3
            , isInError = state.isInError
            , label = "TextArea.contentCreation"
            , autoResize = state.autoResize
            , placeholder = "Placeholder"
            , showLabel = state.showLabel
            , minimumHeight = Css.px 100
            }
        ]
            |> List.map (Html.map parentMessage)
    }


{-| -}
init : State
init =
    { textValues = Dict.empty
    , showLabel = True
    , isInError = False
    , autoResize = False
    }


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        InputGiven id newValue ->
            ( { state | textValues = Dict.insert id newValue state.textValues }
            , Cmd.none
            )

        ToggleLabel bool ->
            ( { state | showLabel = bool }
            , Cmd.none
            )

        ToggleErrorState bool ->
            ( { state | isInError = bool }
            , Cmd.none
            )

        ToggleAutoResize bool ->
            ( { state | autoResize = bool }
            , Cmd.none
            )

        NoOp ->
            ( state, Cmd.none )



-- INTERNAL


type alias Id =
    Int
