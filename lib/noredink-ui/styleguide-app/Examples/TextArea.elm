module Examples.TextArea exposing (Msg, State, example, init, update)

{-|

@docs Msg, State, example, init, update

-}

import Dict exposing (Dict)
import Html.Styled as Html
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.AssetPath exposing (Asset(..))
import Nri.Ui.Checkbox.V6 as Checkbox
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.TextArea.V4 as TextArea


{-| -}
type Msg
    = InputGiven Id String
    | ToggleLabel Bool
    | ToggleAutoResize Bool
    | ToggleErrorState Bool


{-| -}
type alias State =
    { textValues : Dict Int String
    , showLabel : Checkbox.Selected
    , isInError : Checkbox.Selected
    , autoResize : Checkbox.Selected
    }


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { name = "Nri.Ui.TextArea.V4"
    , category = Inputs
    , content =
        [ Heading.h1 [] [ Html.text "Textarea controls" ]
        , Html.div []
            [ Checkbox.checkbox
                { identifier = "show-textarea-label"
                , label = "Show Label"
                , anonymous = True
                , onChange = ToggleLabel
                , selected = state.showLabel
                }
            , Checkbox.checkbox
                { identifier = "textarea-autoresize"
                , label = "Autoresize"
                , anonymous = True
                , onChange = ToggleAutoResize
                , selected = state.autoResize
                }
            , Checkbox.checkbox
                { identifier = "textarea-isInError"
                , label = "Show Error State"
                , anonymous = True
                , onChange = ToggleErrorState
                , selected = state.isInError
                }
            ]
        , TextArea.view
            { value = Maybe.withDefault "" <| Dict.get 1 state.textValues
            , autofocus = False
            , onInput = InputGiven 1
            , onBlur = Nothing
            , isInError = state.isInError == Checkbox.Selected
            , label = "TextArea.view"
            , height =
                if state.autoResize == Checkbox.Selected then
                    TextArea.AutoResize TextArea.SingleLine

                else
                    TextArea.Fixed
            , placeholder = "Placeholder"
            , showLabel = state.showLabel == Checkbox.Selected
            }
        , TextArea.writing
            { value = Maybe.withDefault "" <| Dict.get 2 state.textValues
            , autofocus = False
            , onInput = InputGiven 2
            , onBlur = Nothing
            , isInError = state.isInError == Checkbox.Selected
            , label = "TextArea.writing"
            , height =
                if state.autoResize == Checkbox.Selected then
                    TextArea.AutoResize TextArea.DefaultHeight

                else
                    TextArea.Fixed
            , placeholder = "Placeholder"
            , showLabel = state.showLabel == Checkbox.Selected
            }
        , TextArea.contentCreation
            { value = Maybe.withDefault "" <| Dict.get 3 state.textValues
            , autofocus = False
            , onInput = InputGiven 3
            , onBlur = Nothing
            , isInError = state.isInError == Checkbox.Selected
            , label = "TextArea.contentCreation"
            , height =
                if state.autoResize == Checkbox.Selected then
                    TextArea.AutoResize TextArea.DefaultHeight

                else
                    TextArea.Fixed
            , placeholder = "Placeholder"
            , showLabel = state.showLabel == Checkbox.Selected
            }
        , TextArea.writing
            { value = Maybe.withDefault "" <| Dict.get 4 state.textValues
            , autofocus = False
            , onInput = InputGiven 4
            , onBlur = Just (InputGiven 4 "Neener neener Blur happened")
            , isInError = state.isInError == Checkbox.Selected
            , label = "TextArea.writing onBlur demonstration"
            , height =
                if state.autoResize == Checkbox.Selected then
                    TextArea.AutoResize TextArea.DefaultHeight

                else
                    TextArea.Fixed
            , placeholder = "Placeholder"
            , showLabel = state.showLabel == Checkbox.Selected
            }
        ]
            |> List.map (Html.map parentMessage)
    }


{-| -}
init : State
init =
    { textValues = Dict.empty
    , showLabel = Checkbox.Selected
    , isInError = Checkbox.NotSelected
    , autoResize = Checkbox.NotSelected
    }


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    let
        toggle bool =
            if bool then
                Checkbox.Selected

            else
                Checkbox.NotSelected
    in
    case msg of
        InputGiven id newValue ->
            ( { state | textValues = Dict.insert id newValue state.textValues }
            , Cmd.none
            )

        ToggleLabel bool ->
            ( { state | showLabel = toggle bool }
            , Cmd.none
            )

        ToggleErrorState bool ->
            ( { state | isInError = toggle bool }
            , Cmd.none
            )

        ToggleAutoResize bool ->
            ( { state | autoResize = toggle bool }
            , Cmd.none
            )



-- INTERNAL


type alias Id =
    Int
