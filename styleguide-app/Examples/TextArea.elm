module Examples.TextArea exposing (Msg, State, example, init, update)

{-|

@docs Msg, State, example, init, update

-}

import Dict exposing (Dict)
import Html
import Html.Styled
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.AssetPath exposing (Asset(..))
import Nri.Ui.Checkbox.V3 as Checkbox
import Nri.Ui.Text.V2 as Text
import Nri.Ui.TextArea.V3 as TextArea


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
    , showLabel : Checkbox.IsSelected
    , isInError : Checkbox.IsSelected
    , autoResize : Checkbox.IsSelected
    }


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { filename = "Nri.Ui.TextArea.V3"
    , category = Inputs
    , content =
        [ Text.heading [ Html.Styled.text "Textarea controls" ]
            |> Html.Styled.toUnstyled
        , Html.div []
            [ Checkbox.viewWithLabel assets
                { identifier = "show-textarea-label"
                , label = "Show Label"
                , setterMsg = ToggleLabel
                , selected = state.showLabel
                , disabled = False
                , theme = Checkbox.Square
                , noOpMsg = NoOp
                }
                |> Html.Styled.toUnstyled
            , Checkbox.viewWithLabel assets
                { identifier = "textarea-autoresize"
                , label = "Autoresize"
                , setterMsg = ToggleAutoResize
                , selected = state.autoResize
                , disabled = False
                , theme = Checkbox.Square
                , noOpMsg = NoOp
                }
                |> Html.Styled.toUnstyled
            , Checkbox.viewWithLabel assets
                { identifier = "textarea-isInError"
                , label = "Show Error State"
                , setterMsg = ToggleErrorState
                , selected = state.isInError
                , disabled = False
                , theme = Checkbox.Square
                , noOpMsg = NoOp
                }
                |> Html.Styled.toUnstyled
            ]
        , TextArea.view
            { value = Maybe.withDefault "" <| Dict.get 1 state.textValues
            , autofocus = False
            , onInput = InputGiven 1
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
            |> Html.Styled.toUnstyled
        , TextArea.writing
            { value = Maybe.withDefault "" <| Dict.get 2 state.textValues
            , autofocus = False
            , onInput = InputGiven 2
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
            |> Html.Styled.toUnstyled
        , TextArea.contentCreation
            { value = Maybe.withDefault "" <| Dict.get 3 state.textValues
            , autofocus = False
            , onInput = InputGiven 3
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
            |> Html.Styled.toUnstyled
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

        NoOp ->
            ( state, Cmd.none )



-- INTERNAL


type alias Id =
    Int


assets =
    { checkboxUnchecked_svg = Asset "checkboxUnchecked_svg"
    , checkboxChecked_svg = Asset "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAOEAAADhCAMAAAAJbSJIAAAAb1BMVEX///9DoEc2nDt1tXcoly601bU5nD1nr2o/n0Pw9vAzmzhAn0QxmjYumTMqmDAsmTK92r6UxJacyJ1YqFu52LvC3MOQwpIZlCGLwI3M4s2u0q+q0KueyaCkzKX1+fXq8+rh7uHW6NeDvIVztHVssW4Cx6cUAAACZUlEQVR4nO3Zi1LTYBRF4dOSQkqt4AURL6Di+z+jQWVsadJcen5z9pm1HiDZ3yQzSSZmRERERERERERERERERERERERERERERERElKj7uQeUbrVdzz2hbKuqrlMTV9VikZr4BExN/ANMTHwGpiX+AyYl7gJTEveBCYkvgemIh8BkxDZgKmI7sCEuH+ae5lMXcLF8nHuaT93As7mn+QRQPYDqAVQPoHoA1QOoHkD1AKoHUD2A6gFUD6B6ANUDqB5A9QCqB1A9gOoBVA+gegDVA6geQPUAqgfw/3Rd7MhRgLcXhY4cBHhTLc7LEIMA755mFCFGAhYhBgF+f57hTowGdCcGAX672jm1KzEI8GG7WZQhRgFebvZP70YMAlwvNy8HOBGjAOsDoBMxCPB+U7eNcCAGAa7bgQ7EIECzi/OOIScSwwBLEQMByxBDAUsQgwHNfjoTwwG9iQGBDXHpRwwJ9CQGBZo9OhHDAr2IgYFmZw7E0EAPYnDg6cTwwFOJAsDTiBJAsx+TiSLAhng5jSgDnEoUAk4jSgGnEMWA44lywLFEQaDZxxFESeAYoiiwIb4aRpQFDiUKA80+DCBKA4cQxYFmb3qI8sCGuD1GTAA8TkwBPEKs33U9MbWAZm+7iC3/xiWBR4hZgCOJikCzL1f9MmngCKIq0Ozr6+TAhjjkKioDBxG1gWaf+25UdWAvUR/YQ8wANPvU9aqdBXiEmAXYScwD7PjozQRsJeYCthCzAQ+I+YBm76vkwD1iTuAOMSvQ7LpKDvxLzAz8TcwNNLu5TQ40u5t7ABERERERERERERERERFN7hdTvSHAHrHcMgAAAABJRU5ErkJggg=="
    , checkboxCheckedPartially_svg = Asset "checkboxCheckedPartially_svg"
    , iconPremiumUnlocked_png = Asset "iconPremiumUnlocked_png"
    , iconPremiumLocked_png = Asset "iconPremiumLocked_png"
    , checkboxLockOnInside_svg = Asset "checkboxLockOnInside_svg"
    , iconPremiumKey_png = Asset "iconPremiumKey_png"
    , iconPremiumFlag_svg = Asset "iconPremiumFlag_svg"
    }
