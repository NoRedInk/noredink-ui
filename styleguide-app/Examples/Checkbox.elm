module Examples.Checkbox exposing (Msg, State, example, init, update)

{-|

@docs Msg, State, example, init, update,

-}

import Assets exposing (assets)
import Debug.Control as Control exposing (Control)
import Html.Styled as Html exposing (..)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Checkbox.V3 as Checkbox
import Nri.Ui.Data.PremiumLevel as PremiumLevel exposing (PremiumLevel(..))
import Nri.Ui.PremiumCheckbox.V1 as PremiumCheckbox
import Set exposing (Set)


{-| -}
type Msg
    = ToggleCheck Id Bool
    | SetPremiumControl (Control PremiumExampleConfig)
    | NoOp


{-| -}
type alias State =
    { isChecked : Set String
    , premiumControl : Control PremiumExampleConfig
    }


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { filename = "Nri/Checkbox.elm"
    , category = Inputs
    , content =
        [ viewInteractableCheckbox "styleguide-checkbox-interactable" state
        , viewIndeterminateCheckbox "styleguide-checkbox-indeterminate" state
        , viewLockedOnInsideCheckbox "styleguide-locked-on-inside-checkbox" state
        , viewDisabledCheckbox "styleguide-checkbox-disabled" state
        , h3 [] [ text "Premium Checkboxes" ]
        , Control.view SetPremiumControl state.premiumControl
            |> Html.fromUnstyled
        , viewPremiumCheckboxes state
        ]
            |> List.map (Html.toUnstyled << Html.map parentMessage)
    }


{-| -}
init : State
init =
    { isChecked = Set.empty
    , premiumControl =
        Control.record PremiumExampleConfig
            |> Control.field "disabled" (Control.bool False)
            |> Control.field "teacherPremiumLevel"
                (Control.choice
                    [ ( "Free", Control.value PremiumLevel.Free )
                    , ( "Premium", Control.value PremiumLevel.Premium )
                    , ( "Premium (with writing)", Control.value PremiumLevel.PremiumWithWriting )
                    ]
                )
            |> Control.field "showFlagWhenLocked" (Control.bool True)
    }


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ToggleCheck id checked ->
            let
                isChecked =
                    if checked then
                        Set.insert id state.isChecked
                    else
                        Set.remove id state.isChecked
            in
            ( { state | isChecked = isChecked }, Cmd.none )

        SetPremiumControl premiumControl ->
            ( { state | premiumControl = premiumControl }, Cmd.none )

        NoOp ->
            ( state, Cmd.none )



-- INTERNAL


type alias PremiumExampleConfig =
    { disabled : Bool
    , teacherPremiumLevel : PremiumLevel
    , showFlagWhenLocked : Bool
    }


viewInteractableCheckbox : Id -> State -> Html Msg
viewInteractableCheckbox id state =
    Checkbox.viewWithLabel
        assets
        { identifier = id
        , label = "This is an interactable checkbox!"
        , setterMsg = ToggleCheck id
        , selected = isSelected id state
        , disabled = False
        , theme = Checkbox.Square
        , noOpMsg = NoOp
        }


viewIndeterminateCheckbox : Id -> State -> Html Msg
viewIndeterminateCheckbox id state =
    Checkbox.viewWithLabel
        assets
        { identifier = id
        , label = "This Checkbox is set in an indeterminate state"
        , setterMsg = ToggleCheck id
        , selected = Checkbox.PartiallySelected
        , disabled = True
        , theme = Checkbox.Square
        , noOpMsg = NoOp
        }


viewLockedOnInsideCheckbox : Id -> State -> Html Msg
viewLockedOnInsideCheckbox id state =
    Checkbox.viewWithLabel
        assets
        { identifier = id
        , label = "I'm a locked Checkbox"
        , setterMsg = ToggleCheck id
        , selected = Checkbox.NotSelected
        , disabled = True
        , theme = Checkbox.Locked
        , noOpMsg = NoOp
        }


viewDisabledCheckbox : Id -> State -> Html Msg
viewDisabledCheckbox id state =
    Checkbox.viewWithLabel
        assets
        { identifier = id
        , label = "Disabled theme"
        , setterMsg = ToggleCheck id
        , selected = isSelected id state
        , disabled = True
        , theme = Checkbox.Square
        , noOpMsg = NoOp
        }


viewPremiumCheckboxes : State -> Html Msg
viewPremiumCheckboxes state =
    let
        config =
            Control.currentValue state.premiumControl

        checkbox label premiumLevel =
            PremiumCheckbox.premium
                assets
                { label = label
                , id = "premium-checkbox-" ++ label
                , selected =
                    if Set.member label state.isChecked then
                        Checkbox.Selected
                    else
                        Checkbox.NotSelected
                , disabled = config.disabled
                , teacherPremiumLevel = config.teacherPremiumLevel
                , contentPremiumLevel = premiumLevel
                , allowedFor = PremiumLevel.allowedFor
                , isFree = \level -> level == PremiumLevel.Free
                , showFlagWhenLocked = config.showFlagWhenLocked
                , onChange = ToggleCheck label
                , onLockedClick = NoOp
                , noOpMsg = NoOp
                }
    in
    Html.div []
        [ checkbox "Identify Adjectives 1 (Free)" PremiumLevel.Free
        , checkbox "Identify Adjectives 2 (Premium)" PremiumLevel.Premium
        , checkbox "Revising Wordy Phrases 1 (Writing)" PremiumLevel.PremiumWithWriting
        ]


type alias Id =
    String


isSelected : Id -> State -> Checkbox.IsSelected
isSelected id state =
    if Set.member id state.isChecked then
        Checkbox.Selected
    else
        Checkbox.NotSelected
