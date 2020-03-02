module Examples.Checkbox exposing (Msg, State, example, init, update)

{-|

@docs Msg, State, example, init, update

-}

import Css
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Checkbox.V6 as Checkbox
import Nri.Ui.Data.PremiumLevel as PremiumLevel exposing (PremiumLevel(..))
import Nri.Ui.PremiumCheckbox.V6 as PremiumCheckbox
import Set exposing (Set)


{-| -}
type Msg
    = ToggleCheck Id Bool
    | NoOp


{-| -}
type alias State =
    { isChecked : Set String
    }


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { name = "Nri.Ui.Checkbox.V5"
    , category = Inputs
    , content =
        [ viewInteractableCheckbox "styleguide-checkbox-interactable" state
        , viewIndeterminateCheckbox "styleguide-checkbox-indeterminate" state
        , viewLockedOnInsideCheckbox "styleguide-locked-on-inside-checkbox" state
        , viewDisabledCheckbox "styleguide-checkbox-disabled" state
        , viewDisabledIndeterminateCheckbox "styleguide-checkbox-indeterminate-disabled" state
        , viewMultilineCheckboxes
        , h3 [] [ text "Premium Checkboxes" ]
        , viewPremiumCheckboxes state
        ]
            |> List.map (Html.map parentMessage)
    }


{-| -}
init : State
init =
    { isChecked = Set.empty
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

        NoOp ->
            ( state, Cmd.none )



-- INTERNAL


viewInteractableCheckbox : Id -> State -> Html Msg
viewInteractableCheckbox id state =
    Checkbox.checkbox
        { identifier = id
        , label = "This is an interactable checkbox!"
        , anonymous = True
        , onChange = ToggleCheck id
        , selected = isSelected id state
        }


viewIndeterminateCheckbox : Id -> State -> Html Msg
viewIndeterminateCheckbox id state =
    Checkbox.checkbox
        { identifier = id
        , label =  "This Checkbox is set in an indeterminate state"
        , anonymous = True
        , onChange = ToggleCheck id
        , selected = Checkbox.PartiallySelected
        }


viewLockedOnInsideCheckbox : Id -> State -> Html Msg
viewLockedOnInsideCheckbox id state =
    Checkbox.locked
        { identifier = id
        , label = "I'm a locked Checkbox"
        , anonymous = True
        , onClick = NoOp
        }


viewDisabledIndeterminateCheckbox : Id -> State -> Html Msg
viewDisabledIndeterminateCheckbox id state =
    Checkbox.disabled
        { identifier = id
        , label = "This disabled Checkbox is set in an indeterminate state"
        , anonymous = True
        , selected = Checkbox.PartiallySelected
        }


viewDisabledCheckbox : Id -> State -> Html Msg
viewDisabledCheckbox id state =
    Checkbox.disabled
        { identifier = id
        , label = "Disabled theme"
        , anonymous = True
        , selected = isSelected id state
        }


viewMultilineCheckboxes : Html Msg
viewMultilineCheckboxes =
    Html.section
        [ css [ Css.width (Css.px 500) ] ]
        [ Html.h3 [] [ Html.text "Multiline Text in Checkboxes" ]
        , Checkbox.checkbox
            { identifier = "fake-not-selected"
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , anonymous = True
            , onChange = ToggleCheck "fake-not-selected"
            , selected = Checkbox.NotSelected
            }
        , Checkbox.checkbox
            { identifier = "fake-partially-selected"
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , anonymous = True
            , onChange = ToggleCheck "fake"
            , selected = Checkbox.PartiallySelected
            }
        , Checkbox.locked
            { identifier = "fake-not-selected-locked"
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , anonymous = True
            , onClick = NoOp
            }
        , Checkbox.disabled
            { identifier = "fake-not-selected-square"
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , anonymous = True
            , selected = Checkbox.NotSelected
            }
        ]


viewPremiumCheckboxes : State -> Html Msg
viewPremiumCheckboxes state =
    let
        safeId =
            String.replace " " "-"

        checkbox config =
            PremiumCheckbox.view
                { label = config.label
                , id = "premium-checkbox-" ++ safeId config.label
                , selected =
                    if Set.member config.label state.isChecked then
                        Checkbox.Selected

                    else
                        Checkbox.NotSelected
                , disabled = config.disabled
                , isLocked = config.isLocked
                , isPremium = config.isPremium
                , onChange = ToggleCheck config.label
                , onLockedClick = NoOp
                }
    in
    Html.div []
        [ checkbox
            { label = "Identify Adjectives 2 (Premium)"
            , disabled = False
            , isLocked = False
            , isPremium = True
            }
        , checkbox
            { label = "Identify Adjectives 2 (Free)"
            , disabled = False
            , isLocked = False
            , isPremium = False
            }
        , checkbox
            { label = "Revising Wordy Phrases 2 (Premium, Disabled)"
            , disabled = True
            , isLocked = True
            , isPremium = True
            }
        ]


type alias Id =
    String


isSelected : Id -> State -> Checkbox.Selected
isSelected id state =
    if Set.member id state.isChecked then
        Checkbox.Selected

    else
        Checkbox.NotSelected
