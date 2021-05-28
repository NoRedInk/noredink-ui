module Examples.Checkbox exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Checkbox.V5 as Checkbox
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
example : Example State Msg
example =
    { name = "Checkbox"
    , version = 5
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view =
        \state ->
            [ viewInteractableCheckbox "styleguide-checkbox-interactable" state
            , viewIndeterminateCheckbox "styleguide-checkbox-indeterminate" state
            , viewLockedOnInsideCheckbox "styleguide-locked-on-inside-checkbox" state
            , viewDisabledCheckbox "styleguide-checkbox-disabled" state
            , viewMultilineCheckboxes state
            , h3 [] [ text "Premium Checkboxes" ]
            , viewPremiumCheckboxes state
            ]
    , categories = [ Inputs ]
    , keyboardSupport =
        [ { keys = [ Space ]
          , result = "Select or deselect the checkbox (may cause page scroll)"
          }
        ]
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
    Checkbox.viewWithLabel
        { identifier = id
        , label = "This is an interactable checkbox!"
        , setterMsg = ToggleCheck id
        , selected = isSelected id state
        , disabled = False
        , theme = Checkbox.Square
        }


viewIndeterminateCheckbox : Id -> State -> Html Msg
viewIndeterminateCheckbox id state =
    Checkbox.viewWithLabel
        { identifier = id
        , label = "This Checkbox is set in an indeterminate state"
        , setterMsg = ToggleCheck id
        , selected = Checkbox.PartiallySelected
        , disabled = True
        , theme = Checkbox.Square
        }


viewLockedOnInsideCheckbox : Id -> State -> Html Msg
viewLockedOnInsideCheckbox id state =
    Checkbox.viewWithLabel
        { identifier = id
        , label = "I'm a locked Checkbox"
        , setterMsg = ToggleCheck id
        , selected = Checkbox.NotSelected
        , disabled = True
        , theme = Checkbox.Locked
        }


viewDisabledCheckbox : Id -> State -> Html Msg
viewDisabledCheckbox id state =
    Checkbox.viewWithLabel
        { identifier = id
        , label = "Disabled theme"
        , setterMsg = ToggleCheck id
        , selected = isSelected id state
        , disabled = True
        , theme = Checkbox.Square
        }


viewMultilineCheckboxes : State -> Html Msg
viewMultilineCheckboxes state =
    Html.section
        [ css [ Css.width (Css.px 500) ] ]
        [ Html.h3 [] [ Html.text "Multiline Text in Checkboxes" ]
        , let
            id =
                "styleguide-checkbox-multiline"
          in
          Checkbox.viewWithLabel
            { identifier = id
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , setterMsg = ToggleCheck id
            , selected = isSelected id state
            , disabled = False
            , theme = Checkbox.Square
            }
        , Checkbox.viewWithLabel
            { identifier = "fake-partially-selected"
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , setterMsg = ToggleCheck "fake"
            , selected = Checkbox.PartiallySelected
            , disabled = True
            , theme = Checkbox.Square
            }
        , Checkbox.viewWithLabel
            { identifier = "fake-not-selected-locked"
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , setterMsg = ToggleCheck "fake"
            , selected = Checkbox.NotSelected
            , disabled = True
            , theme = Checkbox.Locked
            }
        , Checkbox.viewWithLabel
            { identifier = "fake-not-selected-square"
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , setterMsg = ToggleCheck "fake"
            , selected = Checkbox.NotSelected
            , disabled = True
            , theme = Checkbox.Square
            }
        ]


viewPremiumCheckboxes : State -> Html Msg
viewPremiumCheckboxes state =
    Html.div []
        [ PremiumCheckbox.view
            { label = "Identify Adjectives 1 (Premium)"
            , id = "premium-checkbox-identify-adjectives-premium"
            , selected =
                if Set.member "premium-1" state.isChecked then
                    Checkbox.Selected

                else
                    Checkbox.NotSelected
            , disabled = False
            , isLocked = False
            , isPremium = True
            , onChange = ToggleCheck "premium-1"
            , onLockedClick = NoOp
            }
        , PremiumCheckbox.view
            { label = "Identify Adjectives 2 (Free)"
            , id = "premium-checkbox-identify-adjectives-free"
            , selected =
                if Set.member "premium-2" state.isChecked then
                    Checkbox.Selected

                else
                    Checkbox.NotSelected
            , disabled = False
            , isLocked = False
            , isPremium = False
            , onChange = ToggleCheck "premium-2"
            , onLockedClick = NoOp
            }
        , PremiumCheckbox.view
            { label = "Revising Wordy Phrases 3 (Premium, Disabled)"
            , id = "premium-checkbox-premium-disabled"
            , selected =
                if Set.member "premium-3" state.isChecked then
                    Checkbox.Selected

                else
                    Checkbox.NotSelected
            , disabled = True
            , isLocked = True
            , isPremium = True
            , onChange = ToggleCheck "premium-3"
            , onLockedClick = NoOp
            }
        ]


type alias Id =
    String


isSelected : Id -> State -> Checkbox.IsSelected
isSelected id state =
    if Set.member id state.isChecked then
        Checkbox.Selected

    else
        Checkbox.NotSelected
