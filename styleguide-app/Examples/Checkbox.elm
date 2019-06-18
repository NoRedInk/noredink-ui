module Examples.Checkbox exposing (Msg, State, example, init, update)

{-|

@docs Msg, State, example, init, update

-}

import Css
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Checkbox.V5 as Checkbox
import Nri.Ui.Data.PremiumLevel as PremiumLevel exposing (PremiumLevel(..))
import Nri.Ui.PremiumCheckbox.V5 as PremiumCheckbox
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


type alias PremiumExampleConfig =
    { disabled : Bool
    , teacherPremiumLevel : PremiumLevel
    , pennant : PremiumCheckbox.Pennant
    }


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


viewMultilineCheckboxes : Html Msg
viewMultilineCheckboxes =
    Html.section
        [ css [ Css.width (Css.px 500) ] ]
        [ Html.h3 [] [ Html.text "Multiline Text in Checkboxes" ]
        , Checkbox.viewWithLabel
            { identifier = "fake"
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , setterMsg = ToggleCheck "fake"
            , selected = Checkbox.NotSelected
            , disabled = False
            , theme = Checkbox.Square
            }
        , Checkbox.viewWithLabel
            { identifier = "fake"
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , setterMsg = ToggleCheck "fake"
            , selected = Checkbox.PartiallySelected
            , disabled = True
            , theme = Checkbox.Square
            }
        , Checkbox.viewWithLabel
            { identifier = "fake"
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , setterMsg = ToggleCheck "fake"
            , selected = Checkbox.NotSelected
            , disabled = True
            , theme = Checkbox.Locked
            }
        , Checkbox.viewWithLabel
            { identifier = "fake"
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , setterMsg = ToggleCheck "fake"
            , selected = Checkbox.NotSelected
            , disabled = True
            , theme = Checkbox.Square
            }
        ]


viewPremiumCheckboxes : State -> Html Msg
viewPremiumCheckboxes state =
    let
        checkbox config =
            PremiumCheckbox.view
                { label = config.label
                , id = "premium-checkbox-" ++ config.label
                , selected =
                    if Set.member config.label state.isChecked then
                        Checkbox.Selected

                    else
                        Checkbox.NotSelected
                , disabled = config.disabled
                , isLocked = config.isLocked
                , pennant = config.pennant
                , onChange = ToggleCheck config.label
                , onLockedClick = NoOp
                }
    in
    Html.div []
        [ checkbox
            { label = "Identify Adjectives 2 (Premium)"
            , disabled = False
            , isLocked = False
            , pennant = PremiumCheckbox.Premium
            }
        , checkbox
            { label = "Revising Wordy Phrases 1 (Writing)"
            , disabled = False
            , isLocked = True
            , pennant = PremiumCheckbox.PremiumWithWriting
            }
        , checkbox
            { label = "Revising Wordy Phrases 2 (Writing) (Disabled)"
            , disabled = True
            , isLocked = True
            , pennant = PremiumCheckbox.PremiumWithWriting
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
