module Examples.Checkbox exposing (Msg, State, example, init, update)

{-|

@docs Msg, State, example, init, update

-}

import Assets exposing (assets)
import Css
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Checkbox.V5 as Checkbox
import Nri.Ui.Data.PremiumLevel as PremiumLevel exposing (PremiumLevel(..))
import Nri.Ui.PremiumCheckbox.V4 as PremiumCheckbox
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
    { filename = "Nri.Ui.Checkbox.V5.elm"
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
    , pennant : Maybe PremiumCheckbox.Pennant
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


viewMultilineCheckboxes : Html Msg
viewMultilineCheckboxes =
    Html.section
        [ css [ Css.width (Css.px 500) ] ]
        [ Html.h3 [] [ Html.text "Multiline Text in Checkboxes" ]
        , Checkbox.viewWithLabel
            assets
            { identifier = "fake"
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , setterMsg = ToggleCheck "fake"
            , selected = Checkbox.NotSelected
            , disabled = False
            , theme = Checkbox.Square
            , noOpMsg = NoOp
            }
        , Checkbox.viewWithLabel
            assets
            { identifier = "fake"
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , setterMsg = ToggleCheck "fake"
            , selected = Checkbox.PartiallySelected
            , disabled = True
            , theme = Checkbox.Square
            , noOpMsg = NoOp
            }
        , Checkbox.viewWithLabel
            assets
            { identifier = "fake"
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , setterMsg = ToggleCheck "fake"
            , selected = Checkbox.NotSelected
            , disabled = True
            , theme = Checkbox.Locked
            , noOpMsg = NoOp
            }
        , Checkbox.viewWithLabel
            assets
            { identifier = "fake"
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , setterMsg = ToggleCheck "fake"
            , selected = Checkbox.NotSelected
            , disabled = True
            , theme = Checkbox.Square
            , noOpMsg = NoOp
            }
        ]


viewPremiumCheckboxes : State -> Html Msg
viewPremiumCheckboxes state =
    let
        checkbox config =
            PremiumCheckbox.premium
                assets
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
                , noOpMsg = NoOp
                }
    in
    Html.div []
        [ checkbox { label = "Identify Adjectives 1 (Free)", disabled = False, isLocked = False, pennant = Nothing }
        , checkbox { label = "Identify Adjectives 2 (Premium)", disabled = False, isLocked = False, pennant = Just PremiumCheckbox.Premium }
        , checkbox { label = "Revising Wordy Phrases 1 (Writing)", disabled = False, isLocked = True, pennant = Just PremiumCheckbox.PremiumWithWriting }
        , checkbox { label = "Revising Wordy Phrases 2 (Writing) (Disabled)", disabled = True, isLocked = True, pennant = Just PremiumCheckbox.PremiumWithWriting }
        ]


type alias Id =
    String


isSelected : Id -> State -> Checkbox.IsSelected
isSelected id state =
    if Set.member id state.isChecked then
        Checkbox.Selected

    else
        Checkbox.NotSelected
