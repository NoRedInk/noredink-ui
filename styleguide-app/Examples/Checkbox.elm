module Examples.Checkbox exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import CheckboxIcons
import Css exposing (Style)
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Checkbox.V6 as Checkbox
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Data.PremiumDisplay as PremiumDisplay
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.PremiumCheckbox.V8 as PremiumCheckbox
import Nri.Ui.Svg.V1 as Svg
import Set exposing (Set)


moduleName : String
moduleName =
    "Checkbox"


version : Int
version =
    6


{-| -}
example : Example State Msg
example =
    { name = "Checkbox"
    , version = 6
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = preview
    , view =
        \ellieLinkConfig state ->
            let
                settings =
                    Control.currentValue state.settings
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControls
                , settings = state.settings
                , mainType = Nothing
                , extraCode = []
                , toExampleCode = \_ -> [ { sectionName = "TODO", code = "TODO" } ]
                }
            , Heading.h2 [ Heading.plaintext "Example" ]
            , viewExample state settings
            , viewInteractableCheckbox "styleguide-checkbox-interactable" state
            , viewIndeterminateCheckbox "styleguide-checkbox-indeterminate" state
            , viewLockedOnInsideCheckbox "styleguide-locked-on-inside-checkbox" state
            , viewDisabledCheckbox "styleguide-checkbox-disabled" state
            , viewMultilineCheckboxes state
            , Heading.h2 [ Heading.plaintext "Premium Checkboxes" ]
            , viewPremiumCheckboxes state
            , viewCustomStyledCheckbox state
            , viewCustomStyledPremiumCheckboxes state
            ]
    , categories = [ Inputs ]
    , keyboardSupport =
        [ { keys = [ Space ]
          , result = "Select or deselect the checkbox (may cause page scroll)"
          }
        ]
    }


preview : List (Html Never)
preview =
    let
        renderPreview ( icon, label_ ) =
            span
                [ css
                    [ Css.color Colors.navy
                    , Css.fontSize (Css.px 15)
                    , Fonts.baseFont
                    , Css.fontWeight (Css.int 600)
                    , Css.displayFlex
                    , Css.alignItems Css.center
                    ]
                ]
                [ Svg.toHtml (Svg.withCss [ Css.marginRight (Css.px 8) ] icon)
                , text label_
                ]
    in
    [ ( CheckboxIcons.lockOnInside "lockOnInside-preview-lockOnInside", "Locked" )
    , ( CheckboxIcons.unchecked "unchecked-preview-unchecked", "Unchecked" )
    , ( CheckboxIcons.checkedPartially "checkedPartially-preview-checkedPartially", "Part checked" )
    , ( CheckboxIcons.checked "checkbox-preview-checked", "Checked" )
    ]
        |> List.map renderPreview


{-| -}
type alias State =
    { isChecked : Set String
    , settings : Control Settings
    }


{-| -}
init : State
init =
    { isChecked = Set.empty
    , settings = controlSettings
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "label" (Control.string "Enable Text to Speech")
        |> Control.field "disabled" (Control.bool False)
        |> Control.field "theme"
            (Control.choice
                [ ( "Square", Control.value Checkbox.Square )
                , ( "Locked", Control.value Checkbox.Locked )
                ]
            )
        |> Control.field "containerCss"
            (Control.choice
                [ ( "[]", Control.value [] )
                , ( "Red dashed border", Control.value [ Css.border3 (Css.px 4) Css.dashed Colors.red ] )
                ]
            )
        |> Control.field "enabledLabelCss"
            (Control.choice
                [ ( "[]", Control.value [] )
                , ( "Orange dotted border", Control.value [ Css.border3 (Css.px 4) Css.dotted Colors.orange ] )
                ]
            )
        |> Control.field "disabledLabelCss"
            (Control.choice
                [ ( "[]", Control.value [] )
                , ( "strikethrough", Control.value [ Css.textDecoration Css.lineThrough ] )
                ]
            )


type alias Settings =
    { label : String
    , disabled : Bool
    , theme : Checkbox.Theme
    , containerCss : List Style
    , enabledLabelCss : List Style
    , disabledLabelCss : List Style
    }


viewExample : State -> Settings -> Html Msg
viewExample state settings =
    let
        id =
            "unique-example-id"
    in
    Checkbox.viewWithLabel
        { identifier = id
        , label = settings.label
        , setterMsg = ToggleCheck id
        , selected = isSelected id state
        , disabled = settings.disabled
        , theme = settings.theme
        , containerCss = settings.containerCss
        , enabledLabelCss = settings.enabledLabelCss
        , disabledLabelCss = settings.disabledLabelCss
        }


{-| -}
type Msg
    = ToggleCheck Id Bool
    | UpdateControls (Control Settings)
    | NoOp


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

        UpdateControls settings ->
            ( { state | settings = settings }, Cmd.none )

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
        , containerCss = []
        , enabledLabelCss = []
        , disabledLabelCss = []
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
        , containerCss = []
        , enabledLabelCss = []
        , disabledLabelCss = []
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
        , containerCss = []
        , enabledLabelCss = []
        , disabledLabelCss = []
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
        , containerCss = []
        , enabledLabelCss = []
        , disabledLabelCss = []
        }


viewMultilineCheckboxes : State -> Html Msg
viewMultilineCheckboxes state =
    Html.section
        [ css [ Css.width (Css.px 500) ] ]
        [ Heading.h2 [ Heading.plaintext "Multiline Text in Checkboxes" ]
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
            , containerCss = []
            , enabledLabelCss = []
            , disabledLabelCss = []
            }
        , Checkbox.viewWithLabel
            { identifier = "fake-partially-selected"
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , setterMsg = ToggleCheck "fake"
            , selected = Checkbox.PartiallySelected
            , disabled = True
            , theme = Checkbox.Square
            , containerCss = []
            , enabledLabelCss = []
            , disabledLabelCss = []
            }
        , Checkbox.viewWithLabel
            { identifier = "fake-not-selected-locked"
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , setterMsg = ToggleCheck "fake"
            , selected = Checkbox.NotSelected
            , disabled = True
            , theme = Checkbox.Locked
            , containerCss = []
            , enabledLabelCss = []
            , disabledLabelCss = []
            }
        , Checkbox.viewWithLabel
            { identifier = "fake-not-selected-square"
            , label = "Ut nobis et vel. Nulla rerum sit eos accusamus placeat. Iure sunt earum voluptatibus autem ratione soluta sint.\n\nIste perferendis eum corporis ullam magnam incidunt eos."
            , setterMsg = ToggleCheck "fake"
            , selected = Checkbox.NotSelected
            , disabled = True
            , theme = Checkbox.Square
            , containerCss = []
            , enabledLabelCss = []
            , disabledLabelCss = []
            }
        ]


viewPremiumCheckboxes : State -> Html Msg
viewPremiumCheckboxes state =
    Html.div []
        [ PremiumCheckbox.view
            { label = "Identify Adjectives 1 (Premium, Unlocked)"
            , onChange = ToggleCheck "premium-1"
            }
            [ PremiumCheckbox.premium PremiumDisplay.PremiumUnlocked
            , PremiumCheckbox.onLockedClick NoOp
            , PremiumCheckbox.selected (Set.member "premium-1" state.isChecked)
            ]
        , PremiumCheckbox.view
            { label = "Identify Adjectives 2 (Free)"
            , onChange = ToggleCheck "premium-2"
            }
            [ PremiumCheckbox.premium PremiumDisplay.Free
            , PremiumCheckbox.onLockedClick NoOp
            , PremiumCheckbox.selected (Set.member "premium-2" state.isChecked)
            ]
        , PremiumCheckbox.view
            { label = "Revising Wordy Phrases 3 (Premium, Locked)"
            , onChange = ToggleCheck "premium-3"
            }
            [ PremiumCheckbox.premium PremiumDisplay.PremiumLocked
            , PremiumCheckbox.onLockedClick (Debug.log "Locked" NoOp)
            , PremiumCheckbox.selected (Set.member "premium-3" state.isChecked)
            ]
        ]


viewCustomStyledCheckbox : State -> Html Msg
viewCustomStyledCheckbox state =
    Html.section
        [ css [ Css.width (Css.px 500) ] ]
        [ Heading.h2 [ Heading.plaintext "Custom-styled Checkboxes" ]
        , let
            id =
                "styleguide-checkbox-custom-style"
          in
          Checkbox.viewWithLabel
            { identifier = id
            , label = "This is a custom-styled Checkbox"
            , setterMsg = ToggleCheck id
            , selected = isSelected id state
            , disabled = False
            , theme = Checkbox.Square
            , containerCss = [ Css.backgroundColor Colors.navy ]
            , enabledLabelCss = [ Css.color Colors.white ]
            , disabledLabelCss = []
            }
        ]


viewCustomStyledPremiumCheckboxes : State -> Html Msg
viewCustomStyledPremiumCheckboxes state =
    Html.section
        [ css [ Css.width (Css.px 500) ] ]
        [ Heading.h2 [ Heading.plaintext "Custom-styled Premium Checkboxes" ]
        , PremiumCheckbox.view
            { label = "This is a custom-styled Premium Checkbox"
            , onChange = ToggleCheck "premium-custom"
            }
            [ PremiumCheckbox.premium PremiumDisplay.PremiumUnlocked
            , PremiumCheckbox.onLockedClick NoOp
            , PremiumCheckbox.selected (Set.member "premium-custom" state.isChecked)
            , PremiumCheckbox.setCheckboxContainerCss [ Css.backgroundColor Colors.navy ]
            , PremiumCheckbox.setCheckboxEnabledLabelCss [ Css.color Colors.white ]
            ]
        ]


type alias Id =
    String


isSelected : Id -> State -> Checkbox.IsSelected
isSelected id state =
    if Set.member id state.isChecked then
        Checkbox.Selected

    else
        Checkbox.NotSelected
