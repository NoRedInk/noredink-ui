module Examples.PremiumCheckbox exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import CheckboxIcons
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Data.PremiumDisplay as PremiumDisplay
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Pennant.V2 as Pennant
import Nri.Ui.PremiumCheckbox.V8 as PremiumCheckbox
import Nri.Ui.Svg.V1 as Svg
import Set exposing (Set)


moduleName : String
moduleName =
    "PremiumCheckbox"


version : Int
version =
    8


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
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
            , Heading.h2 [ Heading.plaintext "Premium Checkboxes" ]
            , viewPremiumCheckboxes state
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
                [ Pennant.premiumFlag
                    |> Svg.withCss [ Css.marginRight (Css.px 8) ]
                    |> Svg.withWidth (Css.px 25)
                    |> Svg.withHeight (Css.px 30)
                    |> Svg.toHtml
                , Svg.toHtml (Svg.withCss [ Css.marginRight (Css.px 8) ] icon)
                , text label_
                ]
    in
    [ ( CheckboxIcons.unchecked "unchecked-preview-unchecked", "Unchecked" )
    , ( CheckboxIcons.checkedPartially "checkedPartially-preview-checkedPartially", "Part checked" )
    , ( CheckboxIcons.checked "checkbox-preview-checked", "Checked" )
    , ( CheckboxIcons.lockOnInside "lockOnInside-preview-lockOnInside", "Locked" )
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


type alias Settings =
    {}


viewExample : State -> Settings -> Html Msg
viewExample state settings =
    text "TODO"


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
