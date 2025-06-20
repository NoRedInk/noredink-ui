module Nri.Ui.PremiumCheckbox.V8 exposing
    ( view
    , selected, partiallySelected
    , premium, onLockedClick
    , Attribute
    , disabled, enabled
    , id
    , setCheckboxContainerCss
    , setCheckboxEnabledLabelCss
    , setCheckboxDisabledLabelCss
    , noMargin
    )

{-|


## Patch changes:

  - Added vouchered state for premium vouchers with gift pennant
  - Replaced lock icon with disabled checkbox for locked state
  - Adjust height of locked Premium button to match other checkbox states
  - Support no-margin option


## Changes from V7:

  - Use PremiumDisplay instead of PremiumLevel
  - Rename showPennant to onLockedClick
  - Fix clicking on locked checkbox to send a onLockedClick
  - Exposes checkbox custom styling

@docs view

@docs selected, partiallySelected


### Content

@docs premium, onLockedClick


### Attributes

@docs Attribute
@docs disabled, enabled
@docs id


### Custom CSS

@docs setCheckboxContainerCss
@docs setCheckboxEnabledLabelCss
@docs setCheckboxDisabledLabelCss
@docs noMargin

-}

import Accessibility.Styled as Html exposing (Html)
import CheckboxIcons
import Css exposing (..)
import Html.Styled.Attributes as Attributes exposing (class, css)
import Html.Styled.Events as Events
import Nri.Ui.Checkbox.V7 as Checkbox
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Data.PremiumDisplay as PremiumDisplay exposing (PremiumDisplay)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as Extra
import Nri.Ui.Pennant.V4 exposing (contentPremiumFlag, giftPremiumFlag)
import Nri.Ui.Svg.V1 as Svg


{-| Set a custom ID for this checkbox and label. If you don't set this,
we'll automatically generate one from the label you pass in, but this can
cause problems if you have more than one checkbox with the same label on
the page. Use this to be more specific and avoid issues with duplicate IDs!
-}
id : String -> Attribute msg
id id_ =
    Attribute (\config -> { config | id = Just id_ })


{-| This disables the input
-}
disabled : Attribute msg
disabled =
    Attribute <| \config -> { config | isDisabled = True }


{-| This enables the input, this is the default behavior
-}
enabled : Attribute msg
enabled =
    Attribute <| \config -> { config | isDisabled = False }


{-| Lock Premium content if the user does not have Premium.
-}
premium : PremiumDisplay -> Attribute msg
premium premiumDisplay =
    Attribute <| \config -> { config | premiumDisplay = premiumDisplay }


{-| Show Premium pennant on Premium content.

When a locked premium checkbox is clicked, the msg that's passed in will fire.

-}
onLockedClick : msg -> Attribute msg
onLockedClick onLockedMsg =
    Attribute <| \config -> { config | onLockedMsg = Just onLockedMsg }


setSelectionStatus : Checkbox.IsSelected -> Attribute msg
setSelectionStatus status =
    Attribute (\config -> { config | selected = status })


{-| Set custom CSS for the checkbox container
-}
setCheckboxContainerCss : List Css.Style -> Attribute msg
setCheckboxContainerCss checkboxContainerCss =
    Attribute <| \config -> { config | checkboxContainerCss = checkboxContainerCss }


{-| Set custom CSS for the enabled checkbox label
-}
setCheckboxEnabledLabelCss : List Css.Style -> Attribute msg
setCheckboxEnabledLabelCss checkboxEnabledLabelCss =
    Attribute <| \config -> { config | checkboxEnabledLabelCss = checkboxEnabledLabelCss }


{-| Set custom CSS for the disabled checkbox label
-}
setCheckboxDisabledLabelCss : List Css.Style -> Attribute msg
setCheckboxDisabledLabelCss checkboxDisabledLabelCss =
    Attribute <| \config -> { config | checkboxDisabledLabelCss = checkboxDisabledLabelCss }


{-| Remove default spacing from the top and bottom of the checkbox.
-}
noMargin : Bool -> Attribute value
noMargin removeMargin =
    Attribute <| \config -> { config | removeMargin = removeMargin }


{-| -}
selected : Bool -> Attribute msg
selected isSelected =
    setSelectionStatus <|
        if isSelected then
            Checkbox.Selected

        else
            Checkbox.NotSelected


{-| -}
partiallySelected : Attribute msg
partiallySelected =
    setSelectionStatus Checkbox.PartiallySelected


{-| Customizations for the RadioButton.
-}
type Attribute msg
    = Attribute (Config msg -> Config msg)


{-| This is private. The public API only exposes `Attribute`.
-}
type alias Config msg =
    { id : Maybe String
    , premiumDisplay : PremiumDisplay
    , isDisabled : Bool
    , containerCss : List Css.Style
    , selected : Checkbox.IsSelected
    , onLockedMsg : Maybe msg
    , checkboxContainerCss : List Css.Style
    , checkboxEnabledLabelCss : List Css.Style
    , checkboxDisabledLabelCss : List Css.Style
    , removeMargin : Bool
    }


emptyConfig : Config msg
emptyConfig =
    { id = Nothing
    , premiumDisplay = PremiumDisplay.Free
    , isDisabled = False
    , containerCss =
        [ Css.displayFlex
        , Css.alignItems Css.center
        ]
    , selected = Checkbox.NotSelected
    , onLockedMsg = Nothing
    , checkboxContainerCss = []
    , checkboxEnabledLabelCss = []
    , checkboxDisabledLabelCss = []
    , removeMargin = False
    }


applyConfig : List (Attribute msg) -> Config msg -> Config msg
applyConfig attributes beginningConfig =
    List.foldl (\(Attribute update) config -> update config)
        beginningConfig
        attributes


{-| -}
view :
    { label : String
    , onChange : Bool -> msg
    }
    -> List (Attribute msg)
    -> Html msg
view { label, onChange } attributes =
    let
        config =
            applyConfig attributes emptyConfig

        idValue =
            case config.id of
                Just specificId ->
                    specificId

                Nothing ->
                    Extra.safeIdWithPrefix "checkbox" label

        isPremium =
            config.premiumDisplay /= PremiumDisplay.Free

        isLocked =
            config.premiumDisplay == PremiumDisplay.PremiumLocked

        isVouchered =
            config.premiumDisplay == PremiumDisplay.PremiumVouchered
    in
    if isLocked then
        viewLockedButton
            { idValue = idValue
            , label = label
            , containerCss = config.containerCss
            , onLockedMsg = config.onLockedMsg
            , removeMargin = config.removeMargin
            }

    else
        Html.div [ css config.containerCss ]
            [ if isVouchered then
                viewPremiumFlag giftPremiumFlag

              else if isPremium then
                viewPremiumFlag contentPremiumFlag

              else
                -- left-align the checkbox with checkboxes that _do_ have the premium pennant
                Html.div [ css [ Css.width (Css.px (iconWidth + iconRightMargin)), Css.flexShrink Css.zero ] ] []
            , Checkbox.view
                { label = label
                , selected = config.selected
                }
                [ Checkbox.id idValue
                , Checkbox.onCheck onChange
                , if config.isDisabled then
                    Checkbox.disabled

                  else
                    Checkbox.enabled
                , Checkbox.containerCss config.checkboxContainerCss
                , if config.isDisabled then
                    Checkbox.labelCss config.checkboxDisabledLabelCss

                  else
                    Checkbox.labelCss config.checkboxEnabledLabelCss
                , Checkbox.noMargin config.removeMargin
                ]
            ]


viewLockedButton :
    { a
        | idValue : String
        , label : String
        , containerCss : List Style
        , onLockedMsg : Maybe msg
        , removeMargin : Bool
    }
    -> Html msg
viewLockedButton { idValue, label, containerCss, onLockedMsg, removeMargin } =
    Html.button
        [ css
            [ height inherit
            , width (Css.pct 100)
            , position relative
            , backgroundColor Css.transparent
            , border Css.zero
            , if removeMargin then
                padding zero

              else
                padding2 (px 9) zero
            , cursor pointer
            , Css.batch containerCss
            , textAlign left
            ]
        , Attributes.id (idValue ++ "-container")
        , case onLockedMsg of
            Just msg ->
                Events.onClick msg

            Nothing ->
                Extra.none
        ]
        [ viewPremiumFlag contentPremiumFlag
        , Html.span
            [ css
                [ outline Css.none
                , Fonts.baseFont
                , fontSize (px 15)
                , display inlineBlock
                , Css.property "transition" "all 0.4s ease"
                , cursor pointer
                ]
            ]
            [ Html.span
                [ class "premium-checkbox-locked-V8__Label"
                , css
                    [ displayFlex
                    , alignItems center
                    , cursor notAllowed
                    , position relative
                    , Fonts.baseFont
                    , fontSize (px 15)
                    ]
                ]
                [ Checkbox.viewIcon [] CheckboxIcons.uncheckedDisabled
                , Html.span
                    [ css [ color Colors.gray45 ]
                    ]
                    [ Html.text label ]
                ]
            ]
        ]


viewPremiumFlag : Svg.Svg -> Html msg
viewPremiumFlag icon =
    icon
        |> Svg.withLabel "Premium"
        |> Svg.withWidth (Css.px iconWidth)
        |> Svg.withHeight
            -- The checkbox is 27px by 27px, which we should match
            (Css.px 27)
        |> Svg.withCss [ Css.marginRight (Css.px iconRightMargin), Css.flexShrink Css.zero ]
        |> Svg.toHtml


iconWidth : Float
iconWidth =
    17


iconRightMargin : Float
iconRightMargin =
    10
