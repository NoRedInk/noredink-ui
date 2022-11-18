module Nri.Ui.Checkbox.V7 exposing
    ( view
    , onCheck
    , Attribute, IsSelected(..)
    , hiddenLabel, visibleLabel
    , selectedFromBool
    , containerCss, labelCss, custom, nriDescription, id, testId
    , disabled, enabled, guidance
    , viewIcon
    )

{-|


# Changes from V6:

  - Reworked api similar to other components based on Attributes
  - Add support for guidance
  - Dropped checkboxLockOnInside functionality
  - Dropped disabledLabelCss functionality. Use labelCss instead in case when the checkbox is disabled.
  - (breaking-change) By default the label is visible (ie: V6.viewWithLabel), use hiddenLabel to migrate from V6.view.

@docs view


### Event handlers

@docs onCheck


### Attributes

@docs Attribute, IsSelected
@docs hiddenLabel, visibleLabel
@docs selectedFromBool
@docs containerCss, labelCss, custom, nriDescription, id, testId
@docs disabled, enabled, guidance


### Internal

@docs viewIcon

-}

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Style
import CheckboxIcons
import Css exposing (..)
import Css.Global
import Html.Styled as Html
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import InputErrorAndGuidanceInternal exposing (ErrorState, Guidance)
import Json.Decode
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as Extra
import Nri.Ui.Svg.V1 exposing (Svg)
import Nri.Ui.Util as Util


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


{-| A guidance message shows below the input, unless an error message is showing instead.
-}
guidance : String -> Attribute msg
guidance =
    Attribute << InputErrorAndGuidanceInternal.setGuidance


{-| Fire a message when toggling the checkbox.
-}
onCheck : (Bool -> msg) -> Attribute msg
onCheck onCheck_ =
    Attribute <| \config -> { config | onCheck = Just onCheck_ }


{-| Adds CSS to the element containing the input.
-}
containerCss : List Css.Style -> Attribute msg
containerCss styles =
    Attribute <| \config -> { config | containerCss = config.containerCss ++ styles }


{-| Adds CSS to the element containing the label text.

Note that these styles don't apply to the literal HTML label element, since it contains the icon SVG as well.

-}
labelCss : List Css.Style -> Attribute msg
labelCss styles =
    Attribute <| \config -> { config | labelCss = config.labelCss ++ styles }


{-| Hides the visible label. (There will still be an invisible label for screen readers.)
-}
hiddenLabel : Attribute msg
hiddenLabel =
    Attribute <| \config -> { config | hideLabel = True }


{-| Shows the visible label. This is the default behavior
-}
visibleLabel : Attribute msg
visibleLabel =
    Attribute <| \config -> { config | hideLabel = False }


{-| Set a custom ID for this checkbox input and label. If you don't set this,
we'll automatically generate one from the label you pass in, but this can
cause problems if you have more than one checkbox input with the same label on
the page. You might also use this helper if you're manually managing focus.
-}
id : String -> Attribute msg
id id_ =
    Attribute <| \config -> { config | id = Just id_ }


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

-}
custom : List (Html.Attribute Never) -> Attribute msg
custom attributes =
    Attribute <| \config -> { config | custom = config.custom ++ attributes }


{-| -}
nriDescription : String -> Attribute msg
nriDescription description =
    custom [ Extra.nriDescription description ]


{-| -}
testId : String -> Attribute msg
testId id_ =
    custom [ Extra.testId id_ ]


{-| Customizations for the Checkbox.
-}
type Attribute msg
    = Attribute (Config msg -> Config msg)


{-| This is private. The public API only exposes `Attribute`.
-}
type alias Config msg =
    { id : Maybe String
    , hideLabel : Bool
    , onCheck : Maybe (Bool -> msg)
    , isDisabled : Bool
    , guidance : Guidance
    , custom : List (Html.Attribute Never)
    , containerCss : List Css.Style
    , labelCss : List Css.Style
    }


{-|

    = Selected --  Checked (rendered with a checkmark)
    | NotSelected -- Not Checked (rendered blank)
    | PartiallySelected -- Indeterminate (rendered dash)

-}
type IsSelected
    = Selected
    | NotSelected
    | PartiallySelected


emptyConfig : Config msg
emptyConfig =
    { id = Nothing
    , hideLabel = False
    , onCheck = Nothing
    , isDisabled = False
    , guidance = InputErrorAndGuidanceInternal.noGuidance
    , custom = []
    , containerCss = []
    , labelCss = []
    }


applyConfig : List (Attribute msg) -> Config msg -> Config msg
applyConfig attributes beginningConfig =
    List.foldl (\(Attribute update) config -> update config)
        beginningConfig
        attributes


{-| View a checkbox (the label is only used for accessibility hints unless visibleLabel attribute is applied)
-}
view :
    { label : String
    , selected : IsSelected
    }
    -> List (Attribute msg)
    -> Html msg
view { label, selected } attributes =
    let
        config =
            applyConfig attributes emptyConfig

        idValue =
            case config.id of
                Just specificId ->
                    specificId

                Nothing ->
                    "checkbox-v7-" ++ Util.safeIdString label

        config_ =
            { identifier = idValue
            , containerCss = config.containerCss
            , label = label
            , hideLabel = config.hideLabel
            , labelCss = config.labelCss
            , onCheck = config.onCheck
            , selected = selected
            , disabled = config.isDisabled
            , guidance = config.guidance
            , error = InputErrorAndGuidanceInternal.noError
            }
    in
    checkboxContainer config_
        [ viewCheckbox config_
        , let
            ( icon, disabledIcon ) =
                case selected of
                    Selected ->
                        ( CheckboxIcons.checked idValue
                        , CheckboxIcons.checkedDisabled
                        )

                    NotSelected ->
                        ( CheckboxIcons.unchecked idValue
                        , CheckboxIcons.uncheckedDisabled
                        )

                    PartiallySelected ->
                        ( CheckboxIcons.checkedPartially idValue
                        , CheckboxIcons.checkedPartiallyDisabled
                        )
          in
          if config.isDisabled then
            viewDisabledLabel config_ disabledIcon

          else
            viewEnabledLabel config_ icon
        , InputErrorAndGuidanceInternal.view config_.identifier (Css.marginTop Css.zero) config_
        ]


{-| If your selectedness is always selected or not selected,
you will likely store that state as a `Bool` in your model.
`selectedFromBool` lets you easily convert that into an `IsSelected` value
for use with `Nri.Ui.Checkbox`.
-}
selectedFromBool : Bool -> IsSelected
selectedFromBool isSelected =
    if isSelected then
        Selected

    else
        NotSelected


selectedToMaybe : IsSelected -> Maybe Bool
selectedToMaybe selected =
    case selected of
        Selected ->
            Just True

        NotSelected ->
            Just False

        PartiallySelected ->
            Nothing


checkboxContainer : { a | identifier : String, containerCss : List Style } -> List (Html msg) -> Html msg
checkboxContainer model =
    Html.span
        [ css
            [ display block
            , height inherit
            , position relative
            , marginLeft (px -4)
            , padding4 (px 13) zero (px 13) (px 40)
            , pseudoClass "focus-within"
                [ Css.Global.descendants
                    [ Css.Global.class "checkbox-icon-container" FocusRing.tightStyles
                    ]
                ]
            , Css.Global.descendants
                [ Css.Global.input
                    [ position absolute
                    , top (calc (pct 50) minus (px 10))
                    , left (px 10)
                    , boxShadow none |> Css.important
                    ]
                ]
            , Css.batch model.containerCss
            ]
        , Attributes.id (model.identifier ++ "-container")
        , Events.stopPropagationOn "click" (Json.Decode.fail "stop click propagation")
        ]


viewCheckbox :
    { a
        | identifier : String
        , onCheck : Maybe (Bool -> msg)
        , selected : IsSelected
        , disabled : Bool
    }
    -> Html.Html msg
viewCheckbox config =
    checkbox config.identifier
        (selectedToMaybe config.selected)
        (List.filterMap identity <|
            [ Just <| Attributes.id config.identifier
            , if config.disabled then
                Just <| Aria.disabled True

              else
                config.onCheck
                    |> Maybe.map (onCheckMsg config.selected)
                    |> Maybe.map (\msg -> Events.onCheck (\_ -> msg))
            ]
        )


onCheckMsg : IsSelected -> (Bool -> msg) -> msg
onCheckMsg selected msg =
    selectedToMaybe selected
        |> Maybe.withDefault False
        |> not
        |> msg


viewEnabledLabel :
    { a
        | identifier : String
        , selected : IsSelected
        , label : String
        , hideLabel : Bool
        , labelCss : List Style
    }
    -> Svg
    -> Html.Html msg
viewEnabledLabel config icon =
    Html.label
        [ Attributes.for config.identifier
        , labelClass config.selected
        , css
            [ display inlineBlock
            , textStyle
            , cursor pointer
            , Css.batch config.labelCss
            ]
        ]
        [ viewIcon [] icon
        , labelView config
        ]


viewDisabledLabel :
    { a
        | identifier : String
        , selected : IsSelected
        , label : String
        , hideLabel : Bool
        , labelCss : List Style
    }
    -> Svg
    -> Html.Html msg
viewDisabledLabel config icon =
    Html.label
        [ Attributes.for config.identifier
        , labelClass config.selected
        , css
            [ display inlineBlock
            , textStyle
            , outline none
            , cursor auto
            , color Colors.gray45
            , Css.batch config.labelCss
            ]
        ]
        [ viewIcon [] icon
        , labelView config
        ]


labelClass : IsSelected -> Html.Attribute msg
labelClass isSelected =
    case isSelected of
        Selected ->
            toClassList [ "Label", "Checked" ]

        NotSelected ->
            toClassList [ "Label", "Unchecked" ]

        PartiallySelected ->
            toClassList [ "Label", "Indeterminate" ]


toClassList : List String -> Html.Attribute msg
toClassList =
    List.map (\a -> ( "checkbox-V7__" ++ a, True )) >> Attributes.classList


textStyle : Style
textStyle =
    batch
        [ Fonts.baseFont
        , fontSize (px 15)
        , fontWeight (int 600)
        , color Colors.navy
        ]


{-| -}
viewIcon : List Style -> Svg -> Html msg
viewIcon styles icon =
    Html.div
        [ css
            [ position absolute
            , left zero
            , top (calc (pct 50) minus (px 18))
            , border3 (px 2) solid transparent
            , padding (px 2)
            , borderRadius (px 3)
            , height (Css.px 27)
            , boxSizing contentBox
            ]
        , Attributes.class "checkbox-icon-container"
        ]
        [ Html.div
            [ css
                [ display inlineBlock
                , backgroundColor Colors.white
                , height (Css.px 27)
                , borderRadius (px 4)
                ]
            ]
            [ Nri.Ui.Svg.V1.toHtml (Nri.Ui.Svg.V1.withCss styles icon)
            ]
        ]


labelView : { a | hideLabel : Bool, label : String } -> Html msg
labelView config =
    if config.hideLabel then
        Html.span Accessibility.Styled.Style.invisible
            [ Html.text config.label ]

    else
        Html.span [] [ Html.text config.label ]
