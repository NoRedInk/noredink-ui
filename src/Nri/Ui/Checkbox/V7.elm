module Nri.Ui.Checkbox.V7 exposing
    ( view
    , onCheck
    , Attribute, IsSelected(..)
    , hiddenLabel, visibleLabel
    , selectedFromBool
    , containerCss, labelCss, custom, nriDescription, id, testId
    , disabled, enabled, guidance, guidanceHtml
    , viewIcon
    )

{-|


## Patch changes:

  - reposition the guidance to be right below the label text
  - fix the hiddenLabel checkbox behavior
  - fix the disabled styles
  - fix "checkboxes can’t be clicked in the lower half when there’s guidance" issue
  - fix duplicative focus ring issue


## Changes from V6:

  - Reworked api similar to other components based on Attributes
  - Add support for guidance
  - Dropped checkboxLockOnInside functionality
  - Dropped disabledLabelCss functionality. Use labelCss instead in case when the checkbox is disabled.
  - (breaking-change) By default the label is visible (ie: V6.viewWithLabel), use hiddenLabel to migrate from V6.view.


## Patch changes:

  - It turns out that "indeterminate" has to be set from JS -- it doesn't work to add the attribute as part of the HTML. So, instead of using an input under the hood, we're using aria-attributes instead. This also allows us to simplify the styles a bit.

@docs view


### Event handlers

@docs onCheck


### Attributes

@docs Attribute, IsSelected
@docs hiddenLabel, visibleLabel
@docs selectedFromBool
@docs containerCss, labelCss, custom, nriDescription, id, testId
@docs disabled, enabled, guidance, guidanceHtml


### Internal

@docs viewIcon

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Style
import CheckboxIcons
import Css exposing (..)
import Css.Global
import EventExtras
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import InputErrorAndGuidanceInternal exposing (Guidance)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as Extra
import Nri.Ui.Svg.V1 exposing (Svg)


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


{-| A guidance message (HTML) shows below the input, unless an error message is showing instead.
-}
guidanceHtml : List (Html msg) -> Attribute msg
guidanceHtml =
    Attribute << InputErrorAndGuidanceInternal.setGuidanceHtml


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
custom : List (Html.Attribute msg) -> Attribute msg
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
    , guidance : Guidance msg
    , custom : List (Html.Attribute msg)
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
                    Extra.safeIdWithPrefix "checkbox-v7" label

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
            , custom = config.custom
            , error = InputErrorAndGuidanceInternal.noError
            }

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
    checkboxContainer config_
        [ if config.isDisabled then
            div [] [ viewIcon [] disabledIcon ]

          else
            -- ensure the entire checkbox icon is always clickable
            div
                (config.onCheck
                    |> Maybe.map (onCheckMsg config_.selected)
                    |> Maybe.map
                        (\msg ->
                            [ EventExtras.onClickStopPropagation msg
                            , css [ cursor pointer ]
                            ]
                        )
                    |> Maybe.withDefault []
                )
                [ viewIcon [] icon ]
        , span []
            (viewCheckboxLabel config_
                (if config.isDisabled then
                    disabledLabelCss

                 else
                    enabledLabelCss
                )
                :: inputGuidance config_
            )
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
            [ displayFlex
            , alignItems center
            , marginLeft (px -4)
            , paddingTop (px 5)
            , paddingBottom (px 5)
            , height inherit
            , pseudoClass "focus-within"
                [ Css.Global.descendants
                    [ Css.Global.class "checkbox-icon-container" FocusRing.tightStyles
                    ]
                ]
            , Css.batch model.containerCss
            ]
        , Attributes.id (model.identifier ++ "-container")
        ]


onCheckMsg : IsSelected -> (Bool -> msg) -> msg
onCheckMsg selected msg =
    selectedToMaybe selected
        |> Maybe.withDefault False
        |> not
        |> msg


enabledLabelCss : List Style
enabledLabelCss =
    [ textStyle
    , cursor pointer
    ]


disabledLabelCss : List Style
disabledLabelCss =
    [ textStyle
    , cursor auto
    , color Colors.gray45
    ]


viewCheckboxLabel :
    { a
        | identifier : String
        , selected : IsSelected
        , onCheck : Maybe (Bool -> msg)
        , disabled : Bool
        , label : String
        , hideLabel : Bool
        , labelCss : List Style
        , error : InputErrorAndGuidanceInternal.ErrorState
        , guidance : Guidance msg
        , custom : List (Html.Attribute msg)
    }
    -> List Style
    -> Html.Html msg
viewCheckboxLabel config styles =
    let
        marginTopAdjustment =
            case config.guidance of
                Just _ ->
                    marginTop (Css.px -highContrastBorderWidth)

                Nothing ->
                    Css.batch []

        attributes =
            List.concat
                [ [ css
                        (paddingLeft (Css.px checkboxIconWidth)
                            :: marginTopAdjustment
                            :: marginLeft (Css.px -checkboxIconWidth)
                            :: (if config.hideLabel then
                                    minHeight (Css.px checkboxIconHeight)

                                else
                                    Css.batch []
                               )
                            :: outline none
                            :: styles
                            ++ config.labelCss
                        )
                  , Attributes.class FocusRing.customClass
                  , Role.checkBox
                  , Key.tabbable True
                  , Attributes.id config.identifier
                  , InputErrorAndGuidanceInternal.describedBy config.identifier config
                  , Aria.checked (selectedToMaybe config.selected)
                  ]
                    ++ config.custom
                , if config.disabled then
                    [ Aria.disabled True ]

                  else
                    config.onCheck
                        |> Maybe.map (onCheckMsg config.selected)
                        |> Maybe.map
                            (\msg ->
                                [ Events.onClick msg
                                , Key.onKeyDownPreventDefault [ Key.space msg ]
                                ]
                            )
                        |> Maybe.withDefault []
                ]

        viewLabel =
            if config.hideLabel then
                Html.span Accessibility.Styled.Style.invisible
                    [ Html.text config.label ]

            else
                Html.text config.label
    in
    Html.div attributes [ viewLabel ]


checkboxIconWidth : Float
checkboxIconWidth =
    40


checkboxIconHeight : Float
checkboxIconHeight =
    27


inputGuidance :
    { a
        | identifier : String
        , error : InputErrorAndGuidanceInternal.ErrorState
        , guidance : Guidance msg
    }
    -> List (Html msg)
inputGuidance config =
    InputErrorAndGuidanceInternal.view config.identifier
        (Css.marginTop Css.zero)
        config


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
            [ border3 (px highContrastBorderWidth) solid transparent
            , borderRadius (px 3)
            , height (Css.px checkboxIconHeight)
            , boxSizing contentBox
            , margin (px 2)
            , marginRight (px 7)
            ]
        , Attributes.class "checkbox-icon-container"
        ]
        [ Html.div
            [ css
                [ display inlineBlock
                , backgroundColor Colors.white
                , height (Css.px checkboxIconHeight)
                , borderRadius (px 4)
                ]
            ]
            [ Nri.Ui.Svg.V1.toHtml (Nri.Ui.Svg.V1.withCss styles icon)
            ]
        ]


highContrastBorderWidth : Float
highContrastBorderWidth =
    2
