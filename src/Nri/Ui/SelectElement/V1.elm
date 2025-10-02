module Nri.Ui.SelectElement.V1 exposing
    ( view
    , Attribute(..)
    , OptionItem(..), SingleOption
    , optionItems, selectedValue, onSelect
    , title, triggerId, popoverId
    , label, hideLabel
    , disabled, loading
    , icon
    , disableWhen
    , error, guidance
    , containerCss, noMargin
    , valueToString, stringToValue
    , customHtmlAttributesForTrigger, customHtmlAttributesForPopover
    )

{-| A select component powered by a custom HTML element `<select-element>`.

This component allows for a dropdown selection with optional grouping.
The underlying custom element handles popover behavior, keyboard navigation,
and accessibility.

@docs view

@docs Attribute

@docs OptionItem, SingleOption

@docs optionItems, selectedValue, onSelect
@docs title, triggerId, popoverId
@docs label, hideLabel
@docs disabled, loading
@docs icon
@docs disableWhen
@docs error, guidance
@docs containerCss, noMargin
@docs valueToString, stringToValue

@docs customHtmlAttributesForTrigger, customHtmlAttributesForPopover

-}

import Css exposing (..)
import Html.Styled exposing (Attribute, Html, button, div, h3, node, span, text)
import Html.Styled.Attributes as Attributes exposing (attribute, class, for, id, tabindex)
import Html.Styled.Events as Events
import Json.Decode as Decode
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.CssVendorPrefix.V1 as VendorPrefixed
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Shadows.V1 as Shadows
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V6 as Text
import Nri.Ui.UiIcon.V2 as UiIcon



-- MODEL


{-| Represents a single selectable option.

  - `content`: The HTML to display in the dropdown.
  - `triggerLabel`: The plain text label to show in the trigger button when selected.
  - `value`: The value associated with this option.

Example:

    { content = Html.Styled.text "Earth"
    , triggerLabel = "Earth"
    , value = Earth
    }

-}
type alias SingleOption value msg =
    { content : Html msg
    , value : value
    , triggerLabel : String
    }


{-| Represents an item in the select dropdown.

You can use `Item` for a single option, or `Group` to group related options under a label.

    SelectElement.Item
        { content = Html.Styled.text "Earth"
        , triggerLabel = "Earth"
        , value = Earth
        }

    SelectElement.Group "Gas Giants"
        [ { content = Html.Styled.text "Jupiter"
          , triggerLabel = "Jupiter"
          , value = Jupiter
          }
        , { content = Html.Styled.text "Saturn"
          , triggerLabel = "Saturn"
          , value = Saturn
          }
        ]

-}
type OptionItem value msg
    = Item (SingleOption value msg)
    | Group String (List (SingleOption value msg))


{-| An attribute for configuring the SelectElement component.

You can use attributes to set options, handle selection, customize appearance, and more.

    SelectElement.view "Choose a planet"
        [ SelectElement.optionItems myOptions
        , SelectElement.selectedValue (Just Earth)
        , SelectElement.onSelect SelectPlanet
        , SelectElement.label "Planets"
        , SelectElement.disabled False
        ]

-}
type Attribute value msg
    = Attribute (Config value msg -> Config value msg)


type alias Config value msg =
    { optionItems : List (OptionItem value msg)
    , selectedValue : Maybe value
    , onSelect : Maybe (value -> msg)
    , triggerId : String
    , popoverId : String
    , title : Maybe String
    , label : Maybe String
    , hideLabel : Bool
    , disabled : Bool
    , loading : Bool
    , icon : Maybe Svg.Svg
    , disableWhen : value -> Bool
    , error : Maybe String
    , guidance : Maybe String
    , containerCss : List Style
    , noMargin : Bool
    , valueToString : value -> String
    , stringToValue : String -> Maybe value
    , customTriggerAttrs : List (Html.Styled.Attribute msg)
    , customPopoverAttrs : List (Html.Styled.Attribute msg)
    }


defaultConfig : Config value msg
defaultConfig =
    { optionItems = []
    , selectedValue = Nothing
    , onSelect = Nothing
    , triggerId = "select-element-trigger-default"
    , popoverId = "select-element-popover-default"
    , title = Nothing
    , label = Nothing
    , hideLabel = False
    , disabled = False
    , loading = False
    , icon = Nothing
    , disableWhen = \_ -> False
    , error = Nothing
    , guidance = Nothing
    , containerCss = []
    , noMargin = False
    , valueToString = \_ -> ""
    , stringToValue = \_ -> Nothing
    , customTriggerAttrs = []
    , customPopoverAttrs = []
    }


applyConfig : List (Attribute value msg) -> Config value msg
applyConfig attributes =
    List.foldl (\(Attribute update) config -> update config) defaultConfig attributes



-- ATTRIBUTES


{-| Defines the list of options and option groups to display in the select dropdown.
-}
optionItems : List (OptionItem value msg) -> Attribute value msg
optionItems items =
    Attribute (\config -> { config | optionItems = items })


{-| Sets the currently selected value.
-}
selectedValue : Maybe value -> Attribute value msg
selectedValue val =
    Attribute (\config -> { config | selectedValue = val })


{-| The message to send when an option is selected.
It receives the newly selected value.
-}
onSelect : (value -> msg) -> Attribute value msg
onSelect handler =
    Attribute (\config -> { config | onSelect = Just handler })


{-| An optional title to display above the select component.
-}
title : String -> Attribute value msg
title t =
    Attribute (\config -> { config | title = Just t })


{-| The HTML ID for the trigger button.
This ID is used by the `select-element` to associate with its trigger.
Defaults to "select-element-trigger-default".
-}
triggerId : String -> Attribute value msg
triggerId newId =
    Attribute (\config -> { config | triggerId = newId })


{-| The HTML ID for the `select-element` popover.
Defaults to "select-element-popover-default".
-}
popoverId : String -> Attribute value msg
popoverId newId =
    Attribute (\config -> { config | popoverId = newId })


{-| A function to convert your custom `value` type to a String.
This is necessary for setting `data-value` attributes on options and for
generating unique option IDs.
-}
valueToString : (value -> String) -> Attribute value msg
valueToString fn =
    Attribute (\config -> { config | valueToString = fn })


{-| A function to convert a String (from the `select-change` event detail)
back to your custom `value` type.
-}
stringToValue : (String -> Maybe value) -> Attribute value msg
stringToValue fn =
    Attribute (\config -> { config | stringToValue = fn })


{-| Add custom HTML attributes to the trigger button.
-}
customHtmlAttributesForTrigger : List (Html.Styled.Attribute msg) -> Attribute value msg
customHtmlAttributesForTrigger attrs =
    Attribute (\config -> { config | customTriggerAttrs = config.customTriggerAttrs ++ attrs })


{-| Add custom HTML attributes to the `select-element` (popover).
-}
customHtmlAttributesForPopover : List (Html.Styled.Attribute msg) -> Attribute value msg
customHtmlAttributesForPopover attrs =
    Attribute (\config -> { config | customPopoverAttrs = config.customPopoverAttrs ++ attrs })


{-| Sets the accessible label for the select trigger.
This is important for screen readers. Use `hideLabel` to control visual display.
-}
label : String -> Attribute value msg
label l =
    Attribute (\config -> { config | label = Just l })


{-| If `True`, the label provided via the `label` attribute will be visually hidden
(but still available to assistive technologies). Defaults to `False`.
-}
hideLabel : Bool -> Attribute value msg
hideLabel hidden =
    Attribute (\config -> { config | hideLabel = hidden })


{-| Disables the select component, preventing interaction.
-}
disabled : Bool -> Attribute value msg
disabled isDisabled =
    Attribute (\config -> { config | disabled = isDisabled })


{-| Puts the select component in a loading state.
This disables interaction and changes the visual appearance.
-}
loading : Bool -> Attribute value msg
loading isLoading =
    Attribute (\config -> { config | loading = isLoading })


{-| Adds an SVG icon to the trigger button.
The icon will be placed before the text.
-}
icon : Svg.Svg -> Attribute value msg
icon i =
    Attribute (\config -> { config | icon = Just i })


{-| A predicate function to determine if an individual option should be disabled.
Disabled options are not selectable and visually distinct.
-}
disableWhen : (value -> Bool) -> Attribute value msg
disableWhen predicate =
    Attribute (\config -> { config | disableWhen = predicate })


{-| Sets an error message to be displayed below the component.
If set, it overrides any guidance message.
-}
error : String -> Attribute value msg
error errMsg =
    Attribute (\config -> { config | error = Just errMsg, guidance = Nothing })


{-| Sets a guidance message to be displayed below the component.
This message is not shown if an error message is present.
-}
guidance : String -> Attribute value msg
guidance guidMsg =
    Attribute
        (\config ->
            { config
                | guidance =
                    if config.error == Nothing then
                        Just guidMsg

                    else
                        config.guidance
            }
        )


{-| Custom CSS styles for the main container div wrapping the component.
-}
containerCss : List Style -> Attribute value msg
containerCss styles =
    Attribute (\config -> { config | containerCss = config.containerCss ++ styles })


{-| If `True`, removes the default top margin from the component container.
-}
noMargin : Bool -> Attribute value msg
noMargin noMrg =
    Attribute (\config -> { config | noMargin = noMrg })



-- STYLES


popoverHostStyles : List Style
popoverHostStyles =
    [ Css.border3 (px 1) solid Colors.gray85
    , backgroundColor Colors.white
    , borderRadius (px 8)
    , maxHeight (px 300)
    , overflowY auto
    , padding zero
    , Shadows.high
    ]


optionBaseStyles : List Style
optionBaseStyles =
    [ padding2 (px 8) (px 16)
    , cursor pointer
    , Css.fontFamilies [ "Muli", "sans-serif" ]
    , fontSize (px 14)
    , color Colors.navy
    , display block
    , whiteSpace noWrap
    , overflow hidden
    , textOverflow ellipsis
    , borderRadius (px 4)
    , margin2 zero (px 8)
    ]


optgroupContainerStyles : List Style
optgroupContainerStyles =
    [ marginBottom (px 8)
    ]


optgroupLabelStyles : List Style
optgroupLabelStyles =
    [ padding2 (px 8) (px 16)
    , Css.fontFamilies [ "Muli", "sans-serif" ]
    , fontSize (px 14)
    , fontWeight (int 600)
    , color Colors.gray20
    , display block
    , marginBottom (px 8)
    , backgroundColor Colors.gray96
    , borderBottom3 (px 1) solid Colors.gray92
    ]


optionDisabledStyles : List Style
optionDisabledStyles =
    [ color Colors.gray45
    , cursor notAllowed
    ]



-- VIEW LOGIC


getLabelForValue : (value -> String) -> value -> List (OptionItem value msg) -> Maybe String
getLabelForValue valToString valueToFind optionItems_ =
    let
        findLabel optItem =
            case optItem of
                Item opt ->
                    if valToString opt.value == valToString valueToFind then
                        Just opt.triggerLabel

                    else
                        Nothing

                Group _ subOpts ->
                    List.filterMap
                        (\opt ->
                            if valToString opt.value == valToString valueToFind then
                                Just opt.triggerLabel

                            else
                                Nothing
                        )
                        subOpts
                        |> List.head
    in
    List.filterMap findLabel optionItems_
        |> List.head


renderSingleOption : Config value msg -> String -> (value -> String) -> SingleOption value msg -> Html msg
renderSingleOption config popoverIdForOption valToString option =
    let
        isOptionDisabled =
            config.disableWhen option.value

        optionSpecificStyles =
            if isOptionDisabled then
                optionDisabledStyles

            else
                []
    in
    div
        ([ attribute "role" "option"
         , attribute "data-value" (valToString option.value)
         , id (popoverIdForOption ++ "-option-" ++ valToString option.value)
         , tabindex -1
         , Attributes.css (optionBaseStyles ++ optionSpecificStyles)
         ]
            ++ (if isOptionDisabled then
                    [ attribute "aria-disabled" "true"
                    ]

                else
                    []
               )
        )
        [ option.content ]


renderOptionItems : Config value msg -> String -> (value -> String) -> List (OptionItem value msg) -> List (Html msg)
renderOptionItems config popoverIdForOptions valToString items =
    List.concatMap
        (\item ->
            case item of
                Item singleOpt ->
                    [ renderSingleOption config popoverIdForOptions valToString singleOpt ]

                Group groupLabel optList ->
                    if List.isEmpty optList then
                        []

                    else
                        [ div
                            [ attribute "role" "group"
                            , attribute "aria-label" groupLabel
                            , class "optgroup"
                            , Attributes.css optgroupContainerStyles
                            ]
                            ([ div
                                [ class "optgroup-label"
                                , Attributes.css optgroupLabelStyles
                                ]
                                [ text groupLabel ]
                             ]
                                ++ List.map (renderSingleOption config popoverIdForOptions valToString) optList
                            )
                        ]
        )
        items


triggerStyles : Config value msg -> List Style
triggerStyles config =
    let
        isEffectivelyDisabled =
            config.disabled || config.loading

        baseBorderColor =
            Colors.gray75

        borderColor =
            if isEffectivelyDisabled then
                Colors.gray85

            else if config.error /= Nothing then
                Colors.purple

            else
                baseBorderColor

        borderBottomWidth =
            if isEffectivelyDisabled then
                Css.px 1

            else
                Css.px 3
    in
    [ Css.border3 (Css.px 1) Css.solid borderColor
    , Css.borderBottomWidth borderBottomWidth
    , Css.borderRadius (Css.px 8)
    , Css.pseudoClass "focus-within"
        [ Css.borderColor Colors.azure
        , Css.borderRadius (Css.px 8) |> Css.important
        ]
    , Fonts.baseFont
    , Css.fontSize (Css.px 15)
    , Css.fontWeight (Css.int 600)
    , Css.color
        (if isEffectivelyDisabled then
            Colors.gray20

         else
            Colors.navy
        )
    , Css.cursor
        (if isEffectivelyDisabled then
            Css.notAllowed

         else
            Css.pointer
        )
    , Css.height (Css.px 45)
    , Css.width (Css.pct 100)
    , Css.paddingLeft (Css.px 15)
    , Css.paddingRight (Css.px 12)
    , Css.position Css.relative
    , Css.displayFlex
    , Css.alignItems Css.center
    , Css.justifyContent Css.spaceBetween
    , Css.marginTop (px -7)
    , Css.position Css.relative
    , selectElementVisualResets
    , if isEffectivelyDisabled then
        backgroundColor Colors.gray85

      else
        backgroundColor Colors.white
    , if config.loading then
        Css.opacity (Css.num 0.7)

      else
        Css.batch []
    ]


selectElementVisualResets : Style
selectElementVisualResets =
    Css.batch
        [ VendorPrefixed.property "appearance" "none"
        , Css.backgroundColor Css.transparent
        ]



-- VIEW


{-| Renders the SelectElement component.
The `defaultTriggerText` is used as the content of the trigger button
if no `selectedValue` provides a label, or if no `label` attribute is set.
-}
view : String -> List (Attribute value msg) -> Html msg
view defaultTriggerText attributes =
    let
        config =
            applyConfig attributes

        isEffectivelyDisabled =
            config.disabled || config.loading

        labelFromSelectedValue : Maybe String
        labelFromSelectedValue =
            Maybe.andThen
                (\sVal -> getLabelForValue config.valueToString sVal config.optionItems)
                config.selectedValue

        buttonLabelText : String
        buttonLabelText =
            case labelFromSelectedValue of
                Just selectedLabel ->
                    selectedLabel

                Nothing ->
                    defaultTriggerText

        componentValueAttributes =
            case config.selectedValue of
                Just val ->
                    [ attribute "value" (config.valueToString val) ]

                Nothing ->
                    []

        selectChangeHandlerAttributeList =
            case config.onSelect of
                Just onSel ->
                    [ Events.on "select-change"
                        (Decode.at [ "detail", "value" ] Decode.string
                            |> Decode.andThen
                                (\strVal ->
                                    case config.stringToValue strVal of
                                        Just actualVal ->
                                            Decode.succeed (onSel actualVal)

                                        Nothing ->
                                            Decode.fail ("SelectElement.V1: Could not convert string '" ++ strVal ++ "' back to value type.")
                                )
                        )
                    ]

                Nothing ->
                    []

        popoverAttributes =
            [ id config.popoverId
            , attribute "popover" "auto"
            , attribute "data-trigger-id" config.triggerId
            , Attributes.css popoverHostStyles
            ]
                ++ componentValueAttributes
                ++ config.customPopoverAttrs
                ++ selectChangeHandlerAttributeList

        triggerBaseAttributes =
            [ id config.triggerId
            , attribute "aria-haspopup" "listbox"
            , attribute "aria-expanded" "false"
            , Attributes.attribute "popovertarget" config.popoverId
            , Attributes.css (triggerStyles config)
            ]
                ++ config.customTriggerAttrs

        triggerDisabledAttributes =
            if isEffectivelyDisabled then
                [ Attributes.disabled True
                , attribute "aria-disabled" "true"
                ]

            else
                []

        finalTriggerAttributes =
            triggerBaseAttributes ++ triggerDisabledAttributes

        labelNodeCss config_ =
            if config_.hideLabel then
                [ Css.display none ]

            else
                [ Fonts.baseFont
                , Css.fontSize (Css.px 12)
                , Css.fontWeight (Css.int 600)
                , if isEffectivelyDisabled then
                    Css.color Colors.gray45

                  else
                    Css.color Colors.navy
                , Css.marginLeft (Css.px 10)
                , Css.padding2 (Css.px 2) (Css.px 5)
                , if isEffectivelyDisabled then
                    backgroundColor Colors.gray92

                  else
                    backgroundColor Colors.white
                , Css.position Css.relative
                , Css.zIndex (Css.int 1)
                , Css.borderRadius (Css.px 4)
                ]

        labelNodeAttributes labelId config_ =
            [ for labelId
            , Attributes.css (labelNodeCss config_)
            ]

        renderIcon =
            case config.icon of
                Nothing ->
                    []

                Just i ->
                    let
                        iconHtml =
                            i
                                |> Svg.withWidth (Css.px 17)
                                |> Svg.withHeight (Css.px 17)
                                |> Svg.toHtml
                                |> Html.Styled.map never
                    in
                    [ span
                        [ Attributes.css
                            [ Css.marginRight (Css.px 8)
                            , displayFlex
                            , alignItems center
                            ]
                        ]
                        [ iconHtml ]
                    ]

        renderErrorOrGuidance =
            case ( config.error, config.guidance ) of
                ( Just errMsg, _ ) ->
                    div [ Attributes.css [ Css.marginTop (Css.px 4) ] ]
                        [ Text.caption
                            [ Text.css [ Css.color Colors.purple, Css.marginTop (Css.px 5) ]
                            , Text.plaintext errMsg
                            ]
                        ]

                ( Nothing, Just guidMsg ) ->
                    div [ Attributes.css [ Css.marginTop (Css.px 4) ] ]
                        [ Text.caption
                            [ Text.css []
                            , Text.plaintext guidMsg
                            ]
                        ]

                _ ->
                    Html.Styled.text ""

        containerStyles =
            (if config.noMargin then
                []

             else
                [ Css.marginTop (Css.px 16)
                ]
            )
                ++ config.containerCss
    in
    div [ Attributes.css containerStyles ]
        ((Maybe.map
            (\t ->
                h3
                    [ Attributes.css
                        [ Fonts.baseFont
                        , Css.fontSize (Css.px 15)
                        , Css.fontWeight (Css.int 700)
                        , Css.color Colors.navy
                        , Css.marginBottom (Css.px 8)
                        ]
                    ]
                    [ text t ]
            )
            config.title
            |> Maybe.map List.singleton
            |> Maybe.withDefault []
         )
            ++ (case config.label of
                    Just lblString ->
                        [ node "label" (labelNodeAttributes config.triggerId config) [ text lblString ] ]

                    Nothing ->
                        []
               )
            ++ [ button
                    finalTriggerAttributes
                    (renderIcon
                        ++ [ span [ Attributes.css [ Css.flexGrow (Css.int 1), Css.textAlign Css.left ] ] [ text buttonLabelText ]
                           , div [ Attributes.css [ Css.displayFlex ] ]
                                [ UiIcon.doubleArrow
                                    |> Svg.withWidth (Css.px 17)
                                    |> Svg.withHeight (Css.px 17)
                                    |> Svg.toHtml
                                ]
                           ]
                    )
               , node "select-element"
                    popoverAttributes
                    (renderOptionItems config config.popoverId config.valueToString config.optionItems)
               , renderErrorOrGuidance
               ]
        )
