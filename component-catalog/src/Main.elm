module Main exposing (main)

import Accessibility.Styled.Aria as Aria
import Browser
import Css exposing (Style)
import Html as RootHtml
import Html.Styled exposing (..)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Checkbox.V7 as Checkbox
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.RadioButton.V4 as RadioButton
import Nri.Ui.SegmentedControl.V14 as SegmentedControl
import Nri.Ui.Select.V9 as Select
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Switch.V2 as Switch
import Nri.Ui.Table.V7 as Table
import Nri.Ui.Tooltip.V3 as Tooltip
import Nri.Ui.UiIcon.V1 as UiIcon
import Sort
import Sort.Set as Set


main : Program () (Set.Set Type) Msg
main =
    Browser.sandbox
        { init = Set.empty sorter
        , view = \model -> view model |> toUnstyled
        , update =
            \msg model ->
                case msg of
                    Toggle toToggle True ->
                        Set.insert toToggle model

                    Toggle toToggle False ->
                        Set.remove toToggle model

                    Swallow ->
                        model
        }


type Type
    = ClickableSvg
    | ClickableText
    | ButtonDisabled
    | ButtonUnfulfilled
    | Checkbox
    | RadioButton
    | SegmentedControlRadioButton
    | SegmentedControlTabs
    | Select
    | Switch


typeToName type_ =
    case type_ of
        ClickableSvg ->
            "ClickableSvg"

        ClickableText ->
            "ClickableText"

        ButtonDisabled ->
            "Button"

        ButtonUnfulfilled ->
            "Button"

        Checkbox ->
            "Checkbox"

        RadioButton ->
            "RadioButton"

        SegmentedControlRadioButton ->
            "SegmentedControl (as radio buttons)"

        SegmentedControlTabs ->
            "SegmentedControl (as tabs)"

        Select ->
            "Select"

        Switch ->
            "Switch"


view model =
    Table.view []
        [ Table.string
            { header = "Component Name"
            , value = .type_ >> typeToName
            , width = Css.px 50
            , cellStyles = always []
            , sort = Nothing
            }
        , Table.custom
            { header = text "view"
            , width = Css.px 250
            , view = .view
            , cellStyles = always []
            , sort = Nothing
            }
        , Table.custom
            { header = text "aria-disabled"
            , width = Css.px 250
            , view =
                \{ ariaDisabled } ->
                    if ariaDisabled then
                        UiIcon.checkmark
                            |> Svg.withLabel "Yes"
                            |> Svg.withColor Colors.greenDark
                            |> Svg.withWidth (Css.px 20)
                            |> Svg.toHtml

                    else
                        UiIcon.x
                            |> Svg.withLabel "No"
                            |> Svg.withColor Colors.red
                            |> Svg.withWidth (Css.px 20)
                            |> Svg.toHtml
            , cellStyles = always []
            , sort = Nothing
            }
        , Table.string
            { header = "Notes"
            , value = .notes
            , width = Css.px 50
            , cellStyles = always [ Css.paddingTop (Css.px 20), Css.paddingBottom (Css.px 20) ]
            , sort = Nothing
            }
        ]
        [ { type_ = ButtonDisabled
          , view =
                viewWithTooltip ButtonDisabled
                    model
                    (\popupTriggerAttributes ->
                        Button.button "Submit"
                            [ Button.custom popupTriggerAttributes
                            , Button.disabled
                            ]
                    )
          , ariaDisabled = False
          , notes = "Button.disabled"
          }
        , { type_ = ButtonUnfulfilled
          , view =
                viewWithTooltip ButtonUnfulfilled
                    model
                    (\popupTriggerAttributes ->
                        Button.button "Submit"
                            [ Button.custom popupTriggerAttributes
                            , Button.unfulfilled
                            , Button.custom [ Aria.disabled True ]
                            ]
                    )
          , ariaDisabled = True
          , notes = "Button.unfulfilled theme does NOT automatically add aria-disabled property."
          }
        , { type_ = Checkbox
          , view =
                viewWithTooltip Checkbox
                    model
                    (\popupTriggerAttributes ->
                        Checkbox.view
                            { label = "Enable Text-to-Speech"
                            , selected = Checkbox.PartiallySelected
                            }
                            [ Checkbox.disabled
                            ]
                    )
          , ariaDisabled = True
          , notes = "Checkbox doesn't currently fully support tooltips"
          }
        , { type_ = ClickableSvg
          , view =
                viewWithTooltip ClickableSvg
                    model
                    (\popupTriggerAttributes ->
                        ClickableSvg.button "Submit"
                            UiIcon.arrowPointingRight
                            [ ClickableSvg.custom popupTriggerAttributes
                            , ClickableSvg.disabled True
                            ]
                    )
          , ariaDisabled = False
          , notes = "There isn't an unfulfilled style for ClickableSvg."
          }
        , { type_ = ClickableText
          , view =
                viewWithTooltip ClickableText
                    model
                    (\popupTriggerAttributes ->
                        ClickableText.button "Submit"
                            [ ClickableText.custom popupTriggerAttributes
                            ]
                    )
          , ariaDisabled = False
          , notes = "ClickableText doesn't support disabling (no styles for it). Note that QW preview doesn't add either aria-disabled or disabled, so I haven't added it here."
          }
        , { type_ = RadioButton
          , view =
                viewWithTooltip RadioButton
                    model
                    (\popupTriggerAttributes ->
                        RadioButton.view
                            { label = "Dogs"
                            , name = "pets"
                            , value = ()
                            , selectedValue = Nothing
                            , valueToString = \_ -> "pets"
                            }
                            [ RadioButton.disabled ]
                    )
          , ariaDisabled = True
          , notes = "RadioButton doesn't currently fully support tooltips"
          }
        , { type_ = SegmentedControlRadioButton
          , view =
                SegmentedControl.viewRadioGroup
                    { onSelect = \_ -> Swallow
                    , options =
                        [ { icon = Nothing
                          , label = text "Source 1"
                          , value = 0
                          , idString = String.fromInt 0
                          , tooltip = tooltipProperties SegmentedControlRadioButton model
                          , attributes = [ Aria.disabled True ]
                          }
                        ]
                    , selected = Nothing
                    , positioning = SegmentedControl.Left SegmentedControl.FitContent
                    , legend = "SegmentedControls 'viewSelectRadio' example"
                    }
          , ariaDisabled = True
          , notes = "aria-disabled added manually. There are no disabled/unfulfilled styles."
          }
        , { type_ = SegmentedControlTabs
          , view =
                SegmentedControl.view
                    { focusAndSelect = \_ -> Swallow
                    , options =
                        [ { icon = Nothing
                          , label = text "Source 1"
                          , value = 0
                          , idString = String.fromInt 0
                          , tabTooltip = []
                          , attributes = []
                          , content = text "content"
                          }
                        , { icon = Nothing
                          , label = text "Source 2"
                          , value = 1
                          , idString = String.fromInt 1
                          , tabTooltip = tooltipProperties SegmentedControlTabs model
                          , attributes = [ Aria.disabled True ]
                          , content = text "content"
                          }
                        ]
                    , selected = 0
                    , positioning = SegmentedControl.Left SegmentedControl.FitContent
                    , toUrl = Nothing
                    }
          , ariaDisabled = True
          , notes = "aria-disabled added manually to the second option. There are no disabled/unfulfilled styles."
          }
        , { type_ = Select
          , view =
                viewWithTooltip Select
                    model
                    (\popupTriggerAttributes ->
                        Select.view "Options"
                            [ Select.disabled
                            ]
                    )
          , ariaDisabled = False
          , notes = "Select doesn't currently fully support tooltips"
          }
        , { type_ = Switch
          , view =
                viewWithTooltip Switch
                    model
                    (\popupTriggerAttributes ->
                        Switch.view { id = "siwtch", label = "Show pandas in results" }
                            [ Switch.disabled True
                            ]
                    )
          , ariaDisabled = True
          , notes = "Switch doesn't currently fully support tooltips"
          }
        ]


viewWithTooltip type_ state input =
    Tooltip.view
        { trigger = input
        , id = "tooltip-" ++ typeToName type_
        }
        (tooltipProperties type_ state)


tooltipProperties type_ state =
    [ Tooltip.open (Set.memberOf state type_)
    , Tooltip.onToggle (Toggle type_)
    , Tooltip.paragraph "The quick brown fox jumps over the lazy dog."
    , Tooltip.auxiliaryDescription
    , Tooltip.onRight
    , Tooltip.smallPadding
    ]


sorter =
    Sort.by Debug.toString Sort.alphabetical


type Msg
    = Toggle Type Bool
    | Swallow
