module Examples exposing (Msg, State, all)

import Example exposing (Example)
import Examples.Accordion as Accordion
import Examples.Alert as Alert
import Examples.AssignmentIcon as AssignmentIcon
import Examples.BannerAlert as BannerAlert
import Examples.Button as Button
import Examples.Callout as Callout
import Examples.Checkbox as Checkbox
import Examples.ClickableSvg as ClickableSvg
import Examples.ClickableText as ClickableText
import Examples.Colors as Colors
import Examples.DisclosureIndicator as DisclosureIndicator
import Examples.Dropdown as Dropdown
import Examples.Fonts as Fonts
import Examples.Heading as Heading
import Examples.Icon as Icon
import Examples.Logo as Logo
import Examples.MasteryIcon as MasteryIcon
import Examples.Modal as Modal
import Examples.Page as Page
import Examples.Pennant as Pennant
import Examples.SegmentedControl as SegmentedControl
import Examples.Select as Select
import Examples.Slide as Slide
import Examples.SlideModal as SlideModal
import Examples.SortableTable as SortableTable
import Examples.Svg as Svg
import Examples.Table as Table
import Examples.Tabs as Tabs
import Examples.Text as Text
import Examples.Text.Writing as Writing
import Examples.TextArea as TextArea
import Examples.TextInput as TextInput
import Examples.Tooltip as Tooltip
import Examples.UiIcon as UiIcon


all : List (Example State Msg)
all =
    [ Example.wrap
        { wrapMsg = AccordionMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    AccordionMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = AccordionState
        , unwrapState =
            \msg ->
                case msg of
                    AccordionState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Accordion.example
    , Example.wrap
        { wrapMsg = AlertMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    AlertMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = AlertState
        , unwrapState =
            \msg ->
                case msg of
                    AlertState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Alert.example
    , Example.wrap
        { wrapMsg = AssignmentIconMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    AssignmentIconMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = AssignmentIconState
        , unwrapState =
            \msg ->
                case msg of
                    AssignmentIconState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        AssignmentIcon.example
    , Example.wrap
        { wrapMsg = BannerAlertMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    BannerAlertMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = BannerAlertState
        , unwrapState =
            \msg ->
                case msg of
                    BannerAlertState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        BannerAlert.example
    , Example.wrap
        { wrapMsg = ButtonMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    ButtonMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = ButtonState
        , unwrapState =
            \msg ->
                case msg of
                    ButtonState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Button.example
    , Example.wrap
        { wrapMsg = CalloutMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    CalloutMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = CalloutState
        , unwrapState =
            \msg ->
                case msg of
                    CalloutState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Callout.example
    , Example.wrap
        { wrapMsg = CheckboxMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    CheckboxMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = CheckboxState
        , unwrapState =
            \msg ->
                case msg of
                    CheckboxState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Checkbox.example
    , Example.wrap
        { wrapMsg = ClickableSvgMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    ClickableSvgMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = ClickableSvgState
        , unwrapState =
            \msg ->
                case msg of
                    ClickableSvgState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        ClickableSvg.example
    , Example.wrap
        { wrapMsg = ClickableTextMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    ClickableTextMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = ClickableTextState
        , unwrapState =
            \msg ->
                case msg of
                    ClickableTextState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        ClickableText.example
    , Example.wrap
        { wrapMsg = ColorsMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    ColorsMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = ColorsState
        , unwrapState =
            \msg ->
                case msg of
                    ColorsState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Colors.example
    , Example.wrap
        { wrapMsg = DisclosureIndicatorMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    DisclosureIndicatorMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = DisclosureIndicatorState
        , unwrapState =
            \msg ->
                case msg of
                    DisclosureIndicatorState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        DisclosureIndicator.example
    , Example.wrap
        { wrapMsg = DropdownMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    DropdownMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = DropdownState
        , unwrapState =
            \msg ->
                case msg of
                    DropdownState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Dropdown.example
    , Example.wrap
        { wrapMsg = FontsMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    FontsMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = FontsState
        , unwrapState =
            \msg ->
                case msg of
                    FontsState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Fonts.example
    , Example.wrap
        { wrapMsg = HeadingMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    HeadingMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = HeadingState
        , unwrapState =
            \msg ->
                case msg of
                    HeadingState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Heading.example
    , Example.wrap
        { wrapMsg = IconMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    IconMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = IconState
        , unwrapState =
            \msg ->
                case msg of
                    IconState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Icon.example
    , Example.wrap
        { wrapMsg = LogoMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    LogoMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = LogoState
        , unwrapState =
            \msg ->
                case msg of
                    LogoState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Logo.example
    , Example.wrap
        { wrapMsg = MasteryIconMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    MasteryIconMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = MasteryIconState
        , unwrapState =
            \msg ->
                case msg of
                    MasteryIconState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        MasteryIcon.example
    , Example.wrap
        { wrapMsg = ModalMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    ModalMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = ModalState
        , unwrapState =
            \msg ->
                case msg of
                    ModalState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Modal.example
    , Example.wrap
        { wrapMsg = PageMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    PageMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = PageState
        , unwrapState =
            \msg ->
                case msg of
                    PageState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Page.example
    , Example.wrap
        { wrapMsg = PennantMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    PennantMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = PennantState
        , unwrapState =
            \msg ->
                case msg of
                    PennantState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Pennant.example
    , Example.wrap
        { wrapMsg = SegmentedControlMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    SegmentedControlMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = SegmentedControlState
        , unwrapState =
            \msg ->
                case msg of
                    SegmentedControlState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        SegmentedControl.example
    , Example.wrap
        { wrapMsg = SelectMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    SelectMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = SelectState
        , unwrapState =
            \msg ->
                case msg of
                    SelectState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Select.example
    , Example.wrap
        { wrapMsg = SlideMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    SlideMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = SlideState
        , unwrapState =
            \msg ->
                case msg of
                    SlideState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Slide.example
    , Example.wrap
        { wrapMsg = SlideModalMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    SlideModalMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = SlideModalState
        , unwrapState =
            \msg ->
                case msg of
                    SlideModalState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        SlideModal.example
    , Example.wrap
        { wrapMsg = SortableTableMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    SortableTableMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = SortableTableState
        , unwrapState =
            \msg ->
                case msg of
                    SortableTableState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        SortableTable.example
    , Example.wrap
        { wrapMsg = SvgMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    SvgMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = SvgState
        , unwrapState =
            \msg ->
                case msg of
                    SvgState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Svg.example
    , Example.wrap
        { wrapMsg = TableMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    TableMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = TableState
        , unwrapState =
            \msg ->
                case msg of
                    TableState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Table.example
    , Example.wrap
        { wrapMsg = TabsMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    TabsMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = TabsState
        , unwrapState =
            \msg ->
                case msg of
                    TabsState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Tabs.example
    , Example.wrap
        { wrapMsg = TextMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    TextMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = TextState
        , unwrapState =
            \msg ->
                case msg of
                    TextState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Text.example
    , Example.wrap
        { wrapMsg = WritingMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    WritingMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = WritingState
        , unwrapState =
            \msg ->
                case msg of
                    WritingState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Writing.example
    , Example.wrap
        { wrapMsg = TextAreaMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    TextAreaMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = TextAreaState
        , unwrapState =
            \msg ->
                case msg of
                    TextAreaState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        TextArea.example
    , Example.wrap
        { wrapMsg = TextInputMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    TextInputMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = TextInputState
        , unwrapState =
            \msg ->
                case msg of
                    TextInputState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        TextInput.example
    , Example.wrap
        { wrapMsg = TooltipMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    TooltipMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = TooltipState
        , unwrapState =
            \msg ->
                case msg of
                    TooltipState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        Tooltip.example
    , Example.wrap
        { wrapMsg = UiIconMsg
        , unwrapMsg =
            \msg ->
                case msg of
                    UiIconMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
        , wrapState = UiIconState
        , unwrapState =
            \msg ->
                case msg of
                    UiIconState childState ->
                        Just childState

                    _ ->
                        Nothing
        }
        UiIcon.example
    ]


type State
    = AccordionState Accordion.State
    | AlertState Alert.State
    | AssignmentIconState AssignmentIcon.State
    | BannerAlertState BannerAlert.State
    | ButtonState Button.State
    | CalloutState Callout.State
    | CheckboxState Checkbox.State
    | ClickableSvgState ClickableSvg.State
    | ClickableTextState ClickableText.State
    | ColorsState Colors.State
    | DisclosureIndicatorState DisclosureIndicator.State
    | DropdownState Dropdown.State
    | FontsState Fonts.State
    | HeadingState Heading.State
    | IconState Icon.State
    | LogoState Logo.State
    | MasteryIconState MasteryIcon.State
    | ModalState Modal.State
    | PageState Page.State
    | PennantState Pennant.State
    | SegmentedControlState SegmentedControl.State
    | SelectState Select.State
    | SlideState Slide.State
    | SlideModalState SlideModal.State
    | SortableTableState SortableTable.State
    | SvgState Svg.State
    | TableState Table.State
    | TabsState Tabs.State
    | TextState Text.State
    | TextAreaState TextArea.State
    | TextInputState TextInput.State
    | TooltipState Tooltip.State
    | UiIconState UiIcon.State
    | WritingState Writing.State


type Msg
    = AccordionMsg Accordion.Msg
    | AlertMsg Alert.Msg
    | AssignmentIconMsg AssignmentIcon.Msg
    | BannerAlertMsg BannerAlert.Msg
    | ButtonMsg Button.Msg
    | CalloutMsg Callout.Msg
    | CheckboxMsg Checkbox.Msg
    | ClickableSvgMsg ClickableSvg.Msg
    | ClickableTextMsg ClickableText.Msg
    | ColorsMsg Colors.Msg
    | DisclosureIndicatorMsg DisclosureIndicator.Msg
    | DropdownMsg Dropdown.Msg
    | FontsMsg Fonts.Msg
    | HeadingMsg Heading.Msg
    | IconMsg Icon.Msg
    | LogoMsg Logo.Msg
    | MasteryIconMsg MasteryIcon.Msg
    | ModalMsg Modal.Msg
    | PageMsg Page.Msg
    | PennantMsg Pennant.Msg
    | SegmentedControlMsg SegmentedControl.Msg
    | SelectMsg Select.Msg
    | SlideMsg Slide.Msg
    | SlideModalMsg SlideModal.Msg
    | SortableTableMsg SortableTable.Msg
    | SvgMsg Svg.Msg
    | TableMsg Table.Msg
    | TabsMsg Tabs.Msg
    | TextMsg Text.Msg
    | TextAreaMsg TextArea.Msg
    | TextInputMsg TextInput.Msg
    | TooltipMsg Tooltip.Msg
    | UiIconMsg UiIcon.Msg
    | WritingMsg Writing.Msg
