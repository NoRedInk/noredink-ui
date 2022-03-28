module Examples exposing (Msg, State, all)

import Example exposing (Example)
import Examples.Accordion as Accordion
import Examples.AssignmentIcon as AssignmentIcon
import Examples.Balloon as Balloon
import Examples.Button as Button
import Examples.Checkbox as Checkbox
import Examples.ClickableSvg as ClickableSvg
import Examples.ClickableText as ClickableText
import Examples.Colors as Colors
import Examples.Confetti as Confetti
import Examples.Container as Container
import Examples.DisclosureIndicator as DisclosureIndicator
import Examples.Divider as Divider
import Examples.Fonts as Fonts
import Examples.Heading as Heading
import Examples.Loading as Loading
import Examples.Logo as Logo
import Examples.Menu as Menu
import Examples.Message as Message
import Examples.Modal as Modal
import Examples.Page as Page
import Examples.Pennant as Pennant
import Examples.RadioButton as RadioButton
import Examples.SegmentedControl as SegmentedControl
import Examples.Select as Select
import Examples.Shadows as Shadows
import Examples.SideNav as SideNav
import Examples.SortableTable as SortableTable
import Examples.Sprite as Sprite
import Examples.Svg as Svg
import Examples.Switch as Switch
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
    [ Accordion.example
        |> Example.wrapMsg AccordionMsg
            (\msg ->
                case msg of
                    AccordionMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState AccordionState
            (\msg ->
                case msg of
                    AccordionState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , AssignmentIcon.example
        |> Example.wrapMsg AssignmentIconMsg
            (\msg ->
                case msg of
                    AssignmentIconMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState AssignmentIconState
            (\msg ->
                case msg of
                    AssignmentIconState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Balloon.example
        |> Example.wrapMsg BalloonMsg
            (\msg ->
                case msg of
                    BalloonMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState BalloonState
            (\msg ->
                case msg of
                    BalloonState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Button.example
        |> Example.wrapMsg ButtonMsg
            (\msg ->
                case msg of
                    ButtonMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState ButtonState
            (\msg ->
                case msg of
                    ButtonState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Checkbox.example
        |> Example.wrapMsg CheckboxMsg
            (\msg ->
                case msg of
                    CheckboxMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState CheckboxState
            (\msg ->
                case msg of
                    CheckboxState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , ClickableSvg.example
        |> Example.wrapMsg ClickableSvgMsg
            (\msg ->
                case msg of
                    ClickableSvgMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState ClickableSvgState
            (\msg ->
                case msg of
                    ClickableSvgState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , ClickableText.example
        |> Example.wrapMsg ClickableTextMsg
            (\msg ->
                case msg of
                    ClickableTextMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState ClickableTextState
            (\msg ->
                case msg of
                    ClickableTextState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Colors.example
        |> Example.wrapMsg ColorsMsg
            (\msg ->
                case msg of
                    ColorsMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState ColorsState
            (\msg ->
                case msg of
                    ColorsState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Confetti.example
        |> Example.wrapMsg ConfettiMsg
            (\msg ->
                case msg of
                    ConfettiMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState ConfettiState
            (\msg ->
                case msg of
                    ConfettiState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Container.example
        |> Example.wrapMsg ContainerMsg
            (\msg ->
                case msg of
                    ContainerMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState ContainerState
            (\msg ->
                case msg of
                    ContainerState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , DisclosureIndicator.example
        |> Example.wrapMsg DisclosureIndicatorMsg
            (\msg ->
                case msg of
                    DisclosureIndicatorMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState DisclosureIndicatorState
            (\msg ->
                case msg of
                    DisclosureIndicatorState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Divider.example
        |> Example.wrapMsg DividerMsg
            (\msg ->
                case msg of
                    DividerMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState DividerState
            (\msg ->
                case msg of
                    DividerState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Fonts.example
        |> Example.wrapMsg FontsMsg
            (\msg ->
                case msg of
                    FontsMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState FontsState
            (\msg ->
                case msg of
                    FontsState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Heading.example
        |> Example.wrapMsg HeadingMsg
            (\msg ->
                case msg of
                    HeadingMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState HeadingState
            (\msg ->
                case msg of
                    HeadingState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Loading.example
        |> Example.wrapMsg LoadingMsg
            (\msg ->
                case msg of
                    LoadingMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState LoadingState
            (\msg ->
                case msg of
                    LoadingState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Logo.example
        |> Example.wrapMsg LogoMsg
            (\msg ->
                case msg of
                    LogoMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState LogoState
            (\msg ->
                case msg of
                    LogoState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Menu.example
        |> Example.wrapMsg MenuMsg
            (\msg ->
                case msg of
                    MenuMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState MenuState
            (\msg ->
                case msg of
                    MenuState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Message.example
        |> Example.wrapMsg MessageMsg
            (\msg ->
                case msg of
                    MessageMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState MessageState
            (\msg ->
                case msg of
                    MessageState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Modal.example
        |> Example.wrapMsg ModalMsg
            (\msg ->
                case msg of
                    ModalMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState ModalState
            (\msg ->
                case msg of
                    ModalState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Page.example
        |> Example.wrapMsg PageMsg
            (\msg ->
                case msg of
                    PageMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState PageState
            (\msg ->
                case msg of
                    PageState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Pennant.example
        |> Example.wrapMsg PennantMsg
            (\msg ->
                case msg of
                    PennantMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState PennantState
            (\msg ->
                case msg of
                    PennantState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , RadioButton.example
        |> Example.wrapMsg RadioButtonMsg
            (\msg ->
                case msg of
                    RadioButtonMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState RadioButtonState
            (\msg ->
                case msg of
                    RadioButtonState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , SegmentedControl.example
        |> Example.wrapMsg SegmentedControlMsg
            (\msg ->
                case msg of
                    SegmentedControlMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState SegmentedControlState
            (\msg ->
                case msg of
                    SegmentedControlState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Select.example
        |> Example.wrapMsg SelectMsg
            (\msg ->
                case msg of
                    SelectMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState SelectState
            (\msg ->
                case msg of
                    SelectState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Shadows.example
        |> Example.wrapMsg ShadowsMsg
            (\msg ->
                case msg of
                    ShadowsMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState ShadowsState
            (\msg ->
                case msg of
                    ShadowsState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , SideNav.example
        |> Example.wrapMsg SideNavMsg
            (\msg ->
                case msg of
                    SideNavMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState SideNavState
            (\msg ->
                case msg of
                    SideNavState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , SortableTable.example
        |> Example.wrapMsg SortableTableMsg
            (\msg ->
                case msg of
                    SortableTableMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState SortableTableState
            (\msg ->
                case msg of
                    SortableTableState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Sprite.example
        |> Example.wrapMsg SpriteMsg
            (\msg ->
                case msg of
                    SpriteMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState SpriteState
            (\msg ->
                case msg of
                    SpriteState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Svg.example
        |> Example.wrapMsg SvgMsg
            (\msg ->
                case msg of
                    SvgMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState SvgState
            (\msg ->
                case msg of
                    SvgState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Switch.example
        |> Example.wrapMsg SwitchMsg
            (\msg ->
                case msg of
                    SwitchMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState SwitchState
            (\msg ->
                case msg of
                    SwitchState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Table.example
        |> Example.wrapMsg TableMsg
            (\msg ->
                case msg of
                    TableMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState TableState
            (\msg ->
                case msg of
                    TableState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Tabs.example
        |> Example.wrapMsg TabsMsg
            (\msg ->
                case msg of
                    TabsMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState TabsState
            (\msg ->
                case msg of
                    TabsState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Text.example
        |> Example.wrapMsg TextMsg
            (\msg ->
                case msg of
                    TextMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState TextState
            (\msg ->
                case msg of
                    TextState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Writing.example
        |> Example.wrapMsg WritingMsg
            (\msg ->
                case msg of
                    WritingMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState WritingState
            (\msg ->
                case msg of
                    WritingState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , TextArea.example
        |> Example.wrapMsg TextAreaMsg
            (\msg ->
                case msg of
                    TextAreaMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState TextAreaState
            (\msg ->
                case msg of
                    TextAreaState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , TextInput.example
        |> Example.wrapMsg TextInputMsg
            (\msg ->
                case msg of
                    TextInputMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState TextInputState
            (\msg ->
                case msg of
                    TextInputState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Tooltip.example
        |> Example.wrapMsg TooltipMsg
            (\msg ->
                case msg of
                    TooltipMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState TooltipState
            (\msg ->
                case msg of
                    TooltipState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , UiIcon.example
        |> Example.wrapMsg UiIconMsg
            (\msg ->
                case msg of
                    UiIconMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> Example.wrapState UiIconState
            (\msg ->
                case msg of
                    UiIconState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    ]


type State
    = AccordionState Accordion.State
    | AssignmentIconState AssignmentIcon.State
    | BalloonState Balloon.State
    | ButtonState Button.State
    | CheckboxState Checkbox.State
    | ClickableSvgState ClickableSvg.State
    | ClickableTextState ClickableText.State
    | ColorsState Colors.State
    | ConfettiState Confetti.State
    | ContainerState Container.State
    | DisclosureIndicatorState DisclosureIndicator.State
    | DividerState Divider.State
    | FontsState Fonts.State
    | HeadingState Heading.State
    | LoadingState Loading.State
    | LogoState Logo.State
    | MenuState Menu.State
    | MessageState Message.State
    | ModalState Modal.State
    | PageState Page.State
    | PennantState Pennant.State
    | RadioButtonState RadioButton.State
    | SegmentedControlState SegmentedControl.State
    | SelectState Select.State
    | ShadowsState Shadows.State
    | SideNavState SideNav.State
    | SortableTableState SortableTable.State
    | SpriteState Sprite.State
    | SvgState Svg.State
    | SwitchState Switch.State
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
    | AssignmentIconMsg AssignmentIcon.Msg
    | BalloonMsg Balloon.Msg
    | ButtonMsg Button.Msg
    | CheckboxMsg Checkbox.Msg
    | ClickableSvgMsg ClickableSvg.Msg
    | ClickableTextMsg ClickableText.Msg
    | ColorsMsg Colors.Msg
    | ConfettiMsg Confetti.Msg
    | ContainerMsg Container.Msg
    | DisclosureIndicatorMsg DisclosureIndicator.Msg
    | DividerMsg Divider.Msg
    | FontsMsg Fonts.Msg
    | HeadingMsg Heading.Msg
    | LoadingMsg Loading.Msg
    | LogoMsg Logo.Msg
    | MenuMsg Menu.Msg
    | MessageMsg Message.Msg
    | ModalMsg Modal.Msg
    | PageMsg Page.Msg
    | PennantMsg Pennant.Msg
    | RadioButtonMsg RadioButton.Msg
    | SegmentedControlMsg SegmentedControl.Msg
    | SelectMsg Select.Msg
    | ShadowsMsg Shadows.Msg
    | SideNavMsg SideNav.Msg
    | SortableTableMsg SortableTable.Msg
    | SpriteMsg Sprite.Msg
    | SvgMsg Svg.Msg
    | SwitchMsg Switch.Msg
    | TableMsg Table.Msg
    | TabsMsg Tabs.Msg
    | TextMsg Text.Msg
    | TextAreaMsg TextArea.Msg
    | TextInputMsg TextInput.Msg
    | TooltipMsg Tooltip.Msg
    | UiIconMsg UiIcon.Msg
    | WritingMsg Writing.Msg
