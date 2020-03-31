module Examples exposing (ModuleStates, Msg, init, subscriptions, update, view)

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
import Html.Styled as Html exposing (Html)


type alias ModuleStates =
    { accordion : Example Accordion.State Msg
    , button : Example Button.State Msg
    , bannerAlert : Example BannerAlert.State Msg
    , clickableText : Example ClickableText.State ClickableText.Msg
    , checkbox : Example Checkbox.State Checkbox.Msg
    , dropdown : Example Dropdown.State Dropdown.Msg
    , segmentedControl : Example SegmentedControl.State SegmentedControl.Msg
    , select : Example Select.State Select.Msg
    , table : Example Table.State Table.Msg
    , textArea : Example TextArea.State TextArea.Msg
    , textInput : Example TextInput.State TextInput.Msg
    , disclosureIndicator : Example DisclosureIndicator.State DisclosureIndicator.Msg
    , modal : Example Modal.State Msg
    , slideModal : Example SlideModal.State SlideModal.Msg
    , slide : Example Slide.State Slide.Msg
    , sortableTable : Example SortableTable.State SortableTable.Msg
    , svg : Example Svg.State Svg.Msg
    , clickableSvg : Example ClickableSvg.State ClickableSvg.Msg
    , tabs : Example Tabs.State Tabs.Msg
    , tooltip : Example Tooltip.State Tooltip.Msg
    }


init : ModuleStates
init =
    { accordion = Example.wrap { wrapMsg = AccordionMsg, unwrapMsg = getAccordionMsg } Accordion.example
    , button = Example.wrap { wrapMsg = ButtonMsg, unwrapMsg = getButtonMsg } Button.example
    , bannerAlert = Example.wrap { wrapMsg = BannerAlertMsg, unwrapMsg = getBannerAlertMsg } BannerAlert.example
    , clickableText = ClickableText.example
    , checkbox = Checkbox.example
    , dropdown = Dropdown.example
    , segmentedControl = SegmentedControl.example
    , select = Select.example
    , table = Table.example
    , textArea = TextArea.example
    , textInput = TextInput.example
    , disclosureIndicator = DisclosureIndicator.example
    , modal = Example.wrap { wrapMsg = ModalMsg, unwrapMsg = getModalMsg } Modal.example
    , slideModal = SlideModal.example
    , slide = Slide.example
    , sortableTable = SortableTable.example
    , svg = Svg.example
    , clickableSvg = ClickableSvg.example
    , tabs = Tabs.example
    , tooltip = Tooltip.example
    }


type Msg
    = AccordionMsg Accordion.Msg
    | ButtonMsg Button.Msg
    | BannerAlertMsg BannerAlert.Msg
    | ModalMsg Modal.Msg


getAccordionMsg : Msg -> Maybe Accordion.Msg
getAccordionMsg msg =
    case msg of
        AccordionMsg childMsg ->
            Just childMsg

        _ ->
            Nothing


getButtonMsg : Msg -> Maybe Button.Msg
getButtonMsg msg =
    case msg of
        ButtonMsg childMsg ->
            Just childMsg

        _ ->
            Nothing


getBannerAlertMsg : Msg -> Maybe BannerAlert.Msg
getBannerAlertMsg msg =
    case msg of
        BannerAlertMsg childMsg ->
            Just childMsg

        _ ->
            Nothing


getModalMsg : Msg -> Maybe Modal.Msg
getModalMsg msg =
    case msg of
        ModalMsg childMsg ->
            Just childMsg

        _ ->
            Nothing


update : Msg -> ModuleStates -> ( ModuleStates, Cmd Msg )
update msg moduleStates =
    let
        update_ example =
            example.update msg example.state
                |> Tuple.mapFirst (\newState -> { example | state = newState })
    in
    case msg of
        AccordionMsg exampleMsg ->
            update_ moduleStates.accordion
                |> Tuple.mapFirst (\accordion -> { moduleStates | accordion = accordion })

        ButtonMsg exampleMsg ->
            update_ moduleStates.button
                |> Tuple.mapFirst (\button -> { moduleStates | button = button })

        BannerAlertMsg exampleMsg ->
            update_ moduleStates.bannerAlert
                |> Tuple.mapFirst (\bannerAlert -> { moduleStates | bannerAlert = bannerAlert })

        ModalMsg exampleMsg ->
            update_ moduleStates.modal
                |> Tuple.mapFirst (\modal -> { moduleStates | modal = modal })


{-| -}
subscriptions : ModuleStates -> Sub Msg
subscriptions moduleStates =
    Sub.batch
        [ moduleStates.modal.subscriptions moduleStates.modal.state
        ]


{-| -}
view : Bool -> (Example state msg -> Bool) -> ModuleStates -> List (Html Msg)
view showFocusLink filter moduleStates =
    -- TODO add the filter back in
    [ Example.view showFocusLink moduleStates.accordion
    , Example.view showFocusLink moduleStates.button
    , Example.view showFocusLink moduleStates.bannerAlert
    , Example.view showFocusLink moduleStates.modal
    ]
