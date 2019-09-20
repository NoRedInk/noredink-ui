module NriModules exposing (ModuleStates, Msg, init, nriThemedModules, subscriptions, update)

import Assets exposing (assets)
import Examples.Alert
import Examples.BannerAlert
import Examples.Button
import Examples.Callout
import Examples.Checkbox
import Examples.ClickableText
import Examples.Colors
import Examples.DisclosureIndicator
import Examples.Dropdown
import Examples.Fonts
import Examples.Icon
import Examples.Modal
import Examples.Page
import Examples.SegmentedControl
import Examples.Select
import Examples.Slide
import Examples.SlideModal
import Examples.SortableTable
import Examples.Table
import Examples.Tabs
import Examples.Text
import Examples.Text.Writing
import Examples.TextArea as TextAreaExample
import Examples.TextInput as TextInputExample
import Examples.Tooltip
import Html exposing (Html, img)
import Html.Attributes exposing (..)
import ModuleExample exposing (Category(..), ModuleExample)
import Url exposing (Url)


type alias ModuleStates =
    { buttonExampleState : Examples.Button.State Msg
    , bannerAlertExampleState : Examples.BannerAlert.State
    , clickableTextExampleState : Examples.ClickableText.State
    , checkboxExampleState : Examples.Checkbox.State
    , dropdownState : Examples.Dropdown.State Examples.Dropdown.Value
    , segmentedControlState : Examples.SegmentedControl.State
    , selectState : Examples.Select.State Examples.Select.Value
    , tableExampleState : Examples.Table.State
    , textAreaExampleState : TextAreaExample.State
    , textInputExampleState : TextInputExample.State
    , disclosureIndicatorExampleState : Examples.DisclosureIndicator.State
    , modalExampleState : Examples.Modal.State
    , slideModalExampleState : Examples.SlideModal.State
    , slideExampleState : Examples.Slide.State
    , sortableTableState : Examples.SortableTable.State
    , tabsExampleState : Examples.Tabs.Tab
    , tooltipExampleState : Examples.Tooltip.State
    }


init : ModuleStates
init =
    { buttonExampleState = Examples.Button.init assets
    , bannerAlertExampleState = Examples.BannerAlert.init
    , clickableTextExampleState = Examples.ClickableText.init assets
    , checkboxExampleState = Examples.Checkbox.init
    , dropdownState = Examples.Dropdown.init
    , segmentedControlState = Examples.SegmentedControl.init assets
    , selectState = Examples.Select.init
    , tableExampleState = Examples.Table.init
    , textAreaExampleState = TextAreaExample.init
    , textInputExampleState = TextInputExample.init
    , disclosureIndicatorExampleState = Examples.DisclosureIndicator.init
    , modalExampleState = Examples.Modal.init
    , slideModalExampleState = Examples.SlideModal.init
    , slideExampleState = Examples.Slide.init
    , sortableTableState = Examples.SortableTable.init
    , tabsExampleState = Examples.Tabs.First
    , tooltipExampleState = Examples.Tooltip.init
    }


type Msg
    = ButtonExampleMsg (Examples.Button.Msg Msg)
    | BannerAlertExampleMsg Examples.BannerAlert.Msg
    | ClickableTextExampleMsg Examples.ClickableText.Msg
    | CheckboxExampleMsg Examples.Checkbox.Msg
    | DropdownMsg Examples.Dropdown.Msg
    | SegmentedControlMsg Examples.SegmentedControl.Msg
    | SelectMsg Examples.Select.Msg
    | ShowItWorked String String
    | TableExampleMsg Examples.Table.Msg
    | TextAreaExampleMsg TextAreaExample.Msg
    | TextInputExampleMsg TextInputExample.Msg
    | DisclosureIndicatorExampleMsg Examples.DisclosureIndicator.Msg
    | ModalExampleMsg Examples.Modal.Msg
    | SlideModalExampleMsg Examples.SlideModal.Msg
    | SlideExampleMsg Examples.Slide.Msg
    | SortableTableMsg Examples.SortableTable.Msg
    | TabsExampleMsg Examples.Tabs.Tab
    | TooltipExampleMsg Examples.Tooltip.Msg
    | NoOp


update : Msg -> ModuleStates -> ( ModuleStates, Cmd Msg )
update outsideMsg moduleStates =
    case outsideMsg of
        ButtonExampleMsg msg ->
            let
                ( buttonExampleState, cmd ) =
                    Examples.Button.update msg moduleStates.buttonExampleState
            in
            ( { moduleStates | buttonExampleState = buttonExampleState }
            , Cmd.map ButtonExampleMsg cmd
            )

        BannerAlertExampleMsg msg ->
            ( { moduleStates
                | bannerAlertExampleState =
                    Examples.BannerAlert.update msg moduleStates.bannerAlertExampleState
              }
            , Cmd.none
            )

        ClickableTextExampleMsg msg ->
            let
                ( clickableTextExampleState, cmd ) =
                    Examples.ClickableText.update msg moduleStates.clickableTextExampleState
            in
            ( { moduleStates | clickableTextExampleState = clickableTextExampleState }
            , Cmd.map ClickableTextExampleMsg cmd
            )

        CheckboxExampleMsg msg ->
            let
                ( checkboxExampleState, cmd ) =
                    Examples.Checkbox.update msg moduleStates.checkboxExampleState
            in
            ( { moduleStates | checkboxExampleState = checkboxExampleState }, Cmd.map CheckboxExampleMsg cmd )

        DropdownMsg msg ->
            let
                ( dropdownState, cmd ) =
                    Examples.Dropdown.update msg moduleStates.dropdownState
            in
            ( { moduleStates | dropdownState = dropdownState }
            , Cmd.map DropdownMsg cmd
            )

        SegmentedControlMsg msg ->
            let
                ( segmentedControlState, cmd ) =
                    Examples.SegmentedControl.update msg moduleStates.segmentedControlState
            in
            ( { moduleStates | segmentedControlState = segmentedControlState }
            , Cmd.map SegmentedControlMsg cmd
            )

        SelectMsg msg ->
            let
                ( selectState, cmd ) =
                    Examples.Select.update msg moduleStates.selectState
            in
            ( { moduleStates | selectState = selectState }
            , Cmd.map SelectMsg cmd
            )

        ShowItWorked group message ->
            let
                _ =
                    Debug.log group message
            in
            ( moduleStates, Cmd.none )

        TableExampleMsg msg ->
            let
                ( tableExampleState, cmd ) =
                    Examples.Table.update msg moduleStates.tableExampleState
            in
            ( { moduleStates | tableExampleState = tableExampleState }
            , Cmd.map TableExampleMsg cmd
            )

        TextAreaExampleMsg msg ->
            let
                ( textAreaExampleState, cmd ) =
                    TextAreaExample.update msg moduleStates.textAreaExampleState
            in
            ( { moduleStates | textAreaExampleState = textAreaExampleState }
            , Cmd.map TextAreaExampleMsg cmd
            )

        TextInputExampleMsg msg ->
            let
                ( textInputExampleState, cmd ) =
                    TextInputExample.update msg moduleStates.textInputExampleState
            in
            ( { moduleStates | textInputExampleState = textInputExampleState }
            , Cmd.map TextInputExampleMsg cmd
            )

        DisclosureIndicatorExampleMsg msg ->
            let
                ( disclosureIndicatorExampleState, cmd ) =
                    Examples.DisclosureIndicator.update msg moduleStates.disclosureIndicatorExampleState
            in
            ( { moduleStates | disclosureIndicatorExampleState = disclosureIndicatorExampleState }
            , Cmd.map DisclosureIndicatorExampleMsg cmd
            )

        ModalExampleMsg msg ->
            let
                ( modalExampleState, cmd ) =
                    Examples.Modal.update msg moduleStates.modalExampleState
            in
            ( { moduleStates | modalExampleState = modalExampleState }
            , Cmd.map ModalExampleMsg cmd
            )

        SlideModalExampleMsg msg ->
            let
                ( slideModalExampleState, cmd ) =
                    Examples.SlideModal.update msg moduleStates.slideModalExampleState
            in
            ( { moduleStates | slideModalExampleState = slideModalExampleState }
            , Cmd.map SlideModalExampleMsg cmd
            )

        SlideExampleMsg msg ->
            let
                ( slideExampleState, cmd ) =
                    Examples.Slide.update msg moduleStates.slideExampleState
            in
            ( { moduleStates | slideExampleState = slideExampleState }
            , Cmd.map SlideExampleMsg cmd
            )

        SortableTableMsg msg ->
            let
                ( sortableTableState, cmd ) =
                    Examples.SortableTable.update msg moduleStates.sortableTableState
            in
            ( { moduleStates | sortableTableState = sortableTableState }
            , Cmd.map SortableTableMsg cmd
            )

        TabsExampleMsg tab ->
            ( { moduleStates | tabsExampleState = tab }
            , Cmd.none
            )

        TooltipExampleMsg msg ->
            let
                newState =
                    Examples.Tooltip.update msg moduleStates.tooltipExampleState
            in
            ( { moduleStates
                | tooltipExampleState = newState
              }
            , Cmd.none
            )

        NoOp ->
            ( moduleStates, Cmd.none )


subscriptions : ModuleStates -> Sub Msg
subscriptions moduleStates =
    Sub.batch
        [ Sub.map ModalExampleMsg (Examples.Modal.subscriptions moduleStates.modalExampleState)
        ]


{-| A container with a visually-apparent size for demonstrating how style guide components
fill their parents.
-}
container : Int -> List (Html msg) -> Html msg
container width children =
    Html.div
        [ Html.Attributes.class "demo-container"
        , style "width" (Debug.toString width ++ "px")
        ]
        children


nriThemedModules : ModuleStates -> List (ModuleExample Msg)
nriThemedModules model =
    [ Examples.Alert.example
    , Examples.BannerAlert.example BannerAlertExampleMsg model.bannerAlertExampleState
    , Examples.Button.example (exampleMessages ButtonExampleMsg) model.buttonExampleState
    , Examples.Callout.example
    , Examples.ClickableText.example (exampleMessages ClickableTextExampleMsg) model.clickableTextExampleState
    , Examples.Checkbox.example CheckboxExampleMsg model.checkboxExampleState
    , Examples.Dropdown.example DropdownMsg model.dropdownState
    , Examples.Icon.example
    , Examples.Page.example NoOp
    , Examples.SegmentedControl.example SegmentedControlMsg model.segmentedControlState
    , Examples.Select.example SelectMsg model.selectState
    , Examples.Text.example
    , Examples.Text.Writing.example
    , Examples.Fonts.example
    , Examples.Table.example TableExampleMsg model.tableExampleState
    , TextAreaExample.example TextAreaExampleMsg model.textAreaExampleState
    , TextInputExample.example TextInputExampleMsg model.textInputExampleState
    , Examples.DisclosureIndicator.example DisclosureIndicatorExampleMsg model.disclosureIndicatorExampleState
    , Examples.Colors.example
    , Examples.Modal.example ModalExampleMsg model.modalExampleState
    , Examples.SlideModal.example SlideModalExampleMsg model.slideModalExampleState
    , Examples.Slide.example SlideExampleMsg model.slideExampleState
    , Examples.SortableTable.example SortableTableMsg model.sortableTableState
    , Examples.Tabs.example TabsExampleMsg model.tabsExampleState
    , Examples.Tooltip.example TooltipExampleMsg model.tooltipExampleState
    ]


exampleMessages : (msg -> Msg) -> String -> ModuleExample.ModuleMessages msg Msg
exampleMessages exampleMessageWrapper exampleName =
    { noOp = NoOp
    , showItWorked = ShowItWorked exampleName
    , wrapper = exampleMessageWrapper
    }


route : Url -> Maybe String
route location =
    location.fragment
        |> Maybe.map (String.dropLeft 1)
