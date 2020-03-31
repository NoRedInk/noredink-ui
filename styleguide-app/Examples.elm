module Examples exposing (ModuleStates, Msg, init, nriThemedModules, subscriptions, update, view)

import Category exposing (Category(..))
import Examples.Accordion as Accordion
import Examples.Alert
import Examples.AssignmentIcon
import Examples.BannerAlert
import Examples.Button
import Examples.Callout
import Examples.Checkbox
import Examples.ClickableSvg
import Examples.ClickableText
import Examples.Colors
import Examples.DisclosureIndicator
import Examples.Dropdown
import Examples.Fonts
import Examples.Heading
import Examples.Icon
import Examples.Logo
import Examples.MasteryIcon
import Examples.Modal
import Examples.Page
import Examples.Pennant
import Examples.SegmentedControl
import Examples.Select
import Examples.Slide
import Examples.SlideModal
import Examples.SortableTable
import Examples.Svg
import Examples.Table
import Examples.Tabs
import Examples.Text
import Examples.Text.Writing
import Examples.TextArea as TextAreaExample
import Examples.TextInput as TextInputExample
import Examples.Tooltip
import Examples.UiIcon
import Html.Styled as Html exposing (Html)
import ModuleExample exposing (ModuleExample)
import Sort.Set as Set exposing (Set)


type alias Example state msg =
    { name : String
    , state : state
    , update : msg -> state -> ( state, Cmd msg )
    , view : state -> List (Html msg)
    , categories : List Category
    }


type alias ModuleStates =
    { accordion : Example Accordion.State Accordion.Msg

    --, buttonExample : { state : Examples.Button.State Msg
    --        , update :Button.State Msg -> Button.State Msg -> (Button.State Msg, Cmd Button.State Msg) }
    --, bannerAlertExample : { state : Examples.BannerAlert.State
    --        , update :Examples.BannerAlert.Msg -> Examples.BannerAlert.State -> (Examples.BannerAlert.State, Cmd Examples.BannerAlert.Msg) }
    --, clickableTextExample : { state : Examples.ClickableText.State
    --        , update :Examples.ClickableText.Msg -> Examples.ClickableText.State -> (Examples.ClickableText.State, Cmd Examples.ClickableText.Msg) }
    --, checkboxExample : { state : Examples.Checkbox.State
    --        , update :Examples.Checkbox.Msg -> Examples.Checkbox.State -> (Examples.Checkbox.State, Cmd Examples.Checkbox.Msg) }
    --, dropdowe : { state : Examples.Dropdown.State Examples.Dropdown.Value
    --        , update :Examples.Dropdown.Msg -> Examples.Dropdown.Value -> (Examples.Dropdown.Value, Cmd Examples.Dropdown.Msg) }
    --, segmentedControe : { state : Examples.SegmentedControl.State
    --        , update :Examples.SegmentedControl.Msg -> Examples.SegmentedControl.State -> (Examples.SegmentedControl.State, Cmd Examples.SegmentedControl.Msg) }
    --, selece : { state : Examples.Select.State
    --        , update :Examples.Select.Msg -> Examples.Select.State -> (Examples.Select.State, Cmd Examples.Select.Msg) }
    --, tableExample : { state : Examples.Table.State
    --        , update :Examples.Table.Msg -> Examples.Table.State -> (Examples.Table.State, Cmd Examples.Table.Msg) }
    --, textAreaExample : { state : TextAreaExample.State
    --        , update : TextAreaExample.Msg -> :: TextAreaExample.State -> (: TextAreaExample.State, Cmd : TextAreaExample.Msg) }
    --, textInputExample : { state : TextInputExample.State
    --        , update : TextInputExample.Msg -> :: TextInputExample.State -> (: TextInputExample.State, Cmd : TextInputExample.Msg) }
    --, disclosureIndicatorExample : { state : Examples.DisclosureIndicator.State
    --        , update :Examples.DisclosureIndicator.Msg -> Examples.DisclosureIndicator.State -> (Examples.DisclosureIndicator.State, Cmd Examples.DisclosureIndicator.Msg) }
    --, modalExample : { state : Examples.Modal.State
    --        , update :Examples.Modal.Msg -> Examples.Modal.State -> (Examples.Modal.State, Cmd Examples.Modal.Msg) }
    --, slideModalExample : { state : Examples.SlideModal.State
    --        , update :Examples.SlideModal.Msg -> Examples.SlideModal.State -> (Examples.SlideModal.State, Cmd Examples.SlideModal.Msg) }
    --, slideExample : { state : Examples.Slide.State
    --        , update :Examples.Slide.Msg -> Examples.Slide.State -> (Examples.Slide.State, Cmd Examples.Slide.Msg) }
    --, sortableTable : { state : Examples.SortableTable.State
    --        , update :Examples.SortableTable.Msg -> Examples.SortableTable.State -> (Examples.SortableTable.State, Cmd Examples.SortableTable.Msg) }
    --, sve : { state : Examples.Svg.State
    --        , update :Examples.Svg.Msg -> Examples.Svg.State -> (Examples.Svg.State, Cmd Examples.Svg.Msg) }
    --, clickableSve : { state : Examples.ClickableSvg.State
    --        , update :Examples.ClickableSvg.Msg -> Examples.ClickableSvg.State -> (Examples.ClickableSvg.State, Cmd Examples.ClickableSvg.Msg) }
    --, tabsExample : { state : Examples.Tabs.Tab
    --        , update :Examples.Tabs.Msg -> Examples.Tabs.Tab -> (Examples.Tabs.Tab, Cmd Examples.Tabs.Msg) }
    --, tooltipExample : { state : Examples.Tooltip.State
    --        , update :Examples.Tooltip.Msg -> Examples.Tooltip.State -> (Examples.Tooltip.State, Cmd Examples.Tooltip.Msg) }
    }


init : ModuleStates
init =
    { accordion = Accordion.example_

    --, buttonExample = { state = Examples.Button.init, update = Examples.Button.init }
    --, bannerAlertExample = { state = Examples.BannerAlert.init, update = Examples.BannerAlert.init }
    --, clickableTextExample = { state = Examples.ClickableText.init, update = Examples.ClickableText.init }
    --, checkboxExample = { state = Examples.Checkbox.init, update = Examples.Checkbox.init }
    --, dropdown = { state = Examples.Dropdown.init, update = Examples.Dropdown.init }
    --, segmentedControl = { state = Examples.SegmentedControl.init, update = Examples.SegmentedControl.init }
    --, select = { state = Examples.Select.init, update = Examples.Select.init }
    --, tableExample = { state = Examples.Table.init, update = Examples.Table.init }
    --, textAreaExample = { state = TextAreaExample.init, update = = TextAreaExample.init }
    --, textInputExample = { state = TextInputExample.init, update = = TextInputExample.init }
    --, disclosureIndicatorExample = { state = Examples.DisclosureIndicator.init, update = Examples.DisclosureIndicator.init }
    --, modalExample = { state = Examples.Modal.init, update = Examples.Modal.init }
    --, slideModalExample = { state = Examples.SlideModal.init, update = Examples.SlideModal.init }
    --, slideExample = { state = Examples.Slide.init, update = Examples.Slide.init }
    --, sortableTable = { state = Examples.SortableTable.init, update = Examples.SortableTable.init }
    --, svg = { state = Examples.Svg.init, update = Examples.Svg.init }
    --, clickableSvg = { state = Examples.ClickableSvg.init, update = Examples.ClickableSvg.init }
    --, tabsExample = { state = Examples.Tabs.First, update = Examples.Tabs.First }
    --, tooltipExample = { state = Examples.Tooltip.init, update = Examples.Tooltip.init }
    }


type Msg
    = ComponentMsg (ModuleStates -> Example Accordion.State Accordion.Msg) (Example Accordion.State Accordion.Msg -> ModuleStates -> ModuleStates) Accordion.Msg
      --| ButtonExampleMsg (Examples.Button.Msg Msg)
      --| BannerAlertExampleMsg Examples.BannerAlert.Msg
      --| ClickableTextExampleMsg Examples.ClickableText.Msg
      --| CheckboxExampleMsg Examples.Checkbox.Msg
      --| DropdownMsg Examples.Dropdown.Msg
      --| SegmentedControlMsg Examples.SegmentedControl.Msg
      --| SelectMsg Examples.Select.Msg
      --| TableExampleMsg Examples.Table.Msg
      --| TextAreaExampleMsg TextAreaExample.Msg
      --| TextInputExampleMsg TextInputExample.Msg
      --| DisclosureIndicatorExampleMsg Examples.DisclosureIndicator.Msg
      --| ModalExampleMsg Examples.Modal.Msg
      --| SlideModalExampleMsg Examples.SlideModal.Msg
      --| SlideExampleMsg Examples.Slide.Msg
      --| SortableTableMsg Examples.SortableTable.Msg
      --| SvgMsg Examples.Svg.Msg
      --| ClickableSvgMsg Examples.ClickableSvg.Msg
      --| TabsExampleMsg Examples.Tabs.Tab
      --| TooltipExampleMsg Examples.Tooltip.Msg
    | ShowItWorked String String
    | NoOp


update : Msg -> ModuleStates -> ( ModuleStates, Cmd Msg )
update outsideMsg moduleStates =
    case outsideMsg of
        ComponentMsg accessor updater msg ->
            let
                module_ =
                    accessor moduleStates

                ( newState, cmd ) =
                    module_.update msg module_.state
            in
            ( updater { module_ | state = newState } moduleStates
            , Cmd.map (ComponentMsg accessor updater) cmd
            )

        --ButtonExampleMsg msg ->
        --    let
        --        ( buttonExampleState, cmd ) =
        --            Examples.Button.update msg moduleStates.buttonExampleState
        --    in
        --    ( { moduleStates | buttonExampleState = buttonExampleState }
        --    , Cmd.map ButtonExampleMsg cmd
        --    )
        --BannerAlertExampleMsg msg ->
        --    ( { moduleStates
        --        | bannerAlertExampleState =
        --            Examples.BannerAlert.update msg moduleStates.bannerAlertExampleState
        --      }
        --    , Cmd.none
        --    )
        --ClickableTextExampleMsg msg ->
        --    let
        --        ( clickableTextExampleState, cmd ) =
        --            Examples.ClickableText.update msg moduleStates.clickableTextExampleState
        --    in
        --    ( { moduleStates | clickableTextExampleState = clickableTextExampleState }
        --    , Cmd.map ClickableTextExampleMsg cmd
        --    )
        --CheckboxExampleMsg msg ->
        --    let
        --        ( checkboxExampleState, cmd ) =
        --            Examples.Checkbox.update msg moduleStates.checkboxExampleState
        --    in
        --    ( { moduleStates | checkboxExampleState = checkboxExampleState }, Cmd.map CheckboxExampleMsg cmd )
        --DropdownMsg msg ->
        --    let
        --        ( dropdownState, cmd ) =
        --            Examples.Dropdown.update msg moduleStates.dropdownState
        --    in
        --    ( { moduleStates | dropdownState = dropdownState }
        --    , Cmd.map DropdownMsg cmd
        --    )
        --SegmentedControlMsg msg ->
        --    let
        --        ( segmentedControlState, cmd ) =
        --            Examples.SegmentedControl.update msg moduleStates.segmentedControlState
        --    in
        --    ( { moduleStates | segmentedControlState = segmentedControlState }
        --    , Cmd.map SegmentedControlMsg cmd
        --    )
        --SelectMsg msg ->
        --    let
        --        ( selectState, cmd ) =
        --            Examples.Select.update msg moduleStates.selectState
        --    in
        --    ( { moduleStates | selectState = selectState }
        --    , Cmd.map SelectMsg cmd
        --    )
        --SvgMsg msg ->
        --    let
        --        ( svgState, cmd ) =
        --            Examples.Svg.update msg moduleStates.svgState
        --    in
        --    ( { moduleStates | svgState = svgState }
        --    , Cmd.map SvgMsg cmd
        --    )
        --ClickableSvgMsg msg ->
        --    let
        --        ( clickableSvgState, cmd ) =
        --            Examples.ClickableSvg.update msg moduleStates.clickableSvgState
        --    in
        --    ( { moduleStates | clickableSvgState = clickableSvgState }
        --    , Cmd.map ClickableSvgMsg cmd
        --    )
        --TableExampleMsg msg ->
        --    let
        --        ( tableExampleState, cmd ) =
        --            Examples.Table.update msg moduleStates.tableExampleState
        --    in
        --    ( { moduleStates | tableExampleState = tableExampleState }
        --    , Cmd.map TableExampleMsg cmd
        --    )
        --TextAreaExampleMsg msg ->
        --    let
        --        ( textAreaExampleState, cmd ) =
        --            TextAreaExample.update msg moduleStates.textAreaExampleState
        --    in
        --    ( { moduleStates | textAreaExampleState = textAreaExampleState }
        --    , Cmd.map TextAreaExampleMsg cmd
        --    )
        --TextInputExampleMsg msg ->
        --    let
        --        ( textInputExampleState, cmd ) =
        --            TextInputExample.update msg moduleStates.textInputExampleState
        --    in
        --    ( { moduleStates | textInputExampleState = textInputExampleState }
        --    , Cmd.map TextInputExampleMsg cmd
        --    )
        --DisclosureIndicatorExampleMsg msg ->
        --    let
        --        ( disclosureIndicatorExampleState, cmd ) =
        --            Examples.DisclosureIndicator.update msg moduleStates.disclosureIndicatorExampleState
        --    in
        --    ( { moduleStates | disclosureIndicatorExampleState = disclosureIndicatorExampleState }
        --    , Cmd.map DisclosureIndicatorExampleMsg cmd
        --    )
        --ModalExampleMsg msg ->
        --    let
        --        ( modalExampleState, cmd ) =
        --            Examples.Modal.update msg moduleStates.modalExampleState
        --    in
        --    ( { moduleStates | modalExampleState = modalExampleState }
        --    , Cmd.map ModalExampleMsg cmd
        --    )
        --SlideModalExampleMsg msg ->
        --    let
        --        ( slideModalExampleState, cmd ) =
        --            Examples.SlideModal.update msg moduleStates.slideModalExampleState
        --    in
        --    ( { moduleStates | slideModalExampleState = slideModalExampleState }
        --    , Cmd.map SlideModalExampleMsg cmd
        --    )
        --SlideExampleMsg msg ->
        --    let
        --        ( slideExampleState, cmd ) =
        --            Examples.Slide.update msg moduleStates.slideExampleState
        --    in
        --    ( { moduleStates | slideExampleState = slideExampleState }
        --    , Cmd.map SlideExampleMsg cmd
        --    )
        --SortableTableMsg msg ->
        --    let
        --        ( sortableTableState, cmd ) =
        --            Examples.SortableTable.update msg moduleStates.sortableTableState
        --    in
        --    ( { moduleStates | sortableTableState = sortableTableState }
        --    , Cmd.map SortableTableMsg cmd
        --    )
        --TabsExampleMsg tab ->
        --    ( { moduleStates | tabsExampleState = tab }
        --    , Cmd.none
        --    )
        --TooltipExampleMsg msg ->
        --    let
        --        newState =
        --            Examples.Tooltip.update msg moduleStates.tooltipExampleState
        --    in
        --    ( { moduleStates
        --        | tooltipExampleState = newState
        --      }
        --    , Cmd.none
        --    )
        ShowItWorked group message ->
            let
                _ =
                    Debug.log group message
            in
            ( moduleStates, Cmd.none )

        NoOp ->
            ( moduleStates, Cmd.none )


subscriptions : ModuleStates -> Sub Msg
subscriptions moduleStates =
    Sub.batch
        [-- Sub.map ModalExampleMsg (Examples.Modal.subscriptions moduleStates.modalExampleState)
        ]


{-| -}
view : ModuleStates -> List (ModuleExample Msg)
view moduleStates =
    [ { name = moduleStates.accordion.name
      , categories = Set.fromList Category.sorter moduleStates.accordion.categories
      , content =
            List.map (Html.map (ComponentMsg .accordion (\state m -> { m | accordion = state })))
                (moduleStates.accordion.view moduleStates.accordion.state)
      }
    ]



--let
--            accordion =
--                moduleStates.accordion
--            ( newState, cmd ) =
--                accordion.update msg accordion.state
--        in
--        ( { moduleStates | accordion = { accordion | state = newState } }
--        , Cmd.map ComponentMsg cmd
--        )


nriThemedModules : ModuleStates -> List (ModuleExample Msg)
nriThemedModules model =
    [--Examples.Alert.example
     --Examples.Accordion.example AccordionExampleMsg model.accordion.state
     --, Examples.AssignmentIcon.example
     --, Examples.BannerAlert.example BannerAlertExampleMsg model.bannerAlertExampleState
     --, Examples.Button.example (exampleMessages ButtonExampleMsg) model.buttonExampleState
     --, Examples.Callout.example
     --, Examples.Checkbox.example CheckboxExampleMsg model.checkboxExampleState
     --, Examples.ClickableText.example (exampleMessages ClickableTextExampleMsg) model.clickableTextExampleState
     --, Examples.Colors.example
     --, Examples.DisclosureIndicator.example DisclosureIndicatorExampleMsg model.disclosureIndicatorExampleState
     --, Examples.Dropdown.example DropdownMsg model.dropdownState
     --, Examples.Fonts.example
     --, Examples.Heading.example
     --, Examples.Icon.example
     --, Examples.Logo.example
     --, Examples.MasteryIcon.example
     --, Examples.Modal.example ModalExampleMsg model.modalExampleState
     --, Examples.Page.example NoOp
     --, Examples.Pennant.example
     --, Examples.SegmentedControl.example SegmentedControlMsg model.segmentedControlState
     --, Examples.Select.example SelectMsg model.selectState
     --, Examples.Slide.example SlideExampleMsg model.slideExampleState
     --, Examples.SlideModal.example SlideModalExampleMsg model.slideModalExampleState
     --, Examples.SortableTable.example SortableTableMsg model.sortableTableState
     --, Examples.Svg.example (exampleMessages SvgMsg) model.svgState
     --, Examples.ClickableSvg.example (exampleMessages ClickableSvgMsg) model.clickableSvgState
     --, Examples.Table.example TableExampleMsg model.tableExampleState
     --, Examples.Tabs.example TabsExampleMsg model.tabsExampleState
     --, Examples.Text.example
     --, Examples.Text.Writing.example
     --, Examples.Tooltip.example TooltipExampleMsg model.tooltipExampleState
     --, Examples.UiIcon.example
     --, TextAreaExample.example TextAreaExampleMsg model.textAreaExampleState
     --, TextInputExample.example TextInputExampleMsg model.textInputExampleState
    ]


exampleMessages : (msg -> Msg) -> String -> ModuleExample.ModuleMessages msg Msg
exampleMessages exampleMessageWrapper exampleName =
    { noOp = NoOp
    , showItWorked = ShowItWorked exampleName
    , wrapper = exampleMessageWrapper
    }
