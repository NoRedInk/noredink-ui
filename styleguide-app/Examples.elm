module Examples exposing (ModuleStates, Msg, init, subscriptions, update, view)

import Category exposing (Category(..))
import Examples.Accordion as Accordion
import Examples.Alert
import Examples.AssignmentIcon
import Examples.BannerAlert as BannerAlert
import Examples.Button as Button
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
    , button : Example Button.State Button.Msg
    , bannerAlert : Example BannerAlert.State BannerAlert.Msg

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
    { accordion = Accordion.example
    , button = Button.example_
    , bannerAlert = BannerAlert.example

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
    = AccordionMsg Accordion.Msg
    | ButtonMsg Button.Msg
    | BannerAlertMsg BannerAlert.Msg


update : Msg -> ModuleStates -> ( ModuleStates, Cmd Msg )
update msg moduleStates =
    let
        updateWith accessor updater wrapMsg childMsg =
            let
                module_ =
                    accessor moduleStates

                ( newState, cmd ) =
                    module_.update childMsg module_.state
            in
            ( updater { module_ | state = newState } moduleStates
            , Cmd.map wrapMsg cmd
            )
    in
    case msg of
        AccordionMsg exampleMsg ->
            updateWith .accordion (\state m -> { m | accordion = state }) AccordionMsg exampleMsg

        ButtonMsg exampleMsg ->
            updateWith .button (\state m -> { m | button = state }) ButtonMsg exampleMsg

        BannerAlertMsg exampleMsg ->
            updateWith .bannerAlert (\state m -> { m | bannerAlert = state }) BannerAlertMsg exampleMsg


subscriptions moduleStates =
    Sub.batch
        [-- Sub.map ModalExampleMsg (Examples.Modal.subscriptions moduleStates.modalExampleState)
        ]


{-| -}
view : ModuleStates -> List (ModuleExample Msg)
view moduleStates =
    [ viewExample AccordionMsg moduleStates.accordion
    , viewExample ButtonMsg moduleStates.button
    , viewExample BannerAlertMsg moduleStates.bannerAlert
    ]


viewExample : (msg -> Msg) -> Example state msg -> ModuleExample Msg
viewExample wrapperMsg example =
    { name = example.name
    , categories = Set.fromList Category.sorter example.categories
    , content = List.map (Html.map wrapperMsg) (example.view example.state)
    }
