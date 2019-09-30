module Examples.Tooltip exposing (example, init, update, State, Msg)

{-|

@docs example, init, update, State, Msg

-}

import Accessibility.Styled as Html
import Css
import Html.Styled.Attributes exposing (css)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Text.V3 as Text
import Nri.Ui.Tooltip.V1 as Tooltip


type TooltipType
    = PrimaryLabel
    | AuxillaryDescription
    | ToggleTip


type alias State =
    { openTooltip : Maybe TooltipType
    }


init : State
init =
    { openTooltip = Nothing }


type Msg
    = ToggleTooltip TooltipType Bool


update : Msg -> State -> State
update msg model =
    case msg of
        ToggleTooltip type_ isOpen ->
            if isOpen then
                { model | openTooltip = Just type_ }

            else
                { model | openTooltip = Nothing }


example : (Msg -> msg) -> State -> ModuleExample msg
example msg model =
    { name = "Nri.Ui.Tooltip.V1"
    , category = Widgets
    , content =
        [ Text.mediumBody [ Html.text "These tooltips look similar, but serve different purposes when reading them via a screen-reader." ]
        , Heading.h3 [] [ Html.text "primaryLabel" ]
        , Text.smallBody [ Html.text "A primary label is used when the tooltip content serves as the main label for its trigger content, e.g. when the trigger content is an icon." ]
        , Tooltip.tooltip [ Html.text "Tooltip" ]
            |> Tooltip.primaryLabel
                { trigger = Tooltip.OnClick
                , triggerHtml = Html.text "Primary Label Trigger"
                , onTrigger = ToggleTooltip PrimaryLabel >> msg
                , isOpen = model.openTooltip == Just PrimaryLabel
                , id = "primary label tooltip"
                }
        , Html.br [ css [ Css.marginBottom (Css.px 20) ] ]
        , Heading.h3 [] [ Html.text "auxillaryDescription" ]
        , Text.smallBody [ Html.text "An auxillary description is used when the tooltip content provides supplementary information about its trigger content." ]
        , Tooltip.tooltip [ Html.text "Tooltip" ]
            |> Tooltip.auxillaryDescription
                { trigger = Tooltip.OnClick
                , triggerHtml = Html.text "Auxillary Description Trigger"
                , onTrigger = ToggleTooltip AuxillaryDescription >> msg
                , isOpen = model.openTooltip == Just AuxillaryDescription
                , id = "Auxillary description"
                }
        , Html.br [ css [ Css.marginBottom (Css.px 20) ] ]
        , Heading.h3 [] [ Html.text "toggleTip" ]
        , Text.smallBody [ Html.text "A Toggle Tip is triggered by the \"?\" icon and provides supplemental information for the page." ]
        , Tooltip.tooltip [ Html.text "Tooltip" ]
            |> Tooltip.toggleTip
                { onTrigger = ToggleTooltip ToggleTip >> msg
                , isOpen = model.openTooltip == Just ToggleTip
                , id = "toggle tip"
                }
        ]
    }
