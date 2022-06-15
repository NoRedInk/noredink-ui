module Nri.Ui.Carousel.V1 exposing (..)

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Nri.Ui
import Nri.Ui.Tooltip.V3 as Tooltip
import TabsInternal.V2 as TabsInternal


type Tab id msg
    = Tab (TabsInternal.Tab id msg)


type Attribute id msg
    = Attribute (TabsInternal.Tab id msg -> TabsInternal.Tab id msg)


type TabPosition
    = Before
    | After


{-| Tooltip defaults: `[Tooltip.smallPadding, Tooltip.onBottom, Tooltip.fitToContent]`
-}
withTooltip : List (Tooltip.Attribute msg) -> Attribute id msg
withTooltip attributes =
    Attribute (\tab -> { tab | tabTooltip = attributes })


{-| Makes it so that the tab can't be clicked or focused via keyboard navigation
-}
disabled : Bool -> Attribute id msg
disabled isDisabled =
    Attribute (\tab -> { tab | disabled = isDisabled })


{-| -}
slideHtml : Html msg -> Attribute id msg
slideHtml content =
    Attribute (\tab -> { tab | panelView = content })


labelledBy : String -> Attribute id msg
labelledBy labelledById =
    Attribute (\tab -> { tab | labelledBy = Just labelledById })


tabHtml : Html Never -> Attribute id msg
tabHtml content =
    Attribute (\tab -> { tab | tabView = [ Html.map never content ] })


{-| -}
buildTab : { id : id, idString : String } -> List (Attribute id msg) -> Tab id msg
buildTab config attributes =
    Tab (TabsInternal.fromList config (List.map (\(Attribute f) -> f) attributes))


view :
    { focusAndSelect : { select : id, focus : Maybe String } -> msg
    , selected : id
    , tabs : List (Tab id msg)
    , tabStyles : Int -> Bool -> List Style
    , tabListStyles : List Style
    , containerStyles : List Style
    , tabListPosition : TabPosition
    }
    -> Html msg
view config =
    let
        { tabList, tabPanels } =
            TabsInternal.views
                { focusAndSelect = config.focusAndSelect
                , selected = config.selected
                , tabs = List.map (\(Tab t) -> t) config.tabs
                , tabStyles = config.tabStyles
                , tabListStyles = config.tabListStyles
                }
    in
    Nri.Ui.styled Html.div
        "Nri-Ui-Carousel__container"
        config.containerStyles
        []
        (case config.tabListPosition of
            Before ->
                [ tabList
                , tabPanels
                ]

            After ->
                [ tabPanels
                , tabList
                ]
        )
