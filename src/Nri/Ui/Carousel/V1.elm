module Nri.Ui.Carousel.V1 exposing (Item, buildItem, view)

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import TabsInternal.V2 as TabsInternal


type Item id msg
    = Item (TabsInternal.Tab id msg)


{-| -}
buildItem : { id : id, idString : String, slideHtml : Html msg, controlHtml : Html Never } -> Item id msg
buildItem config =
    Item
        (TabsInternal.fromList { id = config.id, idString = config.idString }
            [ \tab -> { tab | panelView = config.slideHtml }
            , \tab -> { tab | tabView = [ Html.map never config.controlHtml ] }
            ]
        )


view :
    { focusAndSelect : { select : id, focus : Maybe String } -> msg
    , selected : id
    , items : List (Item id msg)
    , controlStyles : Bool -> List Style
    , controlListStyles : List Style
    }
    -> { controls : Html msg, slides : Html msg }
view config =
    let
        { tabList, tabPanels } =
            TabsInternal.views
                { focusAndSelect = config.focusAndSelect
                , selected = config.selected
                , tabs = List.map (\(Item t) -> t) config.items
                , tabStyles = always config.controlStyles
                , tabListStyles = config.controlListStyles
                }
    in
    { controls = tabList
    , slides = tabPanels
    }
