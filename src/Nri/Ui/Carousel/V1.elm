module Nri.Ui.Carousel.V1 exposing
    ( view
    , Item
    , buildItem
    )

{-| Patch changes:

  - remove tooltip-related code that is never used for Carousel

@docs view
@docs Item
@docs buildItem

-}

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import TabsInternal.V2 as TabsInternal


{-| -}
type Item id msg
    = Item (TabsInternal.Tab id msg)


{-| Builds an selectable item in the Caroursel

`controlHtml` represents the element that will appear in the list of options.

`slideHtml` represents the element that will be shown in your carousel when this item is selected.

-}
buildItem : { id : id, idString : String, slideHtml : Html msg, controlHtml : Html Never } -> Item id msg
buildItem config =
    Item
        (TabsInternal.fromList { id = config.id, idString = config.idString }
            [ \tab -> { tab | panelView = config.slideHtml }
            , \tab -> { tab | tabView = [ Html.map never config.controlHtml ] }
            ]
        )


{-| -}
view :
    { focusAndSelect : { select : id, focus : Maybe String } -> msg
    , selected : id
    , controlStyles : Bool -> List Style
    , controlListStyles : List Style
    , items : List (Item id msg)
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
