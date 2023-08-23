module Nri.Ui.Carousel.V2 exposing (viewWithCombinedControls, viewWithPreviousAndNextControls, viewWithTabControls)

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import TabsInternal.V2 as TabsInternal


{-| General concern: this doesn't enforce proper usage of attributes needed at
the top level carousel container (e.g. role=region and aria-roledescription=carousel)
-}
viewWithPreviousAndNextControls :
    { selected : id
    , panels :
        List
            { id : id
            , slideHtml : Html msg
            }
    , viewPreviousButton : Html msg
    , viewNextButton : Html msg
    }
    ->
        { controls : Html msg
        , slides : Html msg
        }
viewWithPreviousAndNextControls =
    -- Implements the Basic carousel pattern, without using TabsInternal
    Debug.todo "TODO"


viewWithTabControls :
    { selected : id
    , panels :
        List
            { id : id
            , slideHtml : Html msg
            , tabControlHtml : Html Never
            , idString : String
            }
    , focusAndSelect : { select : id, focus : Maybe String } -> msg
    , tabControlStyles : Bool -> List Style
    , tabControlListStyles : List Style
    }
    ->
        { controls : Html msg
        , slides : Html msg
        }
viewWithTabControls config =
    let
        buildTab tabConfig =
            TabsInternal.fromList { id = tabConfig.id, idString = tabConfig.idString }
                [ \tab -> { tab | panelView = tabConfig.slideHtml }
                , \tab -> { tab | tabView = [ Html.map never tabConfig.tabControlHtml ] }
                ]

        { tabList, tabPanels } =
            TabsInternal.views
                { focusAndSelect = config.focusAndSelect
                , selected = config.selected
                , tabs = List.map buildTab config.panels
                , tabStyles = always config.tabControlStyles
                , tabListStyles = config.tabControlListStyles
                }
    in
    { controls = tabList
    , slides = tabPanels
    }


viewWithCombinedControls :
    { selected : id
    , panels :
        List
            { id : id
            , slideHtml : Html msg
            , tabControlHtml : Html msg
            }
    , focusAndSelect : { select : id, focus : Maybe String } -> msg
    , viewTabControl : id -> Html msg
    , tabControlStyles : Bool -> List Style
    , tabControlListStyles : List Style
    , viewPreviousButton : Html msg
    , viewNextButton : Html msg
    }
    ->
        -- NOTE the change in the return type here!
        { tabControls : Html msg
        , previousAndNextControls : Html msg
        , slides : Html msg
        }
viewWithCombinedControls =
    -- Wraps the call to TabsInternal, adding previous/next buttons following
    -- advice from https://www.w3.org/WAI/ARIA/apg/patterns/carousel/.
    Debug.todo "TODO"
