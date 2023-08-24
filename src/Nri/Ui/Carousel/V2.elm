module Nri.Ui.Carousel.V2 exposing
    ( AriaLabel(..)
    , viewWithCombinedControls
    , viewWithPreviousAndNextControls
    , viewWithTabControls
    )

import Accessibility.Styled.Aria as Aria
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attrs exposing (css)
import TabsInternal.V2 as TabsInternal


viewWithPreviousAndNextControls :
    { selected : id
    , panels :
        List
            { id : id
            , slideHtml : Html msg
            , ariaLabel : AriaLabel
            }
    , viewPreviousButton : Html msg
    , viewNextButton : Html msg
    , ariaLabel : AriaLabel
    , controlListStyles : List Style
    }
    ->
        { controls : Html msg
        , slides : Html msg
        , containerAttributes : List (Attribute msg)
        }
viewWithPreviousAndNextControls config =
    { controls =
        div [ css config.controlListStyles ]
            [ config.viewPreviousButton, config.viewNextButton ]
    , slides =
        List.map
            (\panel ->
                Html.div
                    [ Attrs.attribute "role" "group"
                    , Aria.roleDescription "slide"
                    , ariaLabelToAttr panel.ariaLabel
                    , css
                        [ if config.selected == panel.id then
                            Css.display Css.block

                          else
                            Css.display Css.none
                        ]
                    ]
                    [ panel.slideHtml ]
            )
            config.panels
            |> Html.div [ Attrs.attribute "atomic" "false", Attrs.attribute "live" "polite" ]
    , containerAttributes =
        [ Attrs.attribute "role" "region"
        , Aria.roleDescription "carousel"
        , ariaLabelToAttr config.ariaLabel
        ]
    }


ariaLabelToAttr : AriaLabel -> Attribute msg
ariaLabelToAttr label =
    case label of
        IdLabel l ->
            Aria.labeledBy l

        StringLabel l ->
            Aria.label l


type AriaLabel
    = IdLabel String
    | StringLabel String


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
