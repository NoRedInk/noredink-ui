module Nri.Ui.Carousel.V2 exposing
    ( viewWithCombinedControls, viewWithPreviousAndNextControls
    , viewWithTabControls, LabelledBy(..)
    )

{-| Patch changes:

  - added new carousel APIs (with tabbed controls/previous and next controls, and combined controls)

@docs viewWithCombinedControls, viewWithPreviousAndNextControls
@docs viewWithTabControls, AriaLabel

-}

import Accessibility.Styled.Aria as Aria
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attrs exposing (css)
import TabsInternal.V2 as TabsInternal


{-| Builds a carousel with previous and next controls
Returns:
`controls`: previous and next buttons element
`slides` the container with the carousel contents
`containerAttributes` attributes that should be used on the parent div of both the button and slides elements
-}
viewWithPreviousAndNextControls :
    { selected : id
    , panels :
        List
            { id : id
            , slideHtml : Html msg
            , labelledBy : LabelledBy
            }
    , viewPreviousButton : Html msg
    , viewNextButton : Html msg
    , labelledBy : LabelledBy
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
                    , labelledByToAttr panel.labelledBy
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
        , labelledByToAttr config.labelledBy
        ]
    }


labelledByToAttr : LabelledBy -> Attribute msg
labelledByToAttr label =
    case label of
        LabelledByIdOfVisibleLabel l ->
            Aria.labeledBy l

        LabelledByAccessibleLabelOnly l ->
            Aria.label l


{-| Type which represents the type of aria label which will be used
`LabelledByIdOfVisibleLabel` will point to an existing element id on the DOM
`LabelledByAccessibleLabelOnly` will be a label of the element
-}
type LabelledBy
    = LabelledByIdOfVisibleLabel String
    | LabelledByAccessibleLabelOnly String


{-| Builds a carousel with tab buttons
Returns:
`controls`: tabs control buttons
`slides` container with the carousel contents
-}
viewWithTabControls :
    { cfg
        | selected : id
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


{-| Builds a carousel with tab buttons
Returns:
`tabControls`: tabs control buttons
`slides` container with the carousel contents
`previousAndNextControls`: previous and next buttons element
-}
viewWithCombinedControls :
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
    , viewPreviousButton : Html msg
    , viewNextButton : Html msg
    }
    ->
        { tabControls : Html msg
        , previousAndNextControls : Html msg
        , slides : Html msg
        }
viewWithCombinedControls config =
    let
        { controls, slides } =
            viewWithTabControls config
    in
    { tabControls = controls
    , slides = slides
    , previousAndNextControls = div [] [ config.viewPreviousButton, config.viewNextButton ]
    }
