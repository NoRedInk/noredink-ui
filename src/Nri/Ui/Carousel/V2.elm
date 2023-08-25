module Nri.Ui.Carousel.V2 exposing
    ( viewWithCombinedControls, viewWithPreviousAndNextControls
    , viewWithTabControls, LabelledBy(..), Role(..)
    )

{-| Patch changes:

  - added new carousel APIs (with tabbed controls/previous and next controls, and combined controls) using [W3C guidelines](https://www.w3.org/WAI/ARIA/apg/patterns/carousel/)

@docs viewWithCombinedControls, viewWithPreviousAndNextControls
@docs viewWithTabControls, LabelledBy, Role

-}

import Accessibility.Styled.Aria as Aria
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attrs exposing (css)
import TabsInternal.V2 as TabsInternal


{-| Type which represents the type of aria label which will be used
`LabelledByIdOfVisibleLabel` will point to an existing element id on the DOM
`LabelledByAccessibleLabelOnly` will be a label of the element
-}
type LabelledBy
    = LabelledByIdOfVisibleLabel String
    | LabelledByAccessibleLabelOnly String


{-| `Role`, which can be either [Group](https://w3c.github.io/aria/#group) or [Region](https://w3c.github.io/aria/#region)
-}
type Role
    = Group
    | Region


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
    , role : Role
    }
    ->
        { viewPreviousButton : Html msg
        , viewNextButton : Html msg
        , slides : Html msg
        , containerAttributes : List (Attribute msg)
        }
viewWithPreviousAndNextControls config =
    { viewPreviousButton = config.viewPreviousButton
    , viewNextButton = config.viewNextButton
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
        [ Attrs.attribute "role" (roleToString config.role)
        , Aria.roleDescription "carousel"
        , labelledByToAttr config.labelledBy
        ]
    }


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
        , role : Role
        , labelledBy : LabelledBy
    }
    ->
        { controls : Html msg
        , slides : Html msg
        , containerAttributes : List (Attribute msg)
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
    , containerAttributes =
        [ Attrs.attribute "role" (roleToString config.role)
        , Aria.roleDescription "carousel"
        , labelledByToAttr config.labelledBy
        ]
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
    , role : Role
    , labelledBy : LabelledBy
    }
    ->
        { tabControls : Html msg
        , viewPreviousButton : Html msg
        , viewNextButton : Html msg
        , slides : Html msg
        , containerAttributes : List (Attribute msg)
        }
viewWithCombinedControls config =
    let
        { controls, slides, containerAttributes } =
            viewWithTabControls config
    in
    { tabControls = controls
    , slides = slides
    , containerAttributes = containerAttributes
    , viewPreviousButton = config.viewPreviousButton
    , viewNextButton = config.viewNextButton
    }


labelledByToAttr : LabelledBy -> Attribute msg
labelledByToAttr label =
    case label of
        LabelledByIdOfVisibleLabel l ->
            Aria.labeledBy l

        LabelledByAccessibleLabelOnly l ->
            Aria.label l


roleToString : Role -> String
roleToString role =
    case role of
        Group ->
            "group"

        Region ->
            "region"
