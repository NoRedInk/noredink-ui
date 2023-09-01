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
import Accessibility.Styled.Role as Role
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attrs exposing (id)
import List.Extra
import Maybe.Extra
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Svg.V1 exposing (Svg)
import TabsInternal.V2 as TabsInternal
import Accessibility.Styled.Key


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
`slides` the container with the carousel contents
`viewPreviousButton` previous button
`viewNextButton` next button
`containerAttributes` attributes that should be used on the parent div of both the button and slides elements
-}
viewWithPreviousAndNextControls :
    { selected : id
    , panels :
        List
            { id : id
            , slideHtml : Html msg
            , labelledBy : LabelledBy
            , idString : String
            }
    , viewPreviousButton : { attributes : List (ClickableSvg.Attribute msg), icon : Svg, name : String }
    , viewNextButton : { attributes : List (ClickableSvg.Attribute msg), icon : Svg, name : String }
    , labelledBy : LabelledBy
    , role : Role
    , focusAndSelect : { select : id, focus : Maybe String } -> msg
    }
    ->
        { viewPreviousButton : Html msg
        , viewNextButton : Html msg
        , slides : Html msg
        , containerAttributes : List (Attribute msg)
        }
viewWithPreviousAndNextControls config =
    let
        currentPanelIndex =
            List.Extra.findIndex (\p -> p.id == config.selected) config.panels

        previousPanel =
            currentPanelIndex
                |> Maybe.andThen
                    (\index ->
                        List.Extra.getAt
                            (if index - 1 >= 0 then
                                index - 1

                             else
                                List.length config.panels - 1
                            )
                            config.panels
                    )

        nextPanel =
            currentPanelIndex
                |> Maybe.andThen
                    (\index ->
                        List.Extra.getAt
                            (if index + 1 < List.length config.panels then
                                index + 1

                             else
                                0
                            )
                            config.panels
                    )
    in
    { viewPreviousButton =
        ClickableSvg.button config.viewPreviousButton.name
            config.viewPreviousButton.icon
            (config.viewPreviousButton.attributes
                ++ (Maybe.map (\p -> ClickableSvg.onClick (config.focusAndSelect { select = p.id, focus = Just p.idString })) previousPanel
                        |> Maybe.Extra.toList
                   )
            )
    , viewNextButton =
        ClickableSvg.button config.viewNextButton.name
            config.viewNextButton.icon
            (config.viewNextButton.attributes
                ++ (Maybe.map (\p -> ClickableSvg.onClick (config.focusAndSelect { select = p.id, focus = Just p.idString })) nextPanel
                        |> Maybe.Extra.toList
                   )
            )
    , slides =
        List.map
            (\panel ->
                Html.div
                    [ Role.group
                    , Aria.roleDescription "slide"
                    , id panel.idString
                    , labelledByToAttr panel.labelledBy

                    -- use as attribute for testing
                    , if config.selected == panel.id then
                        Attrs.style "display" "block"

                      else
                        Attrs.style "display" "none"
                    ]
                    [ panel.slideHtml ]
            )
            config.panels
            |> Html.div [ Attrs.attribute "atomic" "false", Accessibility.Styled.Key.tabbable False ]
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
`viewPreviousButton` previous button
`viewNextButton` next button
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
    , viewPreviousButton : { attributes : List (ClickableSvg.Attribute msg), icon : Svg, name : String }
    , viewNextButton : { attributes : List (ClickableSvg.Attribute msg), icon : Svg, name : String }
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

        currentPanelIndex =
            List.Extra.findIndex (\p -> p.id == config.selected) config.panels

        previousPanel =
            currentPanelIndex
                |> Maybe.andThen
                    (\index ->
                        List.Extra.getAt
                            (if index - 1 >= 0 then
                                index - 1

                             else
                                List.length config.panels - 1
                            )
                            config.panels
                    )

        nextPanel =
            currentPanelIndex
                |> Maybe.andThen
                    (\index ->
                        List.Extra.getAt
                            (if index + 1 < List.length config.panels then
                                index + 1

                             else
                                0
                            )
                            config.panels
                    )
    in
    { tabControls = controls
    , slides = slides
    , containerAttributes = containerAttributes
    , viewPreviousButton =
        ClickableSvg.button config.viewPreviousButton.name
            config.viewPreviousButton.icon
            (config.viewPreviousButton.attributes
                ++ (Maybe.map (\p -> ClickableSvg.onClick (config.focusAndSelect { select = p.id, focus = Just p.idString })) previousPanel
                        |> Maybe.Extra.toList
                   )
            )
    , viewNextButton =
        ClickableSvg.button config.viewNextButton.name
            config.viewNextButton.icon
            (config.viewNextButton.attributes
                ++ (Maybe.map (\p -> ClickableSvg.onClick (config.focusAndSelect { select = p.id, focus = Just p.idString })) nextPanel
                        |> Maybe.Extra.toList
                   )
            )
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
