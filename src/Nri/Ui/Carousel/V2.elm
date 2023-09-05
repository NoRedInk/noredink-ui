module Nri.Ui.Carousel.V2 exposing
    ( viewWithPreviousAndNextControls
    , viewWithTabControls
    , viewWithCombinedControls
    , LabelledBy(..), Role(..)
    )

{-| Patch changes:

  - added new carousel APIs (with tabbed controls/previous and next controls, and combined controls) using [W3C guidelines](https://www.w3.org/WAI/ARIA/apg/patterns/carousel/)

@docs viewWithPreviousAndNextControls
@docs viewWithTabControls
@docs viewWithCombinedControls
@docs LabelledBy, Role

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key
import Accessibility.Styled.Role as Role
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attrs exposing (id)
import List.Extra
import Maybe.Extra
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Svg.V1 exposing (Svg)
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

  - `slides` the container with the carousel contents
  - `viewPreviousButton` previous button
  - `viewNextButton` next button
  - `containerAttributes` attributes that should be used on the parent div of both the button and slides elements

-}
viewWithPreviousAndNextControls :
    { selected : id
    , slides :
        List
            { id : id
            , idString : String
            , accessibleLabel : String
            , visibleLabelId : Maybe String
            , slideHtml : Html msg
            }
    , previousButton : { name : String, icon : Svg, attributes : List (ClickableSvg.Attribute msg) }
    , nextButton : { name : String, icon : Svg, attributes : List (ClickableSvg.Attribute msg) }
    , accessibleLabel : String
    , visibleLabelId : Maybe String
    , role : Role
    , announceAndSelect : { select : id, announce : String } -> msg
    }
    ->
        { viewPreviousButton : Html msg
        , viewNextButton : Html msg
        , slides : Html msg
        , containerAttributes : List (Attribute msg)
        }
viewWithPreviousAndNextControls config =
    let
        { viewPreviousButton, viewNextButton } =
            case findPreviousAndNextSlides .id config of
                Nothing ->
                    { viewPreviousButton = text "", viewNextButton = text "" }

                Just { previousSlide, nextSlide } ->
                    { viewPreviousButton =
                        viewSlideChangeButton
                            { name = config.previousButton.name
                            , icon = config.previousButton.icon
                            , attributes = config.previousButton.attributes
                            , targetSlideId = previousSlide.id
                            , targetSlideLabel = previousSlide.accessibleLabel
                            , carouselLabel = config.accessibleLabel
                            , announceAndSelect = config.announceAndSelect
                            }
                    , viewNextButton =
                        viewSlideChangeButton
                            { name = config.nextButton.name
                            , icon = config.nextButton.icon
                            , attributes = config.nextButton.attributes
                            , targetSlideId = nextSlide.id
                            , targetSlideLabel = nextSlide.accessibleLabel
                            , carouselLabel = config.accessibleLabel
                            , announceAndSelect = config.announceAndSelect
                            }
                    }
    in
    { viewPreviousButton = viewPreviousButton
    , viewNextButton = viewNextButton
    , slides =
        List.map
            (\slide ->
                Html.div
                    [ Role.group
                    , Aria.roleDescription "slide"
                    , id slide.idString
                    , labelAttribute slide

                    -- use as attribute for testing
                    , if config.selected == slide.id then
                        Attrs.style "display" "block"

                      else
                        Attrs.style "display" "none"
                    ]
                    [ slide.slideHtml ]
            )
            config.slides
            |> Html.div [ Attrs.attribute "atomic" "false", Accessibility.Styled.Key.tabbable False ]
    , containerAttributes =
        [ Attrs.attribute "role" (roleToString config.role)
        , Aria.roleDescription "carousel"
        , labelAttribute config
        ]
    }


findPreviousAndNextSlides :
    (slide -> id)
    -> { config | selected : id, slides : List slide }
    -> Maybe { previousSlide : slide, nextSlide : slide }
findPreviousAndNextSlides getId { selected, slides } =
    let
        currentIndex =
            slides
                |> List.Extra.findIndex (\slide -> getId slide == selected)
                |> -- assuming the provided id is valid. there's not much we can
                   -- do otherwise, anyways!
                   Maybe.withDefault 0

        length =
            List.length slides

        getByIndexWrappingAround index =
            List.Extra.getAt (modBy length index) slides
    in
    case ( getByIndexWrappingAround (currentIndex - 1), getByIndexWrappingAround (currentIndex + 1) ) of
        ( Just a, Just b ) ->
            Just { previousSlide = a, nextSlide = b }

        _ ->
            Nothing


viewSlideChangeButton :
    { name : String
    , icon : Svg
    , attributes : List (ClickableSvg.Attribute msg)
    , targetSlideId : id
    , targetSlideLabel : String
    , carouselLabel : String
    , announceAndSelect : { select : id, announce : String } -> msg
    }
    -> Html msg
viewSlideChangeButton { name, icon, attributes, targetSlideId, targetSlideLabel, carouselLabel, announceAndSelect } =
    ClickableSvg.button name
        icon
        (attributes
            ++ [ ClickableSvg.onClick
                    (announceAndSelect
                        { select = targetSlideId
                        , announce = "Active slide of " ++ carouselLabel ++ " changed to " ++ targetSlideLabel
                        }
                    )
               ]
        )


{-| Builds a carousel with tab buttons
Returns:
`controls`: tabs control buttons
`slides` container with the carousel contents
-}
viewWithTabControls :
    { cfg
        | selected : id
        , slides :
            List
                { id : id
                , idString : String
                , accessibleLabel : String
                , visibleLabelId : Maybe String
                , slideHtml : Html msg
                , tabControlHtml : Html Never
                }
        , tabControlStyles : Bool -> List Style
        , tabControlListStyles : List Style
        , role : Role
        , accessibleLabel : String
        , visibleLabelId : Maybe String
        , focusAndSelect : { select : id, focus : Maybe String } -> msg
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
                , tabs = List.map buildTab config.slides
                , tabStyles = always config.tabControlStyles
                , tabListStyles = config.tabControlListStyles
                }
    in
    { controls = tabList
    , slides = tabPanels
    , containerAttributes =
        [ Attrs.attribute "role" (roleToString config.role)
        , Aria.roleDescription "carousel"
        , labelAttribute config
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
    , slides :
        List
            { id : id
            , idString : String
            , accessibleLabel : String
            , visibleLabelId : Maybe String
            , slideHtml : Html msg
            , tabControlHtml : Html Never
            }
    , tabControlStyles : Bool -> List Style
    , tabControlListStyles : List Style
    , previousButton : { name : String, icon : Svg, attributes : List (ClickableSvg.Attribute msg) }
    , nextButton : { name : String, icon : Svg, attributes : List (ClickableSvg.Attribute msg) }
    , role : Role
    , accessibleLabel : String
    , visibleLabelId : Maybe String
    , focusAndSelect : { select : id, focus : Maybe String } -> msg
    , announceAndSelect : { select : id, announce : String } -> msg
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

        -- let's de duplicate this!
        currentSlideIndex =
            List.Extra.findIndex (\p -> p.id == config.selected) config.slides

        previousSlide =
            currentSlideIndex
                |> Maybe.andThen
                    (\index ->
                        List.Extra.getAt
                            (if index - 1 >= 0 then
                                index - 1

                             else
                                List.length config.slides - 1
                            )
                            config.slides
                    )

        nextSlide =
            currentSlideIndex
                |> Maybe.andThen
                    (\index ->
                        List.Extra.getAt
                            (if index + 1 < List.length config.slides then
                                index + 1

                             else
                                0
                            )
                            config.slides
                    )
    in
    { tabControls = controls
    , slides = slides
    , containerAttributes = containerAttributes
    , viewPreviousButton =
        ClickableSvg.button config.previousButton.name
            config.previousButton.icon
            (config.previousButton.attributes
                ++ (Maybe.map (\p -> ClickableSvg.onClick (config.focusAndSelect { select = p.id, focus = Just p.idString })) previousSlide
                        |> Maybe.Extra.toList
                   )
            )
    , viewNextButton =
        ClickableSvg.button config.nextButton.name
            config.nextButton.icon
            (config.nextButton.attributes
                ++ (Maybe.map (\p -> ClickableSvg.onClick (config.focusAndSelect { select = p.id, focus = Just p.idString })) nextSlide
                        |> Maybe.Extra.toList
                   )
            )
    }


labelAttribute : { c | accessibleLabel : String, visibleLabelId : Maybe String } -> Attribute msg
labelAttribute { accessibleLabel, visibleLabelId } =
    case visibleLabelId of
        Just visibleLabelId_ ->
            Aria.labeledBy visibleLabelId_

        Nothing ->
            Aria.label accessibleLabel


roleToString : Role -> String
roleToString role =
    case role of
        Group ->
            "group"

        Region ->
            "region"
