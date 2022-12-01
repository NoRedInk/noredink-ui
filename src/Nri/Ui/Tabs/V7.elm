module Nri.Ui.Tabs.V7 exposing
    ( view
    , Alignment(..)
    , Tab, Attribute, build
    , tabString, tabHtml, withTooltip, disabled, labelledBy, describedBy
    , panelHtml
    , spaHref
    )

{-| Patch changes:

  - use Tooltip.V3 instead of Tooltip.V2

Changes from V6:

  - Changes Tab construction to follow attributes-based approach
  - Adds tooltip support
  - combine onFocus and onSelect into focusAndSelect msg handler (for tooltips)

@docs view
@docs Alignment
@docs Tab, Attribute, build
@docs tabString, tabHtml, withTooltip, disabled, labelledBy, describedBy
@docs panelHtml
@docs spaHref

-}

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Nri.Ui
import Nri.Ui.Colors.Extra exposing (withAlpha)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Tooltip.V3 as Tooltip
import TabsInternal.V2 as TabsInternal


{-| -}
type Tab id msg
    = Tab (TabsInternal.Tab id msg)


{-| -}
type Attribute id msg
    = Attribute (TabsInternal.Tab id msg -> TabsInternal.Tab id msg)


{-| -}
tabString : String -> Attribute id msg
tabString content =
    Attribute (\tab -> { tab | tabView = [ viewTabDefault content ] })


{-| -}
tabHtml : Html Never -> Attribute id msg
tabHtml content =
    Attribute (\tab -> { tab | tabView = [ Html.map never content ] })


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


{-| Sets an overriding labelledBy on the tab for an external tooltip.
This assumes an external tooltip is set and disables any internal tooltip configured.
-}
labelledBy : String -> Attribute id msg
labelledBy labelledById =
    Attribute (\tab -> { tab | labelledBy = Just labelledById })


{-| Like [`labelledBy`](#labelledBy), but it describes the given element
instead of labeling it.

This attribute can be used multiple times if more than one element describes
this tab.

-}
describedBy : String -> Attribute id msg
describedBy describedById =
    Attribute (\tab -> { tab | describedBy = describedById :: tab.describedBy })


{-| -}
panelHtml : Html msg -> Attribute id msg
panelHtml content =
    Attribute (\tab -> { tab | panelView = content })


{-| -}
spaHref : String -> Attribute id msg
spaHref url =
    Attribute (\tab -> { tab | spaHref = Just url })


{-| -}
tabAttributes : List (Html.Attribute msg) -> Attribute id msg
tabAttributes attrs =
    Attribute (\tab -> { tab | tabAttributes = tab.tabAttributes ++ attrs })


{-| -}
build : { id : id, idString : String } -> List (Attribute id msg) -> Tab id msg
build config attributes =
    Tab
        (TabsInternal.fromList config
            (List.map (\(Attribute f) -> f)
                (tabAttributes [ Attributes.class FocusRing.customClass ]
                    :: attributes
                )
            )
        )


{-| Determines whether tabs are centered or floating to the left or right.
-}
type Alignment
    = Left
    | Center
    | Right


{-| -}
view :
    { title : Maybe String
    , alignment : Alignment
    , customSpacing : Maybe Float
    , focusAndSelect : { select : id, focus : Maybe String } -> msg
    , selected : id
    , tabs : List (Tab id msg)
    }
    -> Html msg
view config =
    let
        { tabList, tabPanels } =
            TabsInternal.views
                { focusAndSelect = config.focusAndSelect
                , selected = config.selected
                , tabs = List.map (\(Tab t) -> t) config.tabs
                , tabStyles = tabStyles config.customSpacing
                , tabListStyles = stylesTabsAligned config.alignment
                }
    in
    Nri.Ui.styled Html.div
        "Nri-Ui-Tabs__container"
        []
        []
        [ Html.styled Html.div
            [ Css.displayFlex
            , Css.alignItems Css.flexEnd
            , Css.borderBottom (Css.px 1)
            , Css.borderBottomStyle Css.solid
            , Css.borderBottomColor Colors.navy
            , Fonts.baseFont
            ]
            []
            [ config.title
                |> Maybe.map viewTitle
                |> Maybe.withDefault (Html.text "")
            , tabList
            ]
        , tabPanels
        ]


{-| -}
viewTabDefault : String -> Html msg
viewTabDefault title =
    Html.div
        [ Attributes.css
            [ Css.padding4 (Css.px 14) (Css.px 20) (Css.px 12) (Css.px 20)
            ]
        ]
        [ Html.text title ]


viewTitle : String -> Html msg
viewTitle title =
    Html.styled Html.h1
        [ Css.flexGrow (Css.int 2)
        , Css.fontSize (Css.px 20)
        , Css.fontWeight Css.bold
        , Css.margin4 (Css.px 5) (Css.px 10) (Css.px 10) Css.zero
        , Css.color Colors.navy
        ]
        []
        [ Html.text title ]



-- STYLES


stylesTabsAligned : Alignment -> List Style
stylesTabsAligned alignment =
    let
        alignmentStyles =
            case alignment of
                Left ->
                    Css.justifyContent Css.flexStart

                Center ->
                    Css.justifyContent Css.center

                Right ->
                    Css.justifyContent Css.flexEnd
    in
    alignmentStyles
        :: [ Css.margin Css.zero
           , Css.fontSize (Css.px 19)
           , Css.displayFlex
           , Css.flexGrow (Css.int 1)
           , Css.padding Css.zero
           ]


tabStyles : Maybe Float -> Int -> Bool -> List Style
tabStyles customSpacing index isSelected =
    let
        stylesDynamic =
            if isSelected then
                [ Css.backgroundColor Colors.white
                , Css.borderBottom (Css.px 1)
                , Css.borderBottomStyle Css.solid
                , Css.borderBottomColor Colors.white
                ]

            else
                [ Css.backgroundColor Colors.frost
                , Css.backgroundImage <|
                    Css.linearGradient2 Css.toTop
                        (Css.stop2 (withAlpha 0.25 Colors.azure) (Css.pct 0))
                        (Css.stop2 (withAlpha 0 Colors.azure) (Css.pct 25))
                        [ Css.stop2 (withAlpha 0 Colors.azure) (Css.pct 100) ]
                ]

        baseStyles =
            [ Css.color Colors.navy
            , Css.position Css.relative

            -- necessary because bourbon or bootstrap or whatever add underlines when tabs are used as links
            , Css.textDecoration Css.none |> important
            , Css.property "background" "none"
            , Css.fontFamily Css.inherit
            , Css.fontSize Css.inherit
            , Css.cursor Css.pointer
            , Css.border zero
            , Css.height (Css.pct 100)
            ]

        stylesTab =
            [ Css.display Css.inlineBlock
            , Css.borderTopLeftRadius (Css.px 10)
            , Css.borderTopRightRadius (Css.px 10)
            , Css.border3 (Css.px 1) Css.solid Colors.navy
            , Css.marginTop Css.zero
            , Css.marginLeft
                (if index == 0 then
                    Css.px 0

                 else
                    Css.px margin
                )
            , Css.marginRight (Css.px margin)
            , Css.padding2 (Css.px 1) (Css.px 6)
            , Css.marginBottom (Css.px -1)
            , Css.cursor Css.pointer
            , property "transition" "background-color 0.2s"
            , property "transition" "border-color 0.2s"
            , hover
                [ backgroundColor Colors.white
                , borderTopColor Colors.azure
                , borderRightColor Colors.azure
                , borderLeftColor Colors.azure
                ]
            , pseudoClass "focus-visible"
                [ FocusRing.outerBoxShadow
                , Css.outline3 (Css.px 2) Css.solid Css.transparent
                ]
            ]

        margin =
            Maybe.withDefault 10 customSpacing / 2
    in
    baseStyles ++ stylesTab ++ stylesDynamic
