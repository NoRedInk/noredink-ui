module Nri.Ui.Tabs.V8 exposing
    ( Attribute, title, alignment, spacing, view
    , Alignment(..)
    , Tab, TabAttribute, build
    , tabString, tabHtml, withTooltip, disabled, labelledBy, describedBy
    , panelHtml
    , spaHref
    )

{-| Changes from V7:

  - Uses an HTML-like API
  - Adds sticky positioning

@docs Attribute, title, alignment, spacing, view
@docs Alignment
@docs Tab, TabAttribute, build
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
type TabAttribute id msg
    = Attribute (TabsInternal.Tab id msg -> TabsInternal.Tab id msg)


{-| -}
tabString : String -> TabAttribute id msg
tabString content =
    Attribute (\tab -> { tab | tabView = [ viewTabDefault content ] })


{-| -}
tabHtml : Html Never -> TabAttribute id msg
tabHtml content =
    Attribute (\tab -> { tab | tabView = [ Html.map never content ] })


{-| Tooltip defaults: `[Tooltip.smallPadding, Tooltip.onBottom, Tooltip.fitToContent]`
-}
withTooltip : List (Tooltip.Attribute msg) -> TabAttribute id msg
withTooltip attributes =
    Attribute (\tab -> { tab | tabTooltip = attributes })


{-| Makes it so that the tab can't be clicked or focused via keyboard navigation
-}
disabled : Bool -> TabAttribute id msg
disabled isDisabled =
    Attribute (\tab -> { tab | disabled = isDisabled })


{-| Sets an overriding labelledBy on the tab for an external tooltip.
This assumes an external tooltip is set and disables any internal tooltip configured.
-}
labelledBy : String -> TabAttribute id msg
labelledBy labelledById =
    Attribute (\tab -> { tab | labelledBy = Just labelledById })


{-| Like [`labelledBy`](#labelledBy), but it describes the given element
instead of labeling it.

This attribute can be used multiple times if more than one element describes
this tab.

-}
describedBy : String -> TabAttribute id msg
describedBy describedById =
    Attribute (\tab -> { tab | describedBy = describedById :: tab.describedBy })


{-| -}
panelHtml : Html msg -> TabAttribute id msg
panelHtml content =
    Attribute (\tab -> { tab | panelView = content })


{-| -}
spaHref : String -> TabAttribute id msg
spaHref url =
    Attribute (\tab -> { tab | spaHref = Just url })


{-| -}
tabAttributes : List (Html.Attribute msg) -> TabAttribute id msg
tabAttributes attrs =
    Attribute (\tab -> { tab | tabAttributes = tab.tabAttributes ++ attrs })


{-| -}
build : { id : id, idString : String } -> List (TabAttribute id msg) -> Tab id msg
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


type Attribute id msg
    = Title String
    | Alignment Alignment
    | Spacing Float


title : String -> Attribute id msg
title =
    Title


alignment : Alignment -> Attribute id msg
alignment =
    Alignment


spacing : Float -> Attribute id msg
spacing =
    Spacing


type alias Config =
    { title : Maybe String
    , alignment : Alignment
    , spacing : Maybe Float
    }


defaultConfig : Config
defaultConfig =
    { title = Nothing
    , alignment = Left
    , spacing = Nothing
    }


updateConfig : Attribute id msg -> Config -> Config
updateConfig attr config =
    case attr of
        Title newTitle ->
            { config | title = Just newTitle }

        Alignment newAlignment ->
            { config | alignment = newAlignment }

        Spacing newSpacing ->
            { config | spacing = Just newSpacing }


{-| -}
view :
    { focusAndSelect : { select : id, focus : Maybe String } -> msg
    , selected : id
    }
    -> List (Attribute id msg)
    -> List (Tab id msg)
    -> Html msg
view { focusAndSelect, selected } attrs tabs =
    let
        config =
            List.foldl updateConfig defaultConfig attrs

        { tabList, tabPanels } =
            TabsInternal.views
                { focusAndSelect = focusAndSelect
                , selected = selected
                , tabs = List.map (\(Tab t) -> t) tabs
                , tabStyles = tabStyles config.spacing
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
viewTabDefault tabTitle =
    Html.div
        [ Attributes.css
            [ Css.padding4 (Css.px 14) (Css.px 20) (Css.px 12) (Css.px 20)
            ]
        ]
        [ Html.text tabTitle ]


viewTitle : String -> Html msg
viewTitle tabTitle =
    Html.styled Html.h1
        [ Css.flexGrow (Css.int 2)
        , Css.fontSize (Css.px 20)
        , Css.fontWeight Css.bold
        , Css.margin4 (Css.px 5) (Css.px 10) (Css.px 10) Css.zero
        , Css.color Colors.navy
        ]
        []
        [ Html.text tabTitle ]



-- STYLES


stylesTabsAligned : Alignment -> List Style
stylesTabsAligned tabAlignment =
    let
        alignmentStyles =
            case tabAlignment of
                Left ->
                    Css.justifyContent Css.flexStart

                Center ->
                    Css.justifyContent Css.center

                Right ->
                    Css.justifyContent Css.flexEnd
    in
    [ alignmentStyles
    , Css.margin Css.zero
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
