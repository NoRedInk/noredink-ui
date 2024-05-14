module Nri.Ui.Tabs.V8 exposing
    ( Attribute, title, spacing
    , Alignment(..), alignment
    , pageBackgroundColor
    , tabListSticky, TabListStickyConfig, tabListStickyCustom
    , view
    , Tab, TabAttribute, build
    , tabString, tabHtml, withTooltip, label, labelledBy, describedBy
    , panelHtml
    , spaHref
    )

{-| Changes from V7:

  - Uses an HTML-like API
  - Adds sticky positioning
  - Adds background color in the tab list (for use with sticky positioning)
  - Adds the ability to make the background of the active tab fade into the background of the panel below it
  - Adds an `data-nri-description='Nri-Ui__tabs'` attribute to the tabs container
  - Adds the ability to explicitly set the label of the tab (rather than using its inner text)


### Attributes

@docs Attribute, title, spacing
@docs Alignment, alignment
@docs pageBackgroundColor
@docs tabListSticky, TabListStickyConfig, tabListStickyCustom
@docs view


### Tabs

@docs Tab, TabAttribute, build
@docs tabString, tabHtml, withTooltip, label, labelledBy, describedBy
@docs panelHtml
@docs spaHref

-}

import Css exposing (..)
import Css.Global
import Css.Media
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Nri.Ui
import Nri.Ui.Colors.Extra exposing (withAlpha)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Tooltip.V3 as Tooltip
import TabsInternal.V2 as TabsInternal


{-| -}
type Tab id msg
    = Tab (TabsInternal.Tab id msg)


{-| -}
type TabAttribute id msg
    = TabAttribute (TabsInternal.Tab id msg -> TabsInternal.Tab id msg)


{-| -}
tabString : String -> TabAttribute id msg
tabString content =
    TabAttribute (\tab -> { tab | tabView = [ viewTabDefault content ] })


{-| -}
tabHtml : Html Never -> TabAttribute id msg
tabHtml content =
    TabAttribute (\tab -> { tab | tabView = [ Html.map never content ] })


{-| Tooltip defaults: `[Tooltip.smallPadding, Tooltip.onBottom, Tooltip.fitToContent]`
-}
withTooltip : List (Tooltip.Attribute msg) -> TabAttribute id msg
withTooltip attributes =
    TabAttribute (\tab -> { tab | tabTooltip = attributes })


{-| Sets an overriding label attribute on the tab for an external tooltip.
This assumes an external tooltip is set and disables any internal tooltip configured.
-}
label : String -> TabAttribute id msg
label label_ =
    TabAttribute (\tab -> { tab | label = TabsInternal.FixedLabel label_ })


{-| Sets an overriding labelled-by attribute on the tab for an external tooltip.
This assumes an external tooltip is set and disables any internal tooltip configured.
-}
labelledBy : String -> TabAttribute id msg
labelledBy labelledById =
    TabAttribute (\tab -> { tab | label = TabsInternal.LabelledBy labelledById })


{-| Like [`labelledBy`](#labelledBy), but it describes the given element
instead of labeling it.

This attribute can be used multiple times if more than one element describes
this tab.

-}
describedBy : String -> TabAttribute id msg
describedBy describedById =
    TabAttribute (\tab -> { tab | describedBy = describedById :: tab.describedBy })


{-| -}
panelHtml : Html msg -> TabAttribute id msg
panelHtml content =
    TabAttribute (\tab -> { tab | panelView = content })


{-| -}
spaHref : String -> TabAttribute id msg
spaHref url =
    TabAttribute (\tab -> { tab | spaHref = Just url })


{-| -}
tabAttributes : List (Html.Attribute msg) -> TabAttribute id msg
tabAttributes attrs =
    TabAttribute (\tab -> { tab | tabAttributes = tab.tabAttributes ++ attrs })


{-| -}
build : { id : id, idString : String } -> List (TabAttribute id msg) -> Tab id msg
build config attributes =
    Tab
        (TabsInternal.fromList config
            (List.map (\(TabAttribute f) -> f)
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


{-| Ways to adapt the appearance of the tabs to your application.
-}
type Attribute id msg
    = Attribute (Config -> Config)


{-| Set a title in the tab list.
-}
title : String -> Attribute id msg
title title_ =
    Attribute (\config -> { config | title = Just title_ })


{-| Set the alignment of the tab list.
-}
alignment : Alignment -> Attribute id msg
alignment alignment_ =
    Attribute (\config -> { config | alignment = alignment_ })


{-| Set the spacing between tabs in the tab list.
-}
spacing : Float -> Attribute id msg
spacing spacing_ =
    Attribute (\config -> { config | spacing = Just spacing_ })


{-| Tell this tab list about the background color of the page it lievs on.

You may want to use this if, for example:

  - you are setting up sticky headers, to prevent page content from showing
    through the background.

  - you are using tabs in a page that has non-white background, so the
    background of the active tab fades into the panel below it.

-}
pageBackgroundColor : Css.Color -> Attribute id msg
pageBackgroundColor color =
    Attribute (\config -> { config | pageBackgroundColor = Just color })


{-| Make the tab list sticky. You probably want to set an explicit background
color along with this!
-}
tabListSticky : Attribute id msg
tabListSticky =
    Attribute (\config -> { config | tabListStickyConfig = Just defaultTabListStickyConfig })


{-| Make the tab list sticky, overriding the default behavior. You should
probably set an explicit background color along with this.
-}
tabListStickyCustom : TabListStickyConfig -> Attribute id msg
tabListStickyCustom custom =
    Attribute (\config -> { config | tabListStickyConfig = Just custom })


type alias Config =
    { title : Maybe String
    , alignment : Alignment
    , spacing : Maybe Float
    , pageBackgroundColor : Maybe Css.Color
    , tabListStickyConfig : Maybe TabListStickyConfig
    }


defaultConfig : Config
defaultConfig =
    { title = Nothing
    , alignment = Left
    , spacing = Nothing
    , pageBackgroundColor = Nothing
    , tabListStickyConfig = Nothing
    }


{-| Configure how the top bar is sticky.

  - `topOffset` controls how far from the top of the viewport the bar will
    stick, in pixels. Content will be visible below this offset in the z-order.
    (**Default value:** 0)
  - `topPadding` controls how far from the top of the viewport the bar will
    be padded. Unlike `topOffset`, content will _not_ be visible behind the
    padding. Be aware that this padding will add space in the DOM even when the
    bar is not sticky. (**Default value:** 0)
  - `zIndex` controls how high up the z-order the bar will float. (**Default
    value:** 0)

-}
type alias TabListStickyConfig =
    { topOffset : Float
    , topPadding : Float
    , zIndex : Int
    }


defaultTabListStickyConfig : TabListStickyConfig
defaultTabListStickyConfig =
    { topOffset = 0
    , topPadding = 0
    , zIndex = 0
    }


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
            List.foldl (\(Attribute fn) soFar -> fn soFar) defaultConfig attrs

        { tabList, tabPanels } =
            TabsInternal.views
                { focusAndSelect = focusAndSelect
                , selected = selected
                , tabs = List.map (\(Tab t) -> t) tabs
                , tabStyles =
                    tabStyles
                        config.spacing
                        (Maybe.withDefault Colors.white config.pageBackgroundColor)
                , tabListStyles = stylesTabsAligned config
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
            , Css.Media.withMedia
                [ MediaQuery.narrowMobile ]
                [ Css.backgroundColor Colors.gray96
                , Css.padding (Css.px 20)
                , Css.borderRadius (Css.px 8)
                , Css.borderBottom Css.zero
                , Css.flexDirection Css.column
                , Css.alignItems flexStart
                , Css.Global.children [ Css.Global.div [ Css.width (Css.pct 100) ] ]
                ]
            , maybeStyle
                (\{ topOffset, topPadding, zIndex } ->
                    Css.Media.withMedia
                        [ MediaQuery.notMobile ]
                        [ Css.position Css.sticky
                        , Css.top (Css.px topOffset)
                        , Css.paddingTop (Css.px topPadding)
                        , Css.zIndex (Css.int zIndex)
                        ]
                )
                config.tabListStickyConfig
            , maybeStyle Css.backgroundColor config.pageBackgroundColor
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


stylesTabsAligned : Config -> List Style
stylesTabsAligned config =
    let
        alignmentStyles =
            case config.alignment of
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
    , Css.Media.withMedia
        [ MediaQuery.narrowMobile ]
        [ Css.flexDirection Css.column
        ]
    ]


maybeStyle : (a -> Style) -> Maybe a -> Style
maybeStyle styler maybeValue =
    case maybeValue of
        Just value ->
            styler value

        Nothing ->
            Css.batch []


tabStyles : Maybe Float -> Css.Color -> Int -> Bool -> List Style
tabStyles customSpacing pageBackgroundColor_ index isSelected =
    let
        stylesDynamic =
            if isSelected then
                [ Css.borderBottom (Css.px 1)
                , Css.borderBottomStyle Css.solid
                , Css.backgroundColor Colors.white
                , Css.borderBottomColor pageBackgroundColor_
                , Css.backgroundImage <|
                    Css.linearGradient2 Css.toTop
                        (Css.stop2 (withAlpha 1 pageBackgroundColor_) (Css.pct 0))
                        (Css.stop2 (withAlpha 0 pageBackgroundColor_) (Css.pct 100))
                        []
                , Css.Media.withMedia
                    [ MediaQuery.narrowMobile ]
                    [ Css.border Css.zero
                    , Css.backgroundImage Css.none
                    , Css.backgroundColor Colors.glacier
                    , Css.borderRadius (Css.px 8)
                    , Css.fontSize (Css.px 15)
                    , Css.fontWeight (Css.int 700)
                    , Css.width (Css.pct 100)
                    ]
                ]

            else
                [ Css.backgroundColor Colors.frost
                , Css.backgroundImage <|
                    Css.linearGradient2 Css.toTop
                        (Css.stop2 (withAlpha 0.25 Colors.azure) (Css.pct 0))
                        (Css.stop2 (withAlpha 0 Colors.azure) (Css.pct 25))
                        [ Css.stop2 (withAlpha 0 Colors.azure) (Css.pct 100) ]
                , Css.Media.withMedia
                    [ MediaQuery.narrowMobile ]
                    [ Css.backgroundImage Css.none
                    , Css.backgroundColor Colors.gray96
                    , Css.border Css.zero
                    , Css.borderRadius (Css.px 8)
                    , Css.fontSize (Css.px 15)
                    , Css.width (Css.pct 100)
                    , Css.fontWeight (Css.int 600)
                    ]
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
            , Css.Media.withMedia
                [ MediaQuery.narrowMobile ]
                [ Css.marginLeft Css.zero
                , Css.marginRight Css.zero
                , Css.textAlign Css.left
                , Css.padding Css.zero
                ]
            , hover
                [ backgroundColor Colors.white
                , borderTopColor Colors.azure
                , borderRightColor Colors.azure
                , borderLeftColor Colors.azure
                , Css.Media.withMedia
                    [ MediaQuery.narrowMobile ]
                    [ backgroundColor Colors.frost
                    , borderTopColor Css.unset
                    , borderRightColor Css.unset
                    , borderLeftColor Css.unset
                    ]
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
