module Nri.Ui.MinimalTabs.V1 exposing
    ( view
    , Tab, TabAttribute, build
    , tabString, tabHtml
    , panelHtml
    , spaHref
    )

{-| This forks Nri.Ui.Tabs.V8

@docs view


### Tabs

@docs Tab, TabAttribute, build
@docs tabString, tabHtml
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


{-| -}
view :
    { focusAndSelect : { select : id, focus : Maybe String } -> msg
    , selected : id
    }
    -> List (Tab id msg)
    -> Html msg
view { focusAndSelect, selected } tabs =
    let
        { tabList, tabPanels } =
            TabsInternal.views
                { focusAndSelect = focusAndSelect
                , selected = selected
                , tabs = List.map (\(Tab t) -> t) tabs
                , tabStyles =
                    tabStyles
                , tabListStyles = stylesTabsAligned
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
            [ tabList ]
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



-- STYLES


stylesTabsAligned : List Style
stylesTabsAligned =
    [ Css.justifyContent Css.flexStart
    , Css.margin Css.zero
    , Css.fontSize (Css.px 19)
    , Css.displayFlex
    , Css.flexGrow (Css.int 1)
    , Css.padding Css.zero
    ]


tabStyles : Int -> Bool -> List Style
tabStyles index isSelected =
    let
        stylesDynamic =
            if isSelected then
                [ Css.borderBottom (Css.px 1)
                , Css.borderBottomStyle Css.solid
                , Css.backgroundColor Colors.white
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
            10 / 2
    in
    baseStyles ++ stylesTab ++ stylesDynamic
