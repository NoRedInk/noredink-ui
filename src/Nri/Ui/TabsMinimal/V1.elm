module Nri.Ui.TabsMinimal.V1 exposing
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
    TabAttribute (\tab -> { tab | tabView = [ Html.text content ] })


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
                , tabStyles = tabStyles
                , tabListStyles = stylesTabsAligned
                }
    in
    Nri.Ui.styled Html.div
        "Nri-Ui-TabsMinimal__container"
        []
        []
        [ Html.styled Html.div
            [ Css.displayFlex
            , Css.alignItems Css.flexEnd
            , Css.borderBottom (Css.px 1)
            , Css.borderBottomStyle Css.solid
            , Css.borderBottomColor Colors.gray85
            , Fonts.baseFont
            ]
            []
            [ tabList ]
        , tabPanels
        ]



-- STYLES


stylesTabsAligned : List Style
stylesTabsAligned =
    [ Css.justifyContent Css.flexStart
    , Css.margin Css.zero
    , Css.fontSize (Css.px 13)
    , Css.displayFlex
    , Css.flexGrow (Css.int 1)
    , Css.padding Css.zero
    ]


tabStyles : Int -> Bool -> List Style
tabStyles _ isSelected =
    let
        stylesDynamic =
            if isSelected then
                [ Css.borderBottom (Css.px 1)
                , Css.borderBottomStyle Css.solid
                , Css.borderBottomColor Colors.gray45
                , Css.color Colors.gray20
                ]

            else
                []

        baseStyles =
            [ Css.color Colors.gray45
            , Css.position Css.relative

            -- necessary because bourbon or bootstrap or whatever add underlines when tabs are used as links
            , Css.textDecoration Css.none |> important
            , Css.property "background" "none"
            , Css.fontFamily Css.inherit
            , Css.fontSize Css.inherit
            , Css.cursor Css.pointer
            , Css.border zero
            , Css.height (Css.pct 100)
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.height (Css.px 38)
            , Css.padding2 Css.zero (Css.px 15)
            ]

        stylesTab =
            [ Css.display Css.inlineBlock
            , Css.cursor Css.pointer
            , pseudoClass "focus-visible"
                [ FocusRing.outerBoxShadow
                , Css.outline3 (Css.px 2) Css.solid Css.transparent
                ]
            ]
    in
    baseStyles ++ stylesTab ++ stylesDynamic
