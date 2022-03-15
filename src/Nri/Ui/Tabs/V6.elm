module Nri.Ui.Tabs.V6 exposing
    ( view
    , Alignment(..)
    , Tab, viewTabDefault
    )

{-| Changes from V5:

  - Uses TabsInternal under the hood
  - Allows user to focus on the selected tabpanel

@docs view
@docs Alignment
@docs Tab, viewTabDefault

-}

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Nri.Ui
import Nri.Ui.Colors.Extra exposing (withAlpha)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import TabsInternal


{-| Determines whether tabs are centered or floating to the left or right.
-}
type Alignment
    = Left
    | Center
    | Right


{-| -}
type alias Tab id msg =
    { id : id
    , idString : String
    , tabView : Html msg
    , panelView : Html msg
    , spaHref : Maybe String
    }


{-| -}
view :
    { title : Maybe String
    , alignment : Alignment
    , customSpacing : Maybe Float
    , onSelect : id -> msg
    , onFocus : String -> msg
    , selected : id
    , tabs : List (Tab id msg)
    }
    -> Html msg
view config =
    let
        toInternalTab : Tab id msg -> TabsInternal.Tab id msg
        toInternalTab tab =
            { id = tab.id
            , idString = tab.idString
            , tabAttributes = []
            , tabView = [ tab.tabView ]
            , panelView = tab.panelView
            , spaHref = tab.spaHref
            }

        { tabList, tabPanels } =
            TabsInternal.views
                { onSelect = config.onSelect
                , onFocus = config.onFocus
                , selected = config.selected
                , tabs = List.map toInternalTab config.tabs
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


tabStyles : Maybe Float -> Bool -> List Style
tabStyles customSpacing isSelected =
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
            , Css.textDecoration Css.none
            , Css.property "background" "none"
            , Css.fontFamily Css.inherit
            , Css.fontSize Css.inherit
            , Css.cursor Css.pointer
            , Css.border zero
            ]

        stylesTab =
            [ Css.display Css.inlineBlock
            , Css.borderTopLeftRadius (Css.px 10)
            , Css.borderTopRightRadius (Css.px 10)
            , Css.border3 (Css.px 1) Css.solid Colors.navy
            , Css.marginTop Css.zero
            , Css.marginRight Css.zero
            , Css.marginLeft (Css.px (Maybe.withDefault 10 customSpacing))
            , Css.padding2 (Css.px 1) (Css.px 6)
            , Css.marginBottom (Css.px -1)
            , Css.cursor Css.pointer
            , Css.firstChild [ Css.marginLeft Css.zero ]
            , property "transition" "background-color 0.2s"
            , property "transition" "border-color 0.2s"
            , hover
                [ backgroundColor Colors.white
                , borderTopColor Colors.azure
                , borderRightColor Colors.azure
                , borderLeftColor Colors.azure
                ]
            ]
    in
    baseStyles ++ stylesTab ++ stylesDynamic
