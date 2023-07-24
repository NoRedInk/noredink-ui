module TabsInternal.V2 exposing
    ( Config, views
    , Tab, fromList
    )

{-|

@docs Config, views
@docs Tab, fromList

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
import Css
import EventExtras
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
import Nri.Ui.FocusLoop.V1 as FocusLoop
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Html.Attributes.V2 as AttributesExtra exposing (safeId, safeIdWithPrefix)
import Nri.Ui.Tooltip.V3 as Tooltip


{-| -}
type alias Config id msg =
    { focusAndSelect : { select : id, focus : Maybe String } -> msg
    , selected : id
    , tabs : List (Tab id msg)
    , tabListStyles : List Css.Style
    , tabStyles : Int -> Bool -> List Css.Style
    }


{-| -}
type alias Tab id msg =
    { id : id
    , idString : String
    , tabAttributes : List (Attribute msg)
    , tabTooltip : List (Tooltip.Attribute msg)
    , tabView : List (Html msg)
    , panelView : Html msg
    , spaHref : Maybe String
    , disabled : Bool
    , labelledBy : Maybe String
    , describedBy : List String
    }


{-| -}
fromList : { id : id, idString : String } -> List (Tab id msg -> Tab id msg) -> Tab id msg
fromList { id, idString } attributes =
    let
        defaults =
            { id = id
            , idString = idString
            , tabAttributes = []
            , tabTooltip = []
            , tabView = []
            , panelView = Html.text ""
            , spaHref = Nothing
            , disabled = False
            , labelledBy = Nothing
            , describedBy = []
            }
    in
    List.foldl (\applyAttr acc -> applyAttr acc) defaults attributes


{-| -}
views : Config id msg -> { tabList : Html msg, tabPanels : Html msg }
views config =
    { tabList = viewTabs config
    , tabPanels = viewTabPanels config
    }


viewTabs : Config id msg -> Html msg
viewTabs config =
    let
        anyTooltips =
            List.any (.tabTooltip >> List.isEmpty >> not) config.tabs
    in
    if anyTooltips then
        -- if any tooltip setup is present, we use aria-owns to associate the
        -- tabs with the tablist children, since otherwise the presence of the tooltips
        -- will mess up the relationship of the tablist and tabs.
        -- the HTML structure also changes, since aria-owns only applies after descendent relationships
        -- (See definition of [aria-owns](https://www.w3.org/TR/wai-aria-1.2/#aria-owns))
        Html.div []
            [ Html.div
                [ Role.tabList
                , Aria.owns (List.map (safeId << .idString) config.tabs)
                ]
                []
            , Html.div [ Attributes.css config.tabListStyles ]
                (List.indexedMap (viewTab_ config) config.tabs)
            ]

    else
        -- if no tooltips are present, we can rely on the DOM structure to set up the relationships correctly.
        Html.div
            [ Role.tabList
            , Attributes.css config.tabListStyles
            ]
            (List.indexedMap (viewTab_ config) config.tabs)


viewTab_ : Config id msg -> Int -> Tab id msg -> Html msg
viewTab_ config index tab =
    let
        isSelected =
            config.selected == tab.id

        tabIndex =
            -- From recommendation at https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Roles/Tab_Role#Best_practices
            if isSelected then
                0

            else
                -1

        ( tag, tagSpecificAttributes ) =
            case tab.spaHref of
                Just href ->
                    -- This is a for a SPA view
                    ( Html.a
                    , [ if isSelected then
                            Aria.currentPage

                        else
                            AttributesExtra.none
                      , Attributes.href href
                      , EventExtras.onClickPreventDefaultForLinkWithHref
                            (config.focusAndSelect { select = tab.id, focus = Nothing })
                      ]
                    )

                Nothing ->
                    -- This is for a non-SPA view
                    ( Html.button
                    , [ Attributes.type_ "button"
                      , Events.onClick (config.focusAndSelect { select = tab.id, focus = Nothing })
                      ]
                    )

        buttonOrLink tooltipAttributes =
            Html.styled tag
                (config.tabStyles index isSelected)
                (tooltipAttributes
                    ++ tagSpecificAttributes
                    ++ tab.tabAttributes
                    ++ [ Attributes.tabindex tabIndex
                       , -- check for isSelected because otherwise users won't
                         -- be able to focus on the current tab with the
                         -- keyboard.
                         Attributes.disabled (not isSelected && tab.disabled)
                       , Aria.selected isSelected
                       , Role.tab
                       , Aria.controls [ tabToBodyId tab.idString ]
                       , Attributes.id (safeId tab.idString)
                       , Key.onKeyUpPreventDefault (keyEvents config tab)
                       ]
                    ++ (case tab.labelledBy of
                            Nothing ->
                                []

                            Just labelledById ->
                                [ Aria.labelledBy labelledById ]
                       )
                    ++ (case tab.describedBy of
                            [] ->
                                []

                            ids ->
                                [ Aria.describedBy ids ]
                       )
                )
                tab.tabView
    in
    -- If the labelledByAttribute gets passed in, we're using an external
    -- tooltip, so we override any existing internal tooltip to not create
    -- accessibility problems.
    case ( tab.labelledBy, tab.tabTooltip ) of
        ( Just _, _ ) ->
            buttonOrLink []

        ( _, [] ) ->
            buttonOrLink []

        ( Nothing, tooltipAttributes ) ->
            Tooltip.view
                { id = safeIdWithPrefix "tab-tooltip" tab.idString
                , trigger = \eventHandlers -> buttonOrLink eventHandlers
                }
                ([ Tooltip.smallPadding
                 , Tooltip.onBottom
                 , Tooltip.fitToContent
                 ]
                    ++ tooltipAttributes
                )


keyEvents : Config id msg -> Tab id msg -> List (Key.Event msg)
keyEvents { focusAndSelect, tabs } thisTab =
    let
        onFocus : Tab id msg -> msg
        onFocus tab =
            focusAndSelect { select = tab.id, focus = Just (safeId tab.idString) }

        findAdjacentTab : Tab id msg -> ( Bool, Maybe msg ) -> ( Bool, Maybe msg )
        findAdjacentTab tab ( isAdjacentTab, acc ) =
            if isAdjacentTab then
                ( False, Just (onFocus tab) )

            else
                ( tab.id == thisTab.id, acc )

        activeTabs : List (Tab id msg)
        activeTabs =
            List.filter (not << .disabled) tabs

        goToNextTab : Maybe msg
        goToNextTab =
            List.foldl findAdjacentTab
                ( False
                , -- if there is no adjacent tab, default to the first tab
                  Maybe.map onFocus (List.head activeTabs)
                )
                activeTabs
                |> Tuple.second

        goToPreviousTab : Maybe msg
        goToPreviousTab =
            List.foldr findAdjacentTab
                ( False
                , -- if there is no adjacent tab, default to the last tab
                  Maybe.map onFocus (List.head (List.reverse activeTabs))
                )
                activeTabs
                |> Tuple.second
    in
    List.filterMap identity
        [ Maybe.map Key.right goToNextTab
        , Maybe.map Key.left goToPreviousTab
        ]


viewTabPanels : Config id msg -> Html msg
viewTabPanels config =
    Keyed.node "div" [] <|
        List.map
            (\tab ->
                ( tabToKeyedNode tab.idString
                , viewTabPanel tab (tab.id == config.selected)
                )
            )
            config.tabs


viewTabPanel : Tab id msg -> Bool -> Html msg
viewTabPanel tab selected =
    Html.div
        ([ Role.tabPanel
         , Aria.labelledBy (safeId tab.idString)
         , Attributes.id (tabToBodyId tab.idString)
         , Attributes.tabindex 0
         , Attributes.class FocusRing.customClass
         , Attributes.css
            [ Css.pseudoClass "focus-visible" [ FocusRing.insetBoxShadow ]
            ]
         ]
            ++ (if selected then
                    [ -- Used as selector for test queries
                      Attributes.attribute "data-selected" "true"
                    , Attributes.style "display" "block"
                    ]

                else
                    [ Attributes.style "display" "none"
                    ]
               )
        )
        [ tab.panelView ]


tabToBodyId : String -> String
tabToBodyId =
    safeIdWithPrefix "tab-body"


tabToKeyedNode : String -> String
tabToKeyedNode =
    safeIdWithPrefix "tabs-internal-keyed-node"
