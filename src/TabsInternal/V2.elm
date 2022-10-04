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
import Json.Decode
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.Tooltip.V3 as Tooltip
import Nri.Ui.Util exposing (dashify)


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
    Html.div
        [ Role.tabList
        , Aria.owns (List.map (tabToId << .idString) config.tabs)
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
                       , Attributes.id (tabToId tab.idString)
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

        ( Nothing, tooltipAttributes ) ->
            Tooltip.view
                { id = "tab-tooltip__" ++ tabToId tab.idString
                , trigger = \eventHandlers -> buttonOrLink eventHandlers
                }
                ([ Tooltip.smallPadding
                 , Tooltip.onBottom
                 , Tooltip.fitToContent
                 ]
                    ++ tooltipAttributes
                )


keyEvents : Config id msg -> Tab id msg -> List (Json.Decode.Decoder msg)
keyEvents { focusAndSelect, tabs } thisTab =
    let
        onFocus : Tab id msg -> msg
        onFocus tab =
            focusAndSelect { select = tab.id, focus = Just (tabToId tab.idString) }

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
         , Aria.labelledBy (tabToId tab.idString)
         , Attributes.id (tabToBodyId tab.idString)
         , Attributes.tabindex 0
         ]
            ++ (if selected then
                    [ -- Used as selector for test queries
                      Attributes.attribute "data-selected" "true"
                    ]

                else
                    [ Attributes.css [ Css.display Css.none ]
                    ]
               )
        )
        [ tab.panelView ]


tabToId : String -> String
tabToId tab =
    dashify (String.toLower tab)


tabToBodyId : String -> String
tabToBodyId tab =
    "tab-body-" ++ tabToId tab


tabToKeyedNode : String -> String
tabToKeyedNode tab =
    "tabs-internal-keyed-node-" ++ tabToId tab
