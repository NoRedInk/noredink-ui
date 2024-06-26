module TabsInternal.V2 exposing
    ( Config, views
    , Tab, fromList
    , LabelSource(..)
    )

{-|

@docs Config, views
@docs Tab, fromList
@docs Label

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
import Nri.Ui.Html.Attributes.V2 as AttributesExtra exposing (nriDescription, safeId, safeIdWithPrefix)
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
    , label : LabelSource
    , describedBy : List String
    }


{-| Determines what the accessible label for the tab will be.

Default behavior is to use the inner text of the tab, but we let users of the
API override this via aria-label or aria-describedby attributes. This might be
useful if you are using icon-only tabs. Otherwise, depending on how the SVG is
set up, you could end up with no accessible labels, or native OS tooltips
showing in addition to our own. See QUO-630.

-}
type LabelSource
    = FromInnerText
    | FixedLabel String
    | LabelledBy String


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
            , label = FromInnerText
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

        onFocus : Tab id msg -> msg
        onFocus tab =
            config.focusAndSelect { select = tab.id, focus = Just (safeId tab.idString) }

        tabs : List (Html msg)
        tabs =
            config.tabs
                |> FocusLoop.addEvents
                    { focus = onFocus
                    , leftRight = True
                    , upDown = False
                    }
                |> List.indexedMap (viewTab_ config)

        tabsAttribute =
            -- used to identify the actual container of the tabs. we should
            -- ideally be able to target `[role=tablist]` when needed but that
            -- won't work because the markup changes slightly in the presence of
            -- tooltips (see comment below!).
            nriDescription "Nri-Ui__tabs"
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
            , Html.div
                [ tabsAttribute
                , Attributes.css config.tabListStyles
                ]
                tabs
            ]

    else
        -- if no tooltips are present, we can rely on the DOM structure to set up the relationships correctly.
        Html.div
            [ Role.tabList
            , tabsAttribute
            , Attributes.css config.tabListStyles
            ]
            tabs


viewTab_ : Config id msg -> Int -> ( Tab id msg, List (Key.Event msg) ) -> Html msg
viewTab_ config index ( tab, keyEvents ) =
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
                         Aria.selected isSelected
                       , Role.tab
                       , Aria.controls [ tabToBodyId tab.idString ]
                       , Attributes.id (safeId tab.idString)
                       , Key.onKeyUpPreventDefault keyEvents
                       ]
                    ++ (case tab.label of
                            FromInnerText ->
                                []

                            FixedLabel label ->
                                [ Aria.label label ]

                            LabelledBy labelledById ->
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
    case tab.tabTooltip of
        [] ->
            buttonOrLink []

        tooltipAttributes ->
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
