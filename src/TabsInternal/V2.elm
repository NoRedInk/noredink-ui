module TabsInternal.V2 exposing
    ( Config, views
    , Tab, fromList
    )

{-|

@docs Config, views
@docs Tab, fromList

-}

import Accessibility.Styled.Aria as Aria
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
                    , [ Events.onClick (config.focusAndSelect { select = tab.id, focus = Nothing })
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
                       , Attributes.id (tabToId tab.idString)
                       , Events.on "keyup" <|
                            Json.Decode.andThen (keyEvents config tab) Events.keyCode
                       , Attributes.class "custom-focus-ring"
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


keyEvents : Config id msg -> Tab id msg -> Int -> Json.Decode.Decoder msg
keyEvents { focusAndSelect, tabs } thisTab keyCode =
    let
        findAdjacentTab tab acc =
            case acc of
                ( _, Just _ ) ->
                    acc

                ( True, Nothing ) ->
                    ( True
                    , if tab.disabled then
                        Nothing

                      else
                        Just { select = tab.id, focus = Just (tabToId tab.idString) }
                    )

                ( False, Nothing ) ->
                    ( tab.id == thisTab.id, Nothing )

        nextTab =
            List.foldl findAdjacentTab ( False, Nothing ) tabs
                |> Tuple.second

        previousTab =
            List.foldr findAdjacentTab ( False, Nothing ) tabs
                |> Tuple.second
    in
    case keyCode of
        39 ->
            -- Right
            case nextTab of
                Just next ->
                    Json.Decode.succeed (focusAndSelect next)

                Nothing ->
                    Json.Decode.fail "No next tab"

        37 ->
            -- Left
            case previousTab of
                Just previous ->
                    Json.Decode.succeed (focusAndSelect previous)

                Nothing ->
                    Json.Decode.fail "No previous tab"

        _ ->
            Json.Decode.fail "Upsupported key event"


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
         ]
            ++ (if selected then
                    [ Aria.hidden False
                    , Attributes.tabindex 0
                    ]

                else
                    [ Attributes.css [ Css.display Css.none ]
                    , Aria.hidden True
                    , Attributes.tabindex -1
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
