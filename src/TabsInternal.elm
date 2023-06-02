module TabsInternal exposing (Config, Tab, views)

{-|

@docs Config, Tab, views

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


{-| -}
type alias Config id msg =
    { onSelect : id -> msg
    , onFocus : String -> msg
    , selected : id
    , tabs : List (Tab id msg)
    , tabListStyles : List Css.Style
    , tabStyles : Bool -> List Css.Style
    }


{-| -}
type alias Tab id msg =
    { id : id
    , idString : String
    , tabAttributes : List (Attribute msg)
    , tabView : List (Html msg)
    , panelView : Html msg
    , spaHref : Maybe String
    }


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
        , Aria.owns (List.map (AttributesExtra.safeId << .idString) config.tabs)
        , Attributes.css config.tabListStyles
        ]
        (List.map (viewTab_ config) config.tabs)


viewTab_ : Config id msg -> Tab id msg -> Html msg
viewTab_ config tab =
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
                      , EventExtras.onClickPreventDefaultForLinkWithHref (config.onSelect tab.id)
                      ]
                    )

                Nothing ->
                    -- This is for a non-SPA view
                    ( Html.button
                    , [ Events.onClick (config.onSelect tab.id)
                      ]
                    )
    in
    Html.styled tag
        (config.tabStyles isSelected)
        (tagSpecificAttributes
            ++ tab.tabAttributes
            ++ [ Attributes.tabindex tabIndex
               , Aria.selected isSelected
               , Role.tab
               , Aria.controls [ tabToBodyId tab.idString ]
               , Attributes.id (AttributesExtra.safeId tab.idString)
               , Events.onFocus (config.onSelect tab.id)
               , Events.on "keyup" <|
                    Json.Decode.andThen (keyEvents config tab) Events.keyCode
               ]
        )
        tab.tabView


keyEvents : Config id msg -> Tab id msg -> Int -> Json.Decode.Decoder msg
keyEvents { onFocus, tabs } thisTab keyCode =
    let
        findAdjacentTab tab acc =
            case acc of
                ( _, Just _ ) ->
                    acc

                ( True, Nothing ) ->
                    ( True, Just (AttributesExtra.safeId tab.idString) )

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
                    Json.Decode.succeed (onFocus next)

                Nothing ->
                    Json.Decode.fail "No next tab"

        37 ->
            -- Left
            case previousTab of
                Just previous ->
                    Json.Decode.succeed (onFocus previous)

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
         , Aria.labelledBy (AttributesExtra.safeId tab.idString)
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


tabToBodyId : String -> String
tabToBodyId =
    AttributesExtra.safeIdWithPrefix "tab-body"


tabToKeyedNode : String -> String
tabToKeyedNode =
    AttributesExtra.safeIdWithPrefix "tabs-internal-keyed-node"
