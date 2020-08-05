module TabsInternal exposing (Config, Tab, views)

{-|

@docs Config, Tab, views

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Widget as Widget
import Css
import EventExtras
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Decode
import Nri.Ui
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1
import Nri.Ui.Html.Attributes.V2 as AttributesExtra


{-| -}
type alias Config id msg =
    { onSelect : id -> msg
    , onFocus : String -> msg
    , selected : id
    , tabs : List (Tab id msg)
    , tabToId : String -> String
    , tabToBodyId : String -> String
    , tabStyles : Bool -> List Css.Style
    , containerStyles : List Css.Style
    }


{-| -}
type alias Tab id msg =
    { id : id
    , idString : String
    , tabView : Html msg
    , panelView : Html msg
    , spaHref : Maybe String
    }


{-| -}
views : Config id msg -> { tabList : Html msg, tabPanels : List (Html msg) }
views config =
    { tabList = viewTabs config
    , tabPanels = viewTabPanels config
    }


viewTabs : Config id msg -> Html msg
viewTabs config =
    Html.div
        [ Role.tabList
        , Attributes.css config.containerStyles
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
                    ( Html.button
                    , [ Events.onClick (config.onSelect tab.id)
                      ]
                    )
    in
    Html.styled tag
        (config.tabStyles isSelected)
        (tagSpecificAttributes
            ++ [ Attributes.tabindex tabIndex
               , Widget.selected isSelected
               , Role.tab
               , Attributes.id (config.tabToId tab.idString)
               , Events.onFocus (config.onSelect tab.id)
               , Events.on "keyup" <|
                    Json.Decode.andThen (keyEvents config tab) Events.keyCode
               ]
        )
        [ tab.tabView
        ]


keyEvents : Config id msg -> Tab id msg -> Int -> Json.Decode.Decoder msg
keyEvents { onFocus, tabs, tabToId } thisTab keyCode =
    let
        findAdjacentTab tab acc =
            case acc of
                ( _, Just _ ) ->
                    acc

                ( True, Nothing ) ->
                    ( True, Just (tabToId tab.idString) )

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


viewTabPanels : Config id msg -> List (Html msg)
viewTabPanels config =
    List.map
        (\tab ->
            Html.div
                ([ Role.tabPanel
                 , Aria.labelledBy (config.tabToId tab.idString)
                 , Attributes.id (config.tabToBodyId tab.idString)
                 ]
                    ++ (if tab.id /= config.selected then
                            [ Attributes.css [ Css.display Css.none ]
                            , Widget.hidden True
                            ]

                        else
                            [ Widget.hidden False ]
                       )
                )
                [ tab.panelView ]
        )
        config.tabs
