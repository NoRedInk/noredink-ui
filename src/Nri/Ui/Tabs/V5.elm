module Nri.Ui.Tabs.V5 exposing
    ( Alignment(..)
    , LinkConfig
    , LinkTabConfig(..)
    , links
    , view
    , Tab
    , viewTabDefault
    )

{-|

@docs Alignment
@docs LinkConfig
@docs LinkTabConfig
@docs links
@docs view

@docs Tab


## Defaults

@docs viewTabDefault

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Widget as Widget
import Css exposing (..)
import EventExtras
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Decode
import List.Zipper exposing (Zipper)
import List.Zipper.Extra
import Nri.Ui
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1


{-| Determines whether tabs are centered or floating to the left or right.
-}
type Alignment
    = Left
    | Center
    | Right


type alias Tab id msg =
    { id : id
    , idString : String
    , tabView : Html msg
    , panelView : Html msg
    }


{-| -}
view :
    { title : Maybe String
    , alignment : Alignment
    , onSelect : id -> msg
    , selected : id
    , tabs : List (Tab id msg)
    }
    -> Html msg
view config =
    Nri.Ui.styled Html.div
        (styledName "container")
        []
        []
        [ Html.styled Html.div
            [ Css.displayFlex
            , Css.alignItems Css.flexEnd
            , Css.borderBottom (Css.px 1)
            , Css.borderBottomStyle Css.solid
            , Css.borderBottomColor Colors.navy
            , Nri.Ui.Fonts.V1.baseFont
            ]
            []
            [ config.title
                |> Maybe.map viewTitle
                |> Maybe.withDefault (Html.text "")
            , Html.styled Html.ul
                (stylesTabsAligned config.alignment)
                [ Role.tabList
                ]
                (List.map
                    (viewTab_
                        { onSelect = config.onSelect
                        , tabs = List.map .id config.tabs
                        , selected = config.selected
                        }
                    )
                    config.tabs
                )
            ]
        , Html.div []
            (List.map
                (\tab ->
                    Html.div
                        ([ Role.tabPanel
                         , Aria.labelledBy (tabToId tab.idString)
                         , Attributes.id (tabToBodyId tab.idString)
                         ]
                            ++ (if tab.id /= config.selected then
                                    [ Attributes.css [ Css.display none ]
                                    , Widget.hidden True
                                    ]

                                else
                                    [ Widget.hidden False ]
                               )
                        )
                        [ tab.panelView ]
                )
                config.tabs
            )
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
        , Css.fontSize (Css.px 30)
        , Css.fontWeight Css.bold
        , Css.margin Css.zero
        , Css.marginTop (Css.px 5)
        , Css.marginBottom (Css.px 10)
        , Css.color Colors.navy
        , Css.width (Css.px 430)
        ]
        []
        [ Html.text title ]


viewTab_ :
    { onSelect : id -> msg
    , tabs : List id
    , selected : id
    }
    -> Tab id msg
    -> Html msg
viewTab_ { onSelect, tabs, selected } tab =
    let
        isSelected =
            selected == tab.id

        tabIndex =
            -- From recommendation at https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Roles/Tab_Role#Best_practices
            -- TODO:
            -- keyboard interactions aren't behaving the way I'd expect.
            -- Right and left arrows aren't shifting the tab focus properly,
            -- and we can't tab to de-selected tabs as a result of this line.
            -- Dig into this more before publishing.
            if isSelected then
                0

            else
                -1

        findAdjacentTab id acc =
            case acc of
                ( _, Just _ ) ->
                    acc

                ( True, Nothing ) ->
                    ( True, Just id )

                ( False, Nothing ) ->
                    ( id == tab.id, Nothing )

        nextTab =
            List.foldl findAdjacentTab ( False, Nothing ) tabs
                |> Tuple.second

        previousTab =
            List.foldr findAdjacentTab ( False, Nothing ) tabs
                |> Tuple.second
    in
    Html.styled Html.button
        (stylesTabSelectable isSelected)
        [ Events.onClick (onSelect tab.id)
        , Key.onKeyDown [ Key.enter (onSelect tab.id) ]
        , Events.onFocus (onSelect tab.id)
        , Attributes.tabindex tabIndex
        , Widget.selected isSelected
        , Role.tab
        , Attributes.id (tabToId tab.idString)
        , Events.on "keyup" <|
            Json.Decode.andThen
                (\keyCode ->
                    if keyCode == 39 then
                        nextTab
                            |> Maybe.map (onSelect >> Json.Decode.succeed)
                            |> Maybe.withDefault (Json.Decode.fail "No next tab")

                    else if keyCode == 37 then
                        previousTab
                            |> Maybe.map (onSelect >> Json.Decode.succeed)
                            |> Maybe.withDefault (Json.Decode.fail "No previous tab")

                    else
                        Json.Decode.fail "Wrong key code"
                )
                Events.keyCode
        , Attributes.css
            [ Css.color Colors.navy
            , Css.margin zero
            , Css.position Css.relative
            , Css.textDecoration Css.none
            , Css.property "background" "none"
            , Css.fontFamily Css.inherit
            , Css.fontSize Css.inherit
            , Css.cursor Css.pointer
            , Css.border zero
            ]
        ]
        [ tab.tabView
        ]


{-| The types of links that we can show.

  - `NormalLink` - A link to another page.
  - `SpaLink` - A link to another SPA page. The `msg` type should be used to handle
    the navigation event.

-}
type LinkTabConfig msg
    = NormalLink
        { label : String
        , href : Maybe String
        }
    | SpaLink
        { label : String
        , href : String
        , msg : msg
        }


{-| Configure a set a tab links
-}
type alias LinkConfig msg =
    { title : Maybe String
    , tabs : Zipper (LinkTabConfig msg)
    , content : Html msg
    , alignment : Alignment
    }


{-| View a set of tab links
-}
links : LinkConfig msg -> Html msg
links config =
    Nri.Ui.styled Html.div
        (styledName "container")
        []
        []
        [ Html.styled Html.nav
            [ Css.displayFlex
            , Css.alignItems Css.flexEnd
            , Css.borderBottom (Css.px 1)
            , Css.borderBottomStyle Css.solid
            , Css.borderBottomColor Colors.navy
            , Nri.Ui.Fonts.V1.baseFont
            ]
            []
            [ config.title
                |> Maybe.map viewTitle
                |> Maybe.withDefault (Html.text "")
            , Html.styled Html.ul
                (stylesTabsAligned config.alignment)
                [ Role.tabList
                ]
                (config.tabs
                    |> mapWithCurrent viewTabLink
                    |> List.Zipper.toList
                )
            ]
        , Html.div [] [ config.content ]
        ]


viewTabLink : Bool -> LinkTabConfig msg -> Html msg
viewTabLink isSelected tabConfig =
    let
        ( tabLabel, tabHref, preventDefault ) =
            case tabConfig of
                NormalLink { label, href } ->
                    ( label, href, [] )

                SpaLink { label, href, msg } ->
                    ( label
                    , Just href
                    , [ EventExtras.onClickPreventDefaultForLinkWithHref msg ]
                    )

        currentPage =
            if isSelected then
                [ Aria.currentPage ]

            else
                []
    in
    Html.styled Html.li
        (stylesTabSelectable isSelected)
        [ Role.presentation
        , Attributes.id (tabToId tabLabel)
        ]
        [ case tabHref of
            Just href ->
                Html.styled Html.a
                    [ Css.color Colors.navy
                    , Css.display Css.inlineBlock
                    , Css.textDecoration Css.none
                    , hover
                        [ textDecoration none
                        ]
                    , focus
                        [ textDecoration none
                        ]
                    ]
                    (List.concat
                        [ [ Attributes.href href
                          , Role.tab
                          ]
                        , preventDefault
                        , currentPage
                        ]
                    )
                    [ Html.text tabLabel ]

            Nothing ->
                Html.styled Html.button
                    [ Css.color Colors.navy
                    , Css.display Css.inlineBlock
                    , Css.textDecoration Css.none
                    , Css.fontFamily Css.inherit
                    , Css.fontSize Css.inherit
                    , Css.border Css.zero
                    , Css.property "background" "none"
                    , Css.lineHeight (Css.num 1)
                    ]
                    (List.concat
                        [ [ Role.tab ]
                        , currentPage
                        ]
                    )
                    [ Html.text tabLabel ]
        ]



-- HELP


tabToId : String -> String
tabToId tab =
    String.replace " " "-" tab


tabToBodyId : String -> String
tabToBodyId tab =
    "tab-body-" ++ tabToId tab


mapWithCurrent : (Bool -> a -> b) -> Zipper a -> Zipper b
mapWithCurrent fn zipper =
    List.Zipper.Extra.from
        (List.map (fn False) (List.Zipper.before zipper))
        (fn True (List.Zipper.current zipper))
        (List.map (fn False) (List.Zipper.after zipper))


styledName : String -> String
styledName suffix =
    "Nri-Ui-Tabs__" ++ suffix



-- STYLES


stylesTabsAligned : Alignment -> List Style
stylesTabsAligned alignment =
    let
        alignmentStyles =
            case alignment of
                Left ->
                    [ Css.justifyContent Css.flexStart ]

                Center ->
                    [ Css.justifyContent Css.center ]

                Right ->
                    [ Css.justifyContent Css.flexEnd ]
    in
    stylesTabs ++ alignmentStyles


stylesTabs : List Style
stylesTabs =
    [ Css.listStyle Css.none
    , Css.margin Css.zero
    , Css.fontSize (Css.px 19)
    , Css.displayFlex
    , Css.flexGrow (Css.int 1)
    , Css.padding Css.zero
    ]


stylesTabSelectable : Bool -> List Style
stylesTabSelectable isSelected =
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
                        (Css.stop2 (Nri.Ui.Colors.Extra.withAlpha 0.25 Colors.azure) (Css.pct 0))
                        (Css.stop2 (Nri.Ui.Colors.Extra.withAlpha 0 Colors.azure) (Css.pct 25))
                        [ Css.stop2 (Nri.Ui.Colors.Extra.withAlpha 0 Colors.azure) (Css.pct 100) ]
                ]
    in
    stylesTab ++ stylesDynamic


stylesTab : List Style
stylesTab =
    [ Css.display Css.inlineBlock
    , Css.borderTopLeftRadius (Css.px 10)
    , Css.borderTopRightRadius (Css.px 10)
    , Css.border3 (Css.px 1) Css.solid Colors.navy
    , Css.marginBottom (Css.px -1)
    , Css.marginLeft (Css.px 10)
    , Css.cursor Css.pointer
    , Css.firstChild
        [ Css.marginLeft Css.zero
        ]
    , property "transition" "background-color 0.2s"
    , hover
        [ backgroundColor Colors.white
        ]
    ]
