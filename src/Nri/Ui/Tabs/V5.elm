module Nri.Ui.Tabs.V5 exposing
    ( Alignment(..)
    , LinkConfig
    , Tab
    , LinkTabConfig(..)
    , links
    , view
    , viewCustom
    )

{-|

@docs Alignment
@docs LinkConfig
@docs Tab
@docs LinkTabConfig
@docs links
@docs view
@docs viewCustom

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


{-| -}
type alias Tab id =
    { label : String
    , id : id
    }


{-| -}
view :
    { title : Maybe String
    , onSelect : id -> msg
    , tabs : Zipper (Tab id)
    , content : id -> Html msg
    , alignment : Alignment
    }
    -> Html msg
view config =
    viewCustom config (\tab -> Html.text tab.label)


{-| -}
viewCustom :
    { title : Maybe String
    , onSelect : id -> msg
    , tabs : Zipper (Tab id)
    , content : id -> Html msg
    , alignment : Alignment
    }
    -> (Tab id -> Html msg)
    -> Html msg
viewCustom config viewInnerTab =
    let
        selected =
            List.Zipper.current config.tabs

        viewTabs =
            List.Zipper.toList config.tabs
                |> List.map (viewTab config viewInnerTab selected)
    in
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
                viewTabs
            ]
        , Html.div
            [ Role.tabPanel
            , Aria.labelledBy (tabToId selected)
            , Widget.hidden False
            , Attributes.id (tabToBodyId selected)
            ]
            [ config.content selected.id ]
        ]


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


viewTab :
    { config
        | onSelect : id -> msg
        , tabs : Zipper (Tab id)
    }
    -> (Tab id -> Html msg)
    -> Tab id
    -> Tab id
    -> Html msg
viewTab { onSelect, tabs } viewInnerTab selected tab =
    let
        isSelected =
            selected.id == tab.id

        tabIndex =
            -- From recommendation at https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Roles/Tab_Role#Best_practices
            if isSelected then
                0

            else
                -1
    in
    Html.styled Html.button
        (stylesTabSelectable isSelected)
        [ Events.onClick (onSelect tab.id)
        , Key.onKeyDown [ Key.enter (onSelect tab.id) ]
        , Events.onFocus (onSelect tab.id)
        , Attributes.tabindex tabIndex
        , Widget.selected (selected.id == tab.id)
        , Role.tab
        , Attributes.id (tabToId tab)
        , Events.on "keyup" <|
            Json.Decode.andThen
                (\keyCode ->
                    if keyCode == 39 then
                        tabs
                            |> List.Zipper.next
                            |> Maybe.map (List.Zipper.current >> .id >> onSelect >> Json.Decode.succeed)
                            |> Maybe.withDefault (Json.Decode.fail "No next tab")

                    else if keyCode == 37 then
                        tabs
                            |> List.Zipper.previous
                            |> Maybe.map (List.Zipper.current >> .id >> onSelect >> Json.Decode.succeed)
                            |> Maybe.withDefault (Json.Decode.fail "No previous tab")

                    else
                        Json.Decode.fail "Wrong key code"
                )
                Events.keyCode
        , Attributes.css
            [ Css.color Colors.navy
            , Css.margin zero
            , Css.padding4 (Css.px 14) (Css.px 20) (Css.px 12) (Css.px 20)
            , Css.position Css.relative
            , Css.textDecoration Css.none
            , Css.property "background" "none"
            , Css.fontFamily Css.inherit
            , Css.fontSize Css.inherit
            , Css.cursor Css.pointer
            , Css.border zero
            ]
        ]
        [ viewInnerTab tab
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
        , Attributes.id (tabToId { label = tabLabel })
        ]
        [ case tabHref of
            Just href ->
                Html.styled Html.a
                    [ Css.color Colors.navy
                    , Css.display Css.inlineBlock
                    , Css.padding4 (Css.px 14) (Css.px 20) (Css.px 12) (Css.px 20)
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
                    , Css.padding4 (Css.px 14) (Css.px 20) (Css.px 12) (Css.px 20)
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


tabToId : { a | label : String } -> String
tabToId tab =
    String.replace " " "-" tab.label


tabToBodyId : { a | label : String } -> String
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
    "Nri-Ui-Tabs-V4-" ++ suffix



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
