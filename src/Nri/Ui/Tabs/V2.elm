module Nri.Ui.Tabs.V2 exposing
    ( Alignment(..)
    , Config
    , LinkConfig
    , Tab
    , TabLink
    , links
    , view
    , viewCustom
    , viewTabDefault
    )

{-|

@docs Alignment
@docs Config
@docs LinkConfig
@docs Tab
@docs TabLink
@docs links
@docs view
@docs viewCustom


## Defaults

@docs viewTabDefault

-}

import Accessibility.Aria
import Accessibility.Key
import Accessibility.Role
import Accessibility.Widget
import Css exposing (Style)
import Css.Foreign exposing (Snippet, children, descendants, everything, selector)
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Decode
import List.Zipper exposing (Zipper(..))
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1
import Nri.Ui.Fonts.V1


{-| This is a better choice for a no-op than "#" because "#" changes your
location bar. See <http://stackoverflow.com/a/20676911> for more details.
-}
noOpHref : Attribute a
noOpHref =
    Attributes.href "javascript:void(0)"


{-| -}
type alias Config id msg =
    { title : Maybe String
    , onSelect : id -> msg
    , tabs : Zipper (Tab id)
    , content : id -> Html msg
    , alignment : Alignment
    }


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
view : Config id msg -> Html msg
view config =
    viewCustom config viewTabDefault


{-| -}
viewTabDefault : Tab id -> Html msg
viewTabDefault tab =
    Html.text tab.label


{-| -}
viewCustom : Config id msg -> (Tab id -> Html msg) -> Html msg
viewCustom config viewInnerTab =
    let
        selected =
            List.Zipper.current config.tabs

        viewTabs =
            List.Zipper.toList config.tabs
                |> List.map (viewTab config viewInnerTab selected)
    in
    Html.div
        []
        [ Html.styled Html.div
            [ Css.displayFlex
            , Css.alignItems Css.flexEnd
            , Css.borderBottom (Css.px 1)
            , Css.borderBottomStyle Css.solid
            , Css.borderBottomColor Nri.Ui.Colors.V1.navy
            , Nri.Ui.Fonts.V1.baseFont
            ]
            []
            [ config.title
                |> Maybe.map viewTitle
                |> Maybe.withDefault (Html.text "")
            , Html.styled Html.ul
                (stylesTabsAligned config.alignment)
                [ Attributes.fromUnstyled <| Accessibility.Role.tabList
                ]
                viewTabs
            ]
        , Html.div
            [ Attributes.fromUnstyled <| Accessibility.Role.tabPanel
            , Attributes.fromUnstyled <| Accessibility.Aria.labelledBy (tabToId selected)
            , Attributes.fromUnstyled <| Accessibility.Widget.hidden False
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
        , Css.color Nri.Ui.Colors.V1.navy
        , Css.width (Css.px 430)
        ]
        []
        [ Html.text title ]


viewTab : Config id msg -> (Tab id -> Html msg) -> Tab id -> Tab id -> Html msg
viewTab { onSelect, tabs } viewInnerTab selected tab =
    let
        isSelected =
            selected.id == tab.id
    in
    Html.styled Html.li
        (stylesTabSelectable isSelected)
        [ Events.onClick (onSelect tab.id)
        , Attributes.fromUnstyled <| Accessibility.Key.onKeyDown [ Accessibility.Key.enter (onSelect tab.id) ]
        , Events.onFocus (onSelect tab.id)
        , Attributes.tabindex 0
        , Attributes.fromUnstyled <| Accessibility.Role.presentation
        , Attributes.id (tabToId tab)
        , Attributes.fromUnstyled <| Accessibility.Aria.controls (tabToBodyId tab)
        , Attributes.fromUnstyled <| Accessibility.Widget.selected (selected.id == tab.id)
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
        ]
        [ Html.styled Html.a
            [ Css.color Nri.Ui.Colors.V1.navy
            , Css.hover [ Css.textDecoration Css.none ]
            , Css.focus [ Css.textDecoration Css.none ]
            , Css.display Css.inlineBlock
            , Css.padding4 (Css.px 14) (Css.px 20) (Css.px 12) (Css.px 20)
            , Css.position Css.relative
            , Css.textDecoration Css.none
            ]
            [ Attributes.fromUnstyled <| Accessibility.Role.tab
            , noOpHref
            , Attributes.tabindex -1
            ]
            [ viewInnerTab tab ]
        ]


{-| Describe a tab that is meant to link to another page
-}
type alias TabLink =
    { label : String
    , href : Maybe String
    }


{-| Configure a set a tab links
-}
type alias LinkConfig msg =
    { title : Maybe String
    , tabs : Zipper TabLink
    , content : Html msg
    , alignment : Alignment
    }


{-| View a set of tab links
-}
links : LinkConfig msg -> Html msg
links config =
    Html.div []
        [ Html.styled Html.nav
            [ Css.displayFlex
            , Css.alignItems Css.flexEnd
            , Css.borderBottom (Css.px 1)
            , Css.borderBottomStyle Css.solid
            , Css.borderBottomColor Nri.Ui.Colors.V1.navy
            , Nri.Ui.Fonts.V1.baseFont
            ]
            []
            [ config.title
                |> Maybe.map viewTitle
                |> Maybe.withDefault (Html.text "")
            , Html.styled Html.ul
                (stylesTabsAligned config.alignment)
                []
                (config.tabs
                    |> mapWithCurrent (viewTabLink config)
                    |> List.Zipper.toList
                )
            ]
        , Html.div [] [ config.content ]
        ]


viewTabLink : LinkConfig msg -> Bool -> TabLink -> Html msg
viewTabLink config isSelected tabLink =
    Html.styled Html.li
        (stylesTabSelectable isSelected)
        [ Attributes.fromUnstyled <| Accessibility.Role.presentation
        , Attributes.id (tabToId tabLink)
        ]
        [ Html.styled Html.a
            [ Css.color Nri.Ui.Colors.V1.navy
            , Css.display Css.inlineBlock
            , Css.padding4 (Css.px 14) (Css.px 20) (Css.px 12) (Css.px 20)
            , Css.textDecoration Css.none
            ]
            [ tabLink.href
                |> Maybe.map Attributes.href
                |> Maybe.withDefault noOpHref
            ]
            [ Html.text tabLink.label ]
        ]



-- HELP


tabToId : { a | label : String } -> String
tabToId tab =
    tab.label


tabToBodyId : { a | label : String } -> String
tabToBodyId tab =
    "tab-body-" ++ tab.label


mapWithCurrent : (Bool -> a -> b) -> Zipper a -> Zipper b
mapWithCurrent fn (Zipper before current after) =
    Zipper
        (List.map (fn False) before)
        (fn True current)
        (List.map (fn False) after)



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
    , Css.marginRight (Css.px 10)
    ]


stylesTabSelectable : Bool -> List Style
stylesTabSelectable isSelected =
    let
        stylesDynamic =
            if isSelected then
                [ Css.backgroundColor Nri.Ui.Colors.V1.white
                , Css.borderBottom (Css.px 1)
                , Css.borderBottomStyle Css.solid
                , Css.borderBottomColor Nri.Ui.Colors.V1.white
                ]

            else
                [ Css.backgroundColor Nri.Ui.Colors.V1.frost
                , Css.backgroundImage <|
                    Css.linearGradient2 Css.toTop
                        (Css.stop2 (Nri.Ui.Colors.Extra.withAlpha 0.25 Nri.Ui.Colors.V1.azure) (Css.pct 0))
                        (Css.stop2 (Nri.Ui.Colors.Extra.withAlpha 0 Nri.Ui.Colors.V1.azure) (Css.pct 25))
                        [ Css.stop2 (Nri.Ui.Colors.Extra.withAlpha 0 Nri.Ui.Colors.V1.azure) (Css.pct 100) ]
                ]
    in
    stylesTab ++ stylesDynamic


stylesTab : List Style
stylesTab =
    [ Css.display Css.inlineBlock
    , Css.borderTopLeftRadius (Css.px 10)
    , Css.borderTopRightRadius (Css.px 10)
    , Css.border3 (Css.px 1) Css.solid Nri.Ui.Colors.V1.navy
    , Css.marginBottom (Css.px -1)
    , Css.marginLeft (Css.px 10)
    , Css.cursor Css.pointer
    , Css.firstChild
        [ Css.marginLeft Css.zero
        ]
    ]
