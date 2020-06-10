module Nri.Ui.Tabs.V5 exposing
    ( Alignment(..)
    , view
    , Tab
    , viewTabDefault
    )

{-|

@docs Alignment
@docs view
@docs Tab


## Defaults

@docs viewTabDefault

-}

import Accessibility.Styled.Aria as Aria
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
import Nri.Ui.Html.Attributes.V2 as AttributesExtra


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
    , onSelect : id -> msg
    , onFocus : String -> msg
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
            , Html.styled Html.div
                (stylesTabsAligned config.alignment)
                [ Role.tabList
                ]
                (List.map
                    (viewTab_
                        { onSelect = config.onSelect
                        , onFocus = config.onFocus
                        , tabs = config.tabs
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
    , onFocus : String -> msg
    , tabs : List (Tab id msg)
    , selected : id
    }
    -> Tab id msg
    -> Html msg
viewTab_ { onSelect, onFocus, tabs, selected } tab =
    let
        isSelected =
            selected == tab.id

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
                      , EventExtras.onClickPreventDefaultForLinkWithHref (onSelect tab.id)
                      ]
                    )

                Nothing ->
                    ( Html.button
                    , [ Events.onClick (onSelect tab.id)
                      ]
                    )

        tabStyles =
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
    in
    Html.styled tag
        (stylesTabSelectable isSelected)
        (tagSpecificAttributes
            ++ [ Attributes.tabindex tabIndex
               , Widget.selected isSelected
               , Role.tab
               , Attributes.id (tabToId tab.idString)
               , Attributes.css tabStyles
               , Events.onFocus (onSelect tab.id)
               , Events.on "keyup"
                    (Json.Decode.andThen (keyEvents onFocus tabs tab) Events.keyCode)
               ]
        )
        [ tab.tabView
        ]


keyEvents : (String -> msg) -> List (Tab id msg) -> Tab id msg -> Int -> Json.Decode.Decoder msg
keyEvents onFocus tabs thisTab keyCode =
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



-- HELP


tabToId : String -> String
tabToId tab =
    String.replace " " "-" tab


tabToBodyId : String -> String
tabToBodyId tab =
    "tab-body-" ++ tabToId tab


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

        stylesTab =
            [ Css.display Css.inlineBlock
            , Css.borderTopLeftRadius (Css.px 10)
            , Css.borderTopRightRadius (Css.px 10)
            , Css.border3 (Css.px 1) Css.solid Colors.navy
            , Css.marginBottom (Css.px -1)
            , Css.marginLeft (Css.px 10)
            , Css.cursor Css.pointer
            , Css.firstChild [ Css.marginLeft Css.zero ]
            , property "transition" "background-color 0.2s"
            , hover [ backgroundColor Colors.white ]
            ]
    in
    stylesTab ++ stylesDynamic
