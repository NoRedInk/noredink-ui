module Nri.Ui.Pagination.V1 exposing (view)

{-| Display a nav element aiding in navigating between pages of content.

    [ Previous ] [ 1 ] [ 2 ] [ 3 ] [ 4 ] [ Next ]

@docs view

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Aria as Aria
import Css
import Css.Transitions as Transitions
import Html.Styled.Attributes exposing (css)
import List.Extra
import Nri.Ui.ClickableText.V5 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.V3 exposing (viewIf)


{-| Pass in a list of page URLs/on-click handlers and the currently-selected page's index.

    view
        [ { onClick = GoToPage 0, href = "#page-1" }
        , { onClick = GoToPage 1, href = "#page-2" }
        , { onClick = GoToPage 2, href = "#page-3" }
        ]
        2

Note: the navigation will only show if there's 2 or more pages of content.

-}
view : List { onClick : msg, href : String } -> Int -> Html msg
view pages currentPageIndex =
    viewIf (\_ -> view_ pages currentPageIndex)
        (List.length pages > 1)


view_ : List { onClick : msg, href : String } -> Int -> Html msg
view_ pages currentPageIndex =
    Html.nav
        [ css
            [ Css.width (Css.pct 100)
            , Css.displayFlex
            , Css.justifyContent Css.center
            , Css.alignItems Css.center
            ]
        , Aria.label "pagination"
        ]
        [ previousPageLink (List.Extra.getAt (currentPageIndex - 1) pages)
        , pages
            |> List.indexedMap
                (\index page ->
                    Html.li [] [ directPageLink currentPageIndex index page ]
                )
            |> Html.ol
                [ css
                    [ Css.displayFlex
                    , Css.flexWrap Css.wrap
                    , Css.justifyContent Css.center
                    , Css.listStyleType Css.none
                    , Css.margin Css.zero
                    , Css.padding Css.zero
                    ]
                ]
        , nextPageLink (List.Extra.getAt (currentPageIndex + 1) pages)
        ]


previousPageLink : Maybe { onClick : msg, href : String } -> Html msg
previousPageLink maybePreviousPage =
    ClickableText.link "Previous\u{00A0}page" <|
        [ ClickableText.small
        , ClickableText.css [ Css.marginRight (Css.px 10) ]
        ]
            ++ linkAttributes maybePreviousPage


nextPageLink : Maybe { onClick : msg, href : String } -> Html msg
nextPageLink maybeNextPage =
    ClickableText.link "Next\u{00A0}page" <|
        [ ClickableText.small
        , ClickableText.css [ Css.marginLeft (Css.px 10) ]
        ]
            ++ linkAttributes maybeNextPage


directPageLink : Int -> Int -> { onClick : msg, href : String } -> Html msg
directPageLink currentPageIndex pageIndex page =
    let
        humanPage =
            String.fromInt (pageIndex + 1)
    in
    ClickableText.link humanPage <|
        [ ClickableText.small
        , List.filterMap identity
            [ Just (Aria.label ("Page " ++ humanPage))
            , if pageIndex == currentPageIndex then
                Just Aria.currentPage

              else
                Nothing
            ]
            |> ClickableText.custom
        , ClickableText.css
            [ Css.padding2 (Css.px 7) (Css.px 13.5)
            , Css.minWidth (Css.px 36)
            , Css.minHeight (Css.px 33)
            , Css.textAlign Css.center
            , Css.margin (Css.px 7)
            , Css.borderRadius (Css.px 8)
            , Transitions.transition
                [ Transitions.backgroundColor 300
                , Transitions.color 300
                ]
            , if pageIndex == currentPageIndex then
                Css.batch
                    [ Css.backgroundColor Colors.glacier
                    , Css.border3 (Css.px 1) Css.solid Colors.glacier |> Css.important
                    , Css.color Colors.navy
                    ]

              else
                Css.batch []
            ]
        ]
            ++ linkAttributes (Just page)


linkAttributes : Maybe { onClick : msg, href : String } -> List (ClickableText.Attribute msg)
linkAttributes maybePage =
    case maybePage of
        Just { onClick, href } ->
            [ ClickableText.onClick onClick
            , ClickableText.linkSpa href
            ]

        Nothing ->
            [ ClickableText.disabled True ]
