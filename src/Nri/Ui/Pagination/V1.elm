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
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.V3 exposing (viewIf)


{-|

  - goToPage : a msg to handle going to a page based on the index
  - currentPageIndex : the currently-selected page's index
  - a list of page URLs

```
view GoToPage 2 [ "#page-1", "#page-2", "#page-3" ]
```

Note: the navigation will only show if there's 2 or more pages of content.

-}
view : (Int -> msg) -> Int -> List String -> Html msg
view goToPage currentPageIndex pages =
    viewIf (\_ -> view_ goToPage currentPageIndex pages)
        (List.length pages > 1)


view_ : (Int -> msg) -> Int -> List String -> Html msg
view_ goToPage currentPageIndex pages =
    let
        lastPageIndex =
            List.length pages - 1
    in
    Html.nav
        [ css
            [ Css.width (Css.pct 100)
            , Css.displayFlex
            , Css.justifyContent Css.center
            , Css.alignItems Css.center
            ]
        , Aria.label "pagination"
        ]
        [ previousPageLink goToPage
            currentPageIndex
            (List.Extra.getAt (currentPageIndex - 1) pages)
        , pages
            |> List.indexedMap
                (\page url ->
                    Html.li [] [ directPageLink goToPage currentPageIndex page url ]
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
        , nextPageLink goToPage
            currentPageIndex
            (List.Extra.getAt (currentPageIndex + 1) pages)
        ]


previousPageLink : (Int -> msg) -> Int -> Maybe String -> Html msg
previousPageLink goToPage currentPageIndex maybeUrl =
    ClickableText.link "Previous\u{00A0}Page" <|
        [ ClickableText.small
        , ClickableText.css [ Css.marginRight (Css.px 10) ]
        ]
            ++ linkAttributes (goToPage (currentPageIndex - 1)) maybeUrl


nextPageLink : (Int -> msg) -> Int -> Maybe String -> Html msg
nextPageLink goToPage currentPageIndex maybeUrl =
    ClickableText.link "Next\u{00A0}Page" <|
        [ ClickableText.small
        , ClickableText.css [ Css.marginLeft (Css.px 10) ]
        ]
            ++ linkAttributes (goToPage (currentPageIndex + 1)) maybeUrl


directPageLink : (Int -> msg) -> Int -> Int -> String -> Html msg
directPageLink goToPage currentPageIndex page url =
    let
        humanPage =
            String.fromInt (page + 1)
    in
    ClickableText.link humanPage <|
        [ ClickableText.small
        , List.filterMap identity
            [ Just (Aria.label ("Page " ++ humanPage))
            , if page == currentPageIndex then
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
            , if page == currentPageIndex then
                Css.batch
                    [ Css.backgroundColor Colors.glacier
                    , Css.color Colors.navy
                    ]

              else
                Css.batch []
            ]
        ]
            ++ linkAttributes (goToPage page) (Just url)


linkAttributes : msg -> Maybe String -> List (ClickableText.Attribute msg)
linkAttributes msg maybeUrl =
    case maybeUrl of
        Just url ->
            [ ClickableText.onClick msg
            , ClickableText.linkSpa url
            ]

        Nothing ->
            [ ClickableText.disabled True ]
