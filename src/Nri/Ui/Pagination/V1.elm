module Nri.Ui.Pagination.V1 exposing (view)

{-| Display a nav element aiding in navigating between pages of content.

    [ Previous ] [ 1 ] [ 2 ] [ 3 ] [ 4 ] [ Next ]

@docs view

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Aria as Aria
import Css
import Css.Transitions as Transitions
import Html.Styled.Attributes as Attrs exposing (css)
import List.Extra
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.V3 exposing (viewIf)


{-| -}
view : (Int -> msg) -> Int -> List a -> Html msg
view goToPage currentPageIndex pages =
    viewIf (\_ -> view_ goToPage currentPageIndex pages)
        (List.length pages > 1)


view_ : (Int -> msg) -> Int -> List a -> Html msg
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
        [ ClickableText.button "Previous\u{00A0}Page"
            [ ClickableText.small
            , if currentPageIndex == 0 then
                ClickableText.custom [ Attrs.disabled True ]

              else
                ClickableText.onClick (goToPage (currentPageIndex - 1))
            , ClickableText.css [ Css.marginRight (Css.px 10) ]
            ]
        , List.range 0 lastPageIndex
            |> List.map
                (\page ->
                    let
                        humanPage =
                            String.fromInt (page + 1)
                    in
                    Html.li []
                        [ ClickableText.button humanPage
                            [ ClickableText.small
                            , ClickableText.onClick (goToPage page)
                            , List.filterMap identity
                                [ Just (Aria.label ("Page " ++ humanPage))
                                , if page == currentPageIndex then
                                    Just Aria.currentPage

                                  else
                                    Nothing
                                ]
                                |> ClickableText.custom
                            , ClickableText.css
                                [ Css.width (Css.px 36)
                                , Css.height (Css.px 33)
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
                        ]
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
        , ClickableText.button "Next\u{00A0}Page"
            [ ClickableText.small
            , if currentPageIndex == lastPageIndex then
                ClickableText.custom [ Attrs.disabled True ]

              else
                ClickableText.onClick (goToPage (currentPageIndex + 1))
            , ClickableText.css [ Css.marginLeft (Css.px 10) ]
            ]
        ]
