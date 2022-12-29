module Nri.Ui.Pagination.V1 exposing (view)

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Aria as Aria
import Css
import Css.Transitions as Transitions
import Html.Styled.Attributes as Attrs exposing (css)
import List.Zipper as Zipper exposing (Zipper)
import List.Zipper.Extra
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors


view : (Int -> msg) -> Zipper a -> Html msg
view goToPage pages =
    let
        currentPage =
            List.Zipper.Extra.currentIndex pages
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
            , if Zipper.isFirst pages then
                ClickableText.custom [ Attrs.disabled True ]

              else
                ClickableText.onClick (goToPage (currentPage - 1))
            , ClickableText.css [ Css.marginRight (Css.px 10) ]
            ]
        , List.range 0 (List.Zipper.Extra.size pages - 1)
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
                                , if page == currentPage then
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
                                , if page == currentPage then
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
            , if Zipper.isLast pages then
                ClickableText.custom [ Attrs.disabled True ]

              else
                ClickableText.onClick (goToPage (currentPage + 1))
            , ClickableText.css [ Css.marginLeft (Css.px 10) ]
            ]
        ]
