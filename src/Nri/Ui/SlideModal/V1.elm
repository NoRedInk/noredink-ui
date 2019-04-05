module Nri.Ui.SlideModal.V1 exposing (view)

import Accessibility.Styled as Html exposing (..)
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Widget as Widget
import Css
import Css.Global
import Html.Styled
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Nri.Ui
import Nri.Ui.AssetPath exposing (Asset(..))
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Icon.V3 as Icon
import Nri.Ui.Text.V2 as Text


view { icon, title, content, onDismiss, footerContent, width } =
    Nri.Ui.styled div
        "modal-backdrop-container"
        (Css.backgroundColor (Nri.Ui.Colors.Extra.withAlpha 0.9 Colors.navy)
            :: [ Css.height (Css.vh 100)
               , Css.left Css.zero
               , Css.overflow Css.hidden
               , Css.position Css.fixed
               , Css.top Css.zero
               , Css.width (Css.pct 100)
               , Css.zIndex (Css.int 200)
               , Css.displayFlex
               , Css.alignItems Css.center
               , Css.justifyContent Css.center
               ]
        )
        [ Role.dialog
        , Widget.modal True
        ]
        [ Nri.Ui.styled div
            "modal-container"
            [ Css.width (Css.px 600)
            , Css.minHeight (Css.px 400)
            , Css.maxHeight <| Css.calc (Css.vh 100) Css.minus (Css.px 100)
            , Css.padding4 (Css.px 35) Css.zero (Css.px 25) Css.zero
            , Css.margin2 (Css.px 75) Css.auto
            , Css.backgroundColor Colors.white
            , Css.borderRadius (Css.px 20)
            , Css.property "box-shadow" "0 1px 10px 0 rgba(0, 0, 0, 0.35)"
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.flexDirection Css.column
            , Css.flexWrap Css.noWrap
            , Fonts.baseFont
            ]
            []
            [ -- This global <style> node sets overflow to hidden on the body element,
              -- thereby preventing the page from scrolling behind the backdrop when the modal is
              -- open (and this node is present on the page).
              Css.Global.global
                [ Css.Global.body
                    [ Css.overflow Css.hidden ]
                ]
            , viewIcon icon
            , Text.subHeading [ Html.text title ]
            , viewContent content
            , viewFooter footerContent
            ]
        ]


viewContent : Html msg -> Html msg
viewContent content =
    Nri.Ui.styled div
        "modal-content"
        [ Css.overflowY Css.auto
        , Css.padding2 (Css.px 30) (Css.px 45)
        , Css.width (Css.pct 100)
        , Css.minHeight (Css.px 150)
        , Css.boxSizing Css.borderBox
        ]
        []
        [ content ]


viewFooter : List (Html msg) -> Html msg
viewFooter footerContent =
    case footerContent of
        [] ->
            Html.text ""

        _ ->
            Nri.Ui.styled div
                "modal-footer"
                [ Css.alignItems Css.center
                , Css.displayFlex
                , Css.flexDirection Css.column
                , Css.flexGrow (Css.int 2)
                , Css.flexWrap Css.noWrap
                , Css.margin4 (Css.px 20) Css.zero Css.zero Css.zero
                , Css.width (Css.pct 100)
                ]
                []
                (List.map
                    (\x ->
                        Nri.Ui.styled div
                            "modal-footer-item"
                            [ Css.margin4 (Css.px 10) Css.zero Css.zero Css.zero
                            , Css.firstChild
                                [ Css.margin Css.zero
                                ]
                            ]
                            []
                            [ x ]
                    )
                    footerContent
                )


viewIcon : Html msg -> Html msg
viewIcon svg =
    div
        [ css
            [ Css.width (Css.px 100)
            , Css.height (Css.px 100)
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.Global.children
                [ Css.Global.svg
                    [ Css.maxHeight (Css.px 100)
                    , Css.width (Css.px 100)
                    ]
                ]
            ]
        ]
        [ svg ]
