module Nri.Ui.Modal.V2
    exposing
        ( Model
        , info
        , warning
        )

{-| Changes from V1:

  - Use Styled Html

@docs Model
@docs info
@docs warning

-}

import Accessibility.Styled as Html exposing (..)
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Widget as Widget
import Css
import Css.Foreign exposing (Snippet, body, children, descendants, everything, selector)
import Html.Styled
import Html.Styled.Events exposing (onClick)
import Nri.Ui
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1
import Nri.Ui.Fonts.V1 as Fonts


{-|

  - `onDismiss`: If `Nothing`, the modal will not be dismissable
  - `visibleTitle`: If `False`, the title will still be used for screen readers
  - `content`: This will be placed in a `width:100%` div in the main area of the modal
  - `footerContent`: The optional items here will be stacked below the main content area and center-aligned.
    Commonly you will either give a list of Nri.Ui.Buttons,
    or an empty list.

-}
type alias Model msg =
    { title : String
    , visibleTitle : Bool
    , content : Html msg
    , footerContent : List (Html msg)
    , onDismiss : Maybe msg
    , width : Maybe Int
    }


type ModalType
    = Info
    | Warning


{-| -}
info : Model msg -> Html msg
info =
    view Info


{-| -}
warning : Model msg -> Html msg
warning =
    view Warning


view : ModalType -> Model msg -> Html msg
view modalType { title, visibleTitle, content, onDismiss, footerContent, width } =
    Nri.Ui.styled div
        "modal-backdrop-container"
        ((case modalType of
            Info ->
                Css.backgroundColor (Nri.Ui.Colors.Extra.withAlpha 0.9 Nri.Ui.Colors.V1.navy)

            Warning ->
                Css.backgroundColor (Nri.Ui.Colors.Extra.withAlpha 0.9 Nri.Ui.Colors.V1.gray20)
         )
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
        , Widget.label title
        , Widget.modal True
        ]
        [ Nri.Ui.styled Html.Styled.div
            "modal-click-catcher"
            [ Css.bottom Css.zero
            , Css.left Css.zero
            , Css.position Css.absolute
            , Css.right Css.zero
            , Css.top Css.zero
            ]
            (case onDismiss of
                Nothing ->
                    []

                Just msg ->
                    [ onClick msg ]
            )
            []
        , Nri.Ui.styled div
            "modal-container"
            [ Css.width (Css.px 600)
            , Css.maxHeight <| Css.calc (Css.vh 100) Css.minus (Css.px 100)
            , Css.padding4 (Css.px 35) Css.zero (Css.px 25) Css.zero
            , Css.margin2 (Css.px 75) Css.auto
            , Css.backgroundColor Nri.Ui.Colors.V1.white
            , Css.borderRadius (Css.px 20)
            , Css.property "box-shadow" "0 1px 10px 0 rgba(0, 0, 0, 0.35)"
            , Css.position Css.relative -- required for closeButtonContainer
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
              Css.Foreign.global
                [ Css.Foreign.body
                    [ Css.overflow Css.hidden ]
                ]
            , if visibleTitle then
                viewHeader modalType title
              else
                text ""
            , viewContent modalType content
            , viewFooter footerContent
            ]
        ]


viewHeader : ModalType -> String -> Html msg
viewHeader modalType title =
    Nri.Ui.styled Html.h3
        "modal-header"
        ((case modalType of
            Info ->
                Css.color Nri.Ui.Colors.V1.navy

            Warning ->
                Css.color Nri.Ui.Colors.V1.red
         )
            :: [ Css.fontWeight (Css.int 700)
               , Css.lineHeight (Css.px 27)
               , Css.margin4 Css.zero Css.zero (Css.px 40) Css.zero
               , Css.fontSize (Css.px 20)
               , Fonts.baseFont
               ]
        )
        []
        [ Html.text title
        ]


viewContent : ModalType -> Html msg -> Html msg
viewContent modalType content =
    Nri.Ui.styled div
        "modal-content"
        [ Css.overflowY Css.scroll
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
                , Css.margin4 (Css.px 40) Css.zero Css.zero Css.zero
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
