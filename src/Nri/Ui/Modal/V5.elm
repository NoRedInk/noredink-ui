module Nri.Ui.Modal.V5 exposing
    ( Model, Dismissibility(..)
    , info
    , warning
    )

{-| Changes from V4:

@docs Model, Dismissibility
@docs info
@docs warning

-}

import Accessibility.Styled as Html exposing (..)
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Widget as Widget
import Color
import Css
import Css.Global exposing (Snippet, body, children, descendants, everything, selector)
import Html.Styled
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Modal
import Nri.Ui
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.SpriteSheet
import Nri.Ui.Svg.V1


{-|

  - `onDismiss`: See `Dismissibility`
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
    , onDismiss : Dismissibility msg
    , width : Maybe Int
    }


{-|

  - `NotDismissible`
  - `WithBackgroundOrX`: user can dismiss by clicking "X" (top right) or modal backdrop
  - `WithOnlyX`: user can dismiss by clicking "X" (top right)

-}
type Dismissibility msg
    = NotDismissible
    | WithBackgroundOrX msg
    | WithOnlyX msg


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
                Css.backgroundColor (Nri.Ui.Colors.Extra.withAlpha 0.9 Colors.navy)

            Warning ->
                Css.backgroundColor (Nri.Ui.Colors.Extra.withAlpha 0.9 Colors.gray20)
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
                NotDismissible ->
                    []

                WithBackgroundOrX msg ->
                    [ onClick msg ]

                WithOnlyX msg ->
                    []
            )
            []
        , Nri.Ui.styled div
            "modal-container"
            [ Css.width (Css.px 600)
            , Css.maxHeight <| Css.calc (Css.vh 100) Css.minus (Css.px 100)
            , Css.padding4 (Css.px 40) Css.zero (Css.px 40) Css.zero
            , Css.margin2 (Css.px 75) Css.auto
            , Css.backgroundColor Colors.white
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
              Css.Global.global
                [ Css.Global.body
                    [ Css.overflow Css.hidden ]
                ]
            , case onDismiss of
                NotDismissible ->
                    text ""

                WithBackgroundOrX msg ->
                    closeButton msg

                WithOnlyX msg ->
                    closeButton msg
            , if visibleTitle then
                viewHeader modalType title

              else
                text ""
            , viewContent [ content ]
            , viewFooter footerContent
            ]
        ]


modalConfig :
    ModalType
    -> String
    -> Html msg
    ->
        { ifClosed : Html msg
        , overlayColor : String
        , modalContainer : List (Html msg) -> Html msg
        , title : ( String, List (Attribute Never) )
        , content : Html msg
        }
modalConfig modalType title content =
    { ifClosed = text ""
    , overlayColor =
        (case modalType of
            Info ->
                Colors.navy

            Warning ->
                Colors.gray20
        )
            |> Nri.Ui.Colors.Extra.withAlpha 0.9
            |> Nri.Ui.Colors.Extra.toCoreColor
            |> Color.toCssString
    , modalContainer =
        \modalContents ->
            div
                [ css
                    [ Css.width (Css.px 600)
                    , Css.maxHeight <| Css.calc (Css.vh 100) Css.minus (Css.px 100)
                    , Css.padding4 (Css.px 40) Css.zero (Css.px 40) Css.zero
                    , Css.margin2 (Css.px 75) Css.auto
                    , Css.backgroundColor Colors.white
                    , Css.borderRadius (Css.px 20)
                    , Css.property "box-shadow" "0 1px 10px 0 rgba(0, 0, 0, 0.35)"
                    , Css.position Css.relative -- required for closeButtonContainer
                    , Css.displayFlex
                    , Css.alignItems Css.center
                    , Css.flexDirection Css.column
                    , Css.flexWrap Css.noWrap
                    , Fonts.baseFont
                    ]
                ]
                [ -- This global <style> node sets overflow to hidden on the body element,
                  -- thereby preventing the page from scrolling behind the backdrop when the modal is
                  -- open (and this node is present on the page).
                  Css.Global.global
                    [ Css.Global.body
                        [ Css.overflow Css.hidden ]
                    ]
                , viewContent modalContents
                ]
    , title =
        ( title
        , [ css
                [ Css.fontWeight (Css.int 700)
                , Css.lineHeight (Css.px 27)
                , Css.margin2 Css.zero (Css.px 49)
                , Css.fontSize (Css.px 20)
                , Fonts.baseFont
                , Css.textAlign Css.center
                , case modalType of
                    Info ->
                        Css.color Colors.navy

                    Warning ->
                        Css.color Colors.red
                ]
          ]
        )
    , content = content
    }


closeButton : msg -> Html msg
closeButton msg =
    Nri.Ui.styled button
        "close-button-container"
        [ Css.position Css.absolute
        , Css.top Css.zero
        , Css.right Css.zero
        , Css.padding (Css.px 25)
        , Css.borderWidth Css.zero
        , Css.width (Css.px 75)
        , Css.backgroundColor Css.transparent
        , Css.cursor Css.pointer
        , Css.color Colors.azure
        , Css.hover [ Css.color Colors.azureDark ]
        , Css.property "transition" "color 0.1s"
        ]
        [ onClick msg
        , Widget.label "Close modal"
        ]
        [ Nri.Ui.Svg.V1.toHtml Nri.Ui.SpriteSheet.xSvg
        ]


viewHeader : ModalType -> String -> Html msg
viewHeader modalType title =
    Nri.Ui.styled Html.h3
        "modal-header"
        ((case modalType of
            Info ->
                Css.color Colors.navy

            Warning ->
                Css.color Colors.red
         )
            :: [ Css.fontWeight (Css.int 700)
               , Css.lineHeight (Css.px 27)
               , Css.margin2 Css.zero (Css.px 49)
               , Css.fontSize (Css.px 20)
               , Fonts.baseFont
               , Css.textAlign Css.center
               ]
        )
        []
        [ Html.text title
        ]


viewContent : List (Html msg) -> Html msg
viewContent =
    Nri.Ui.styled div
        "modal-content"
        [ Css.overflowY Css.auto
        , Css.padding2 (Css.px 30) (Css.px 40)
        , Css.width (Css.pct 100)
        , Css.minHeight (Css.px 150)
        , Css.boxSizing Css.borderBox
        ]
        []


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
                , Css.minHeight (Css.px 125) -- so the footer doesn't compress on Safari
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
