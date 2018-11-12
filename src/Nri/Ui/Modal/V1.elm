module Nri.Ui.Modal.V1 exposing
    ( Model
    , info
    , styles
    , warning
    )

{-|

@docs Model
@docs info
@docs styles
@docs warning

-}

import Css
import Css.Global exposing (Snippet, body, children, descendants, everything, selector)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Styled exposing (toUnstyled)
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Styles.V1


type ModalType
    = Info
    | Warning


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
    div
        [ case modalType of
            Info ->
                styles.class [ BackdropContainer, BackdropContainerInfo ]

            Warning ->
                styles.class [ BackdropContainer, BackdropContainerWarning ]
        , Html.Attributes.attribute "role" "dialog"
        , Html.Attributes.attribute "aria-modal" "true"
        , Html.Attributes.attribute "aria-label" title
        ]
        [ div
            (case onDismiss of
                Nothing ->
                    [ styles.class [ ClickCatcher ] ]

                Just msg ->
                    [ styles.class [ ClickCatcher ], onClick msg ]
            )
            []
        , div
            [ styles.class [ ModalContainer ]
            , Html.Attributes.style <|
                case width of
                    Nothing ->
                        []

                    Just width ->
                        [ ( "width", toString width ++ "px" ) ]
            ]
            [ styleNode
            , if visibleTitle then
                viewHeader modalType title

              else
                Html.text ""
            , viewContent modalType content
            , viewFooter footerContent
            ]
        ]


viewHeader : ModalType -> String -> Html msg
viewHeader modalType title =
    Html.h3
        [ case modalType of
            Info ->
                styles.class [ Heading, HeadingInfo ]

            Warning ->
                styles.class [ Heading, HeadingWarning ]
        ]
        [ Html.text title
        ]


viewContent : ModalType -> Html msg -> Html msg
viewContent modalType content =
    styles.div Content
        [ content ]


viewFooter : List (Html msg) -> Html msg
viewFooter footerContent =
    case footerContent of
        [] ->
            Html.text ""

        _ ->
            styles.div Footer
                (List.map (\x -> styles.div FooterItem [ x ]) footerContent)


{-| HACK: This inline <style> node sets overflow to hidden on the body element,
thereby preventing the page from scrolling behind the backdrop when the modal is
open (and this node is present on the page).
-}
styleNode : Html msg
styleNode =
    [ Css.Global.body
        [ Css.overflow Css.hidden ]
    ]
        |> Css.Global.global
        |> toUnstyled


type CssClasses
    = BackdropContainer
    | BackdropContainerInfo
    | BackdropContainerWarning
    | ClickCatcher
    | Content
    | Footer
    | FooterItem
    | Heading
    | HeadingInfo
    | HeadingWarning
    | ModalContainer


{-| -}
styles : Nri.Ui.Styles.V1.Styles Never CssClasses b
styles =
    Nri.Ui.Styles.V1.styles "Nri-Ui-Modal-"
        [ Css.Global.class BackdropContainer
            [ Css.height (Css.vh 100)
            , Css.left Css.zero
            , Css.overflow Css.hidden
            , Css.position Css.fixed
            , Css.top Css.zero
            , Css.width (Css.pct 100)
            , Css.zIndex (Css.int 3)
            ]
        , Css.Global.class BackdropContainerWarning
            [ Css.backgroundColor (Nri.Ui.Colors.Extra.withAlpha 0.9 Nri.Ui.Colors.V1.gray20)
            ]
        , Css.Global.class BackdropContainerInfo
            [ Css.backgroundColor (Nri.Ui.Colors.Extra.withAlpha 0.9 Nri.Ui.Colors.V1.navy)
            ]
        , Css.Global.class ClickCatcher
            [ Css.bottom Css.zero
            , Css.left Css.zero
            , Css.position Css.absolute
            , Css.right Css.zero
            , Css.top Css.zero
            ]
        , Css.Global.class Content
            [ Css.flexShrink (Css.int 4)
            , Css.overflowY Css.auto
            , Css.padding2 Css.zero (Css.px 45)
            , Css.width (Css.pct 100)
            ]
        , Css.Global.class Footer
            [ Css.alignItems Css.center
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.flexGrow (Css.int 2)
            , Css.flexWrap Css.noWrap
            , Css.margin4 (Css.px 40) Css.zero Css.zero Css.zero
            , Css.width (Css.pct 100)
            ]
        , Css.Global.class FooterItem
            [ Css.margin4 (Css.px 10) Css.zero Css.zero Css.zero
            , Css.firstChild
                [ Css.margin Css.zero
                ]
            ]
        , Css.Global.class Heading
            [ Css.fontWeight (Css.int 700)
            , Css.lineHeight (Css.px 27)
            , Css.margin4 Css.zero Css.zero (Css.px 40) Css.zero
            ]
        , Css.Global.class HeadingInfo
            [ Css.fontSize (Css.px 20)
            , Fonts.baseFont
            , Css.color Nri.Ui.Colors.V1.navy
            ]
        , Css.Global.class HeadingWarning
            [ Css.fontSize (Css.px 20)
            , Fonts.baseFont
            , Css.color Nri.Ui.Colors.V1.red
            ]
        , Css.Global.class ModalContainer
            [ Css.alignItems Css.center
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.flexWrap Css.noWrap
            , Css.backgroundColor Nri.Ui.Colors.V1.white
            , Css.borderRadius (Css.px 20)
            , Css.margin2 (Css.px 75) Css.auto
            , Css.maxHeight (Css.calc (Css.vh 100) Css.minus (Css.px 150))
            , Css.padding2 (Css.px 45) Css.zero
            , Css.position Css.relative -- required for closeButtonContainer
            , Css.property "box-shadow" "0 1px 10px 0 rgba(0, 0, 0, 0.35)"
            , Css.width (Css.px 600)
            ]
        ]
