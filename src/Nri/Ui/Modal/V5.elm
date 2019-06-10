module Nri.Ui.Modal.V5 exposing
    ( Model
    , info
    , warning
    , Msg, closeButton, init, launchButton, subscriptions, toOverlayColor, update, viewFooter, viewModalContainer, viewTitle
    )

{-| Changes from V4:

@docs Model, Dismissibility
@docs info
@docs warning

-}

import Accessibility.Style
import Accessibility.Styled as Html exposing (..)
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Style
import Accessibility.Styled.Widget as Widget
import Color
import Css
import Css.Global exposing (Snippet, body, children, descendants, everything, selector)
import Html as Root
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


type alias Model =
    Modal.Model


{-| -}
init : Model
init =
    Modal.init


type alias Msg =
    Modal.Msg


{-| Include the subscription if you want the modal to dismiss on `Esc`.
-}
subscriptions : Model -> Sub Msg
subscriptions =
    Modal.subscriptions


{-| -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Modal.update msg model


{-| -}
info :
    { launchButton : Maybe (Root.Html Msg)
    , title : String
    , visibleTitle : Bool
    , content : Html msg
    , parentMsg : Msg -> msg
    }
    -> Model
    -> Html msg
info config model =
    Modal.view
        { ifClosed =
            Maybe.map (Root.map config.parentMsg) config.launchButton
                |> Maybe.withDefault (Root.text "")
        , overlayColor = toOverlayColor Colors.navy
        , modalContainer =
            \l ->
                viewModalContainer (List.map Html.Styled.fromUnstyled l)
                    |> Html.Styled.toUnstyled
        , title =
            viewTitle
                { title = config.title
                , color = Colors.navy
                , visibleTitle = config.visibleTitle
                }
        , content = Html.Styled.toUnstyled config.content
        }
        model
        |> Html.Styled.fromUnstyled


{-| -}
warning :
    { launchButton : Maybe (Root.Html Msg)
    , title : String
    , visibleTitle : Bool
    , content : Html msg
    , parentMsg : Msg -> msg
    }
    -> Model
    -> Html msg
warning config model =
    Modal.view
        { ifClosed =
            Maybe.map (Root.map config.parentMsg) config.launchButton
                |> Maybe.withDefault (Root.text "")
        , overlayColor = toOverlayColor Colors.red
        , modalContainer =
            \l ->
                viewModalContainer (List.map Html.Styled.fromUnstyled l)
                    |> Html.Styled.toUnstyled
        , title =
            viewTitle
                { title = config.title
                , color = Colors.navy
                , visibleTitle = config.visibleTitle
                }
        , content = Html.Styled.toUnstyled config.content
        }
        model
        |> Html.Styled.fromUnstyled


launchButton : List Css.Style -> String -> Maybe (Root.Html Msg)
launchButton styles label =
    button
        (css styles
            :: (Modal.openOnClick (String.replace " " "-" label)
                    |> List.map Html.Styled.Attributes.fromUnstyled
               )
        )
        [ text label ]
        |> Html.Styled.toUnstyled
        |> Just


toOverlayColor : Css.Color -> String
toOverlayColor color =
    color
        |> Nri.Ui.Colors.Extra.withAlpha 0.9
        |> Nri.Ui.Colors.Extra.toCoreColor
        |> Color.toCssString


viewModalContainer modalContents =
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
        , Nri.Ui.styled div
            "modal-content"
            [ Css.overflowY Css.auto
            , Css.padding2 (Css.px 30) (Css.px 40)
            , Css.width (Css.pct 100)
            , Css.minHeight (Css.px 150)
            , Css.boxSizing Css.borderBox
            ]
            []
            modalContents
        ]


viewTitle { visibleTitle, title, color } =
    ( title
    , if visibleTitle then
        [--css
         --    [ Css.fontWeight (Css.int 700)
         --    , Css.lineHeight (Css.px 27)
         --    , Css.margin2 Css.zero (Css.px 49)
         --    , Css.fontSize (Css.px 20)
         --    , Fonts.baseFont
         --    , Css.textAlign Css.center
         --    , Css.color color
         --    ]
        ]

      else
        Accessibility.Style.invisible
    )


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
