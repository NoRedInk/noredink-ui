module Nri.Ui.Modal.V5 exposing
    ( info, warning
    , Model, init, Msg, update, subscriptions
    , viewTitle, viewContent, viewFooter
    , launchButton, closeButton
    , primaryButton, secondaryButton, dangerButton
    , FocusableElement(..)
    )

{-| Changes from V4:

@docs info, warning
@docs Model, init, Msg, update, subscriptions

@docs viewTitle, viewContent, viewFooter
@docs launchButton, closeButton
@docs primaryButton, secondaryButton, dangerButton

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
import Html.Attributes exposing (style)
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
    { launchButton : Maybe (Html msg)
    , title : Css.Color -> ( String, List (Root.Attribute Never) )
    , content : Html msg
    }
    -> Model
    -> Html msg
info config model =
    Modal.view
        { ifClosed =
            config.launchButton
                |> Maybe.map Html.Styled.toUnstyled
                |> Maybe.withDefault (Root.text "")
        , overlayColor = toOverlayColor Colors.navy
        , modalContainer = viewModalContainer
        , title = config.title Colors.navy
        , content = Html.Styled.toUnstyled config.content
        }
        model
        |> Html.Styled.fromUnstyled


{-| -}
warning :
    { launchButton : Maybe (Html msg)
    , title : Css.Color -> ( String, List (Root.Attribute Never) )
    , content : Html msg
    }
    -> Model
    -> Html msg
warning config model =
    Modal.view
        { ifClosed =
            config.launchButton
                |> Maybe.map Html.Styled.toUnstyled
                |> Maybe.withDefault (Root.text "")
        , overlayColor = toOverlayColor Colors.gray20
        , modalContainer = viewModalContainer
        , title = config.title Colors.red
        , content = Html.Styled.toUnstyled config.content
        }
        model
        |> Html.Styled.fromUnstyled


launchButton : List Css.Style -> String -> Html Msg
launchButton styles label =
    button
        (css styles
            :: (Modal.openOnClick (String.replace " " "-" label)
                    |> List.map Html.Styled.Attributes.fromUnstyled
               )
        )
        [ text label ]


toOverlayColor : Css.Color -> String
toOverlayColor color =
    color
        |> Nri.Ui.Colors.Extra.withAlpha 0.9
        |> Nri.Ui.Colors.Extra.toCoreColor
        |> Color.toCssString


viewModalContainer : List (Root.Html msg) -> Root.Html msg
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
        , div [] (List.map Html.Styled.fromUnstyled modalContents)
        ]
        |> Html.Styled.toUnstyled


viewTitle : { visibleTitle : Bool, title : String } -> Css.Color -> ( String, List (Root.Attribute Never) )
viewTitle { visibleTitle, title } color =
    ( title
    , if visibleTitle then
        [ style "font-weight" "700"
        , style "line-height" "27px"
        , style "margin" "0 49px"
        , style "font-size" "20px"
        , style "text-align" "center"
        , style "color" (Color.toCssString (Nri.Ui.Colors.Extra.toCoreColor color))
        ]

      else
        Accessibility.Style.invisible
    )


type FocusableElement
    = OnlyFocusableElement
    | FirstFocusableElement
    | LastFocusableElement


closeButton : FocusableElement -> Html Msg
closeButton focusableElement =
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
        (Widget.label "Close modal"
            :: (Modal.closeOnClick
                    :: (case focusableElement of
                            OnlyFocusableElement ->
                                Modal.singleFocusableElement

                            FirstFocusableElement ->
                                Modal.firstFocusableElement

                            LastFocusableElement ->
                                Modal.lastFocusableElement
                       )
                    |> List.map Html.Styled.Attributes.fromUnstyled
               )
        )
        [ Nri.Ui.Svg.V1.toHtml Nri.Ui.SpriteSheet.xSvg
        ]


{-| -}
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


{-| -}
viewFooter : List (Html msg) -> Html msg
viewFooter =
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


{-| -}
primaryButton : FocusableElement -> msg -> String -> Html msg
primaryButton focusableElement msg label =
    Nri.Ui.styled button
        "modal__primary-button"
        [ buttonStyle, colorStyle PrimaryColors, sizeStyle ]
        [ onClick msg ]
        [ text label ]


{-| -}
secondaryButton : FocusableElement -> msg -> String -> Html msg
secondaryButton focusableElement msg label =
    Nri.Ui.styled button
        "modal__secondary-button"
        [ buttonStyle, colorStyle SecondaryColors, sizeStyle ]
        [ onClick msg ]
        [ text label ]


{-| -}
dangerButton : FocusableElement -> msg -> String -> Html msg
dangerButton focusableElement msg label =
    Nri.Ui.styled button
        "modal__warning-button"
        [ buttonStyle, colorStyle DangerColors, sizeStyle ]
        [ onClick msg ]
        [ text label ]


buttonStyle : Css.Style
buttonStyle =
    Css.batch
        [ Css.cursor Css.pointer
        , -- Specifying the font can and should go away after bootstrap is removed from application.css
          Fonts.baseFont
        , Css.textOverflow Css.ellipsis
        , Css.overflow Css.hidden
        , Css.textDecoration Css.none
        , Css.backgroundImage Css.none
        , Css.textShadow Css.none
        , Css.property "transition" "background-color 0.2s, color 0.2s, box-shadow 0.2s, border 0.2s, border-width 0s"
        , Css.boxShadow Css.none
        , Css.border Css.zero
        , Css.marginBottom Css.zero
        , Css.hover [ Css.textDecoration Css.none ]
        , Css.display Css.inlineFlex
        , Css.alignItems Css.center
        , Css.justifyContent Css.center
        ]


type ColorPalette
    = PrimaryColors
    | SecondaryColors
    | DangerColors


colorStyle : ColorPalette -> Css.Style
colorStyle colorPalette =
    let
        config =
            case colorPalette of
                PrimaryColors ->
                    { background = Colors.azure
                    , hover = Colors.azureDark
                    , text = Colors.white
                    , border = Nothing
                    , shadow = Colors.azureDark
                    }

                SecondaryColors ->
                    { background = Colors.white
                    , hover = Colors.glacier
                    , text = Colors.azure
                    , border = Just <| Colors.azure
                    , shadow = Colors.azure
                    }

                DangerColors ->
                    { background = Colors.red
                    , hover = Colors.redDark
                    , text = Colors.white
                    , border = Nothing
                    , shadow = Colors.redDark
                    }
    in
    Css.batch
        [ Css.color config.text
        , Css.backgroundColor config.background
        , Css.fontWeight (Css.int 700)
        , Css.textAlign Css.center
        , case config.border of
            Nothing ->
                Css.borderStyle Css.none

            Just color ->
                Css.batch
                    [ Css.borderColor color
                    , Css.borderStyle Css.solid
                    ]
        , Css.borderBottomStyle Css.solid
        , Css.borderBottomColor config.shadow
        , Css.fontStyle Css.normal
        , Css.hover
            [ Css.color config.text
            , Css.backgroundColor config.hover
            , Css.disabled [ Css.backgroundColor config.background ]
            ]
        , Css.visited [ Css.color config.text ]
        ]


sizeStyle : Css.Style
sizeStyle =
    let
        config =
            { fontSize = 20
            , height = 56
            , imageHeight = 20
            , shadowHeight = 4
            , minWidth = 200
            }

        sizingAttributes =
            let
                verticalPaddingPx =
                    2
            in
            [ Css.minHeight (Css.px config.height)
            , Css.paddingTop (Css.px verticalPaddingPx)
            , Css.paddingBottom (Css.px verticalPaddingPx)
            ]

        widthAttributes =
            [ Css.paddingLeft (Css.px 16)
            , Css.paddingRight (Css.px 16)
            , Css.minWidth (Css.px 230)
            ]

        lineHeightPx =
            22
    in
    Css.batch
        [ Css.fontSize (Css.px config.fontSize)
        , Css.borderRadius (Css.px 8)
        , Css.lineHeight (Css.px lineHeightPx)
        , Css.boxSizing Css.borderBox
        , Css.borderWidth (Css.px 1)
        , Css.borderBottomWidth (Css.px config.shadowHeight)
        , Css.batch sizingAttributes
        , Css.batch widthAttributes
        ]
