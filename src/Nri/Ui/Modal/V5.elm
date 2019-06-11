module Nri.Ui.Modal.V5 exposing
    ( info, warning
    , Model, init, Msg, update, subscriptions
    , viewTitle, viewContent, viewFooter
    , launchButton, closeButton
    , FocusableElement(..), primaryButton, secondaryButton, dangerButton
    )

{-| Changes from V4:

@docs info, warning
@docs Model, init, Msg, update, subscriptions

@docs viewTitle, viewContent, viewFooter
@docs launchButton, closeButton
@docs FocusableElement, primaryButton, secondaryButton, dangerButton

-}

import Accessibility.Style
import Accessibility.Styled as Html exposing (..)
import Accessibility.Styled.Style
import Accessibility.Styled.Widget as Widget
import Color
import Css
import Css.Global
import Html as Root
import Html.Attributes exposing (style)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Modal
import Nri.Ui
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.SpriteSheet
import Nri.Ui.Svg.V1


{-| -}
type alias Model =
    Modal.Model


{-| -}
init : { dismissOnEscAndOverlayClick : Bool } -> Model
init =
    Modal.init


{-| -}
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
    { title : Css.Color -> ( String, List (Root.Attribute Never) )
    , content : Html msg
    , wrapMsg : Msg -> msg
    }
    -> Model
    -> Html msg
info config model =
    Modal.view
        { overlayColor = toOverlayColor Colors.navy
        , wrapMsg = config.wrapMsg
        , modalAttributes = modalStyles
        , title = config.title Colors.navy
        , content = toUnstyled config.content
        }
        model
        |> fromUnstyled


{-| -}
warning :
    { title : Css.Color -> ( String, List (Root.Attribute Never) )
    , content : Html msg
    , wrapMsg : Msg -> msg
    }
    -> Model
    -> Html msg
warning config model =
    Modal.view
        { overlayColor = toOverlayColor Colors.gray20
        , wrapMsg = config.wrapMsg
        , modalAttributes = modalStyles
        , title = config.title Colors.red
        , content = toUnstyled config.content
        }
        model
        |> fromUnstyled


{-| -}
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
        |> toCssString


modalStyles : List (Root.Attribute Never)
modalStyles =
    [ style "width" "600px"
    , style "max-height" "calc(100vh - 100px)"
    , style "padding" "40px 0 40px 0"
    , style "margin" "75px auto"
    , style "background-color" (toCssString Colors.white)
    , style "border-radius" "20px"
    , style "box-shadow" "0 1px 10px 0 rgba(0, 0, 0, 0.35)"
    , style "position" "relative" -- required for closeButtonContainer
    ]


{-| -}
viewTitle : { visibleTitle : Bool, title : String } -> Css.Color -> ( String, List (Root.Attribute Never) )
viewTitle { visibleTitle, title } color =
    ( title
    , if visibleTitle then
        [ style "font-weight" "700"
        , style "line-height" "27px"
        , style "margin" "0 49px"
        , style "font-size" "20px"
        , style "text-align" "center"
        , style "color" (toCssString color)
        ]

      else
        Accessibility.Style.invisible
    )


toCssString : Css.Color -> String
toCssString =
    Color.toCssString << Nri.Ui.Colors.Extra.toCoreColor


{-| -}
type FocusableElement
    = OnlyFocusableElement
    | FirstFocusableElement
    | MiddleFocusableElement
    | LastFocusableElement


withFocusTrap : FocusableElement -> List (Attribute Msg)
withFocusTrap focusableElement =
    List.map Html.Styled.Attributes.fromUnstyled
        (case focusableElement of
            OnlyFocusableElement ->
                Modal.singleFocusableElement

            FirstFocusableElement ->
                Modal.firstFocusableElement

            MiddleFocusableElement ->
                []

            LastFocusableElement ->
                Modal.lastFocusableElement
        )


{-| -}
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
            :: onClick Modal.close
            :: withFocusTrap focusableElement
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
primaryButton : FocusableElement -> msg -> (Msg -> msg) -> String -> Html msg
primaryButton focusableElement msg wrapMsg label =
    Nri.Ui.styled button
        "modal__primary-button"
        [ buttonStyle, colorStyle PrimaryColors, sizeStyle ]
        (onClick msg
            :: List.map (Html.Styled.Attributes.map wrapMsg)
                (withFocusTrap focusableElement)
        )
        [ text label ]


{-| -}
secondaryButton : FocusableElement -> msg -> (Msg -> msg) -> String -> Html msg
secondaryButton focusableElement msg wrapMsg label =
    Nri.Ui.styled button
        "modal__secondary-button"
        [ buttonStyle
        , colorStyle SecondaryColors
        , Css.fontSize (Css.px 20)
        , Css.marginTop (Css.px 30)
        ]
        (onClick msg
            :: List.map (Html.Styled.Attributes.map wrapMsg)
                (withFocusTrap focusableElement)
        )
        [ text label ]


{-| -}
dangerButton : FocusableElement -> msg -> (Msg -> msg) -> String -> Html msg
dangerButton focusableElement msg wrapMsg label =
    Nri.Ui.styled button
        "modal__warning-button"
        [ buttonStyle, colorStyle DangerColors, sizeStyle ]
        (onClick msg
            :: List.map (Html.Styled.Attributes.map wrapMsg)
                (withFocusTrap focusableElement)
        )
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
                    , shadow = Colors.azureDark
                    }

                SecondaryColors ->
                    { background = Colors.white
                    , hover = Colors.white
                    , text = Colors.azure
                    , shadow = Colors.white
                    }

                DangerColors ->
                    { background = Colors.red
                    , hover = Colors.redDark
                    , text = Colors.white
                    , shadow = Colors.redDark
                    }
    in
    Css.batch
        [ Css.color config.text
        , Css.backgroundColor config.background
        , Css.fontWeight (Css.int 700)
        , Css.textAlign Css.center
        , Css.borderStyle Css.none
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
