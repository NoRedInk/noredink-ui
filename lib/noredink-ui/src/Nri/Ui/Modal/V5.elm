module Nri.Ui.Modal.V5 exposing
    ( Model, init
    , Msg, update, subscriptions
    , info, warning, FocusableElementAttrs
    , viewContent, viewFooter
    , launchButton, closeButton
    , primaryButton, secondaryButton, dangerButton
    )

{-| Changes from V4:

  - Remove dependence on Assets
  - Adds keyboard support (escape key to exit, tabs contained within the modal)

These changes have required major API changes. Be sure to wire up subscriptions!

    import Html.Styled exposing (..)
    import Nri.Ui.Modal.V5 as Modal

    view : Modal.State -> Html Msg
    view state =
        Modal.info
            { title = { title = "Modal Header", visibleTitle = True }
            , wrapMsg = ModalMsg
            , content =
                \{ onlyFocusableElement } ->
                    div []
                        [ Modal.viewContent [ text "Content goes here!" ]
                        , Modal.viewFooter
                            [ Modal.primaryButton DoSomething "Continue" onlyFocusableElement
                            , text "`onlyFocusableElement` will trap the focus on the 'Continue' button."
                            ]
                        ]
            }
            state

    subscriptions : Modal.State -> Sub Msg
    subscriptions state =
        Modal.subscriptions state


## State and updates

@docs Model, init
@docs Msg, update, subscriptions


## Views


### Modals

@docs info, warning, FocusableElementAttrs


### View containers

@docs viewContent, viewFooter


### Buttons

@docs launchButton, closeButton
@docs primaryButton, secondaryButton, dangerButton

-}

import Accessibility.Modal as Modal
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
init : Model
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
update : { dismissOnEscAndOverlayClick : Bool } -> Msg -> Model -> ( Model, Cmd Msg )
update config msg model =
    Modal.update config msg model


{-| -}
type alias FocusableElementAttrs msg =
    { onlyFocusableElement : List (Root.Attribute msg)
    , firstFocusableElement : List (Root.Attribute msg)
    , lastFocusableElement : List (Root.Attribute msg)
    }


{-| -}
info :
    { title : { visibleTitle : Bool, title : String }
    , content : FocusableElementAttrs msg -> Html msg
    , wrapMsg : Msg -> msg
    }
    -> Model
    -> Html msg
info config model =
    Modal.view
        { overlayColor = toOverlayColor Colors.navy
        , wrapMsg = config.wrapMsg
        , modalAttributes = modalStyles
        , title = viewTitle Colors.navy config.title
        , content = config.content >> toUnstyled
        }
        model
        |> fromUnstyled


{-| -}
warning :
    { title : { visibleTitle : Bool, title : String }
    , content : FocusableElementAttrs msg -> Html msg
    , wrapMsg : Msg -> msg
    }
    -> Model
    -> Html msg
warning config model =
    Modal.view
        { overlayColor = toOverlayColor Colors.gray20
        , wrapMsg = config.wrapMsg
        , modalAttributes = modalStyles
        , title = viewTitle Colors.red config.title
        , content = config.content >> toUnstyled
        }
        model
        |> fromUnstyled


toOverlayColor : Css.Color -> String
toOverlayColor color =
    toCssString (Nri.Ui.Colors.Extra.withAlpha 0.9 color)


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
viewTitle : Css.Color -> { visibleTitle : Bool, title : String } -> ( String, List (Root.Attribute Never) )
viewTitle color { visibleTitle, title } =
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
    Nri.Ui.Colors.Extra.fromCssColor >> Color.toRGBString


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



--BUTTONS


{-| -}
launchButton : (Msg -> msg) -> List Css.Style -> String -> Html msg
launchButton wrapMsg styles label =
    button
        (css styles
            :: List.map Html.Styled.Attributes.fromUnstyled
                (Modal.openOnClick wrapMsg (String.replace " " "-" label))
        )
        [ text label ]


{-| -}
closeButton : (Msg -> msg) -> List (Root.Attribute msg) -> Html msg
closeButton wrapMsg focusableElementAttrs =
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
            :: Html.Styled.Attributes.map wrapMsg (onClick Modal.close)
            :: List.map Html.Styled.Attributes.fromUnstyled focusableElementAttrs
        )
        [ Nri.Ui.Svg.V1.toHtml Nri.Ui.SpriteSheet.xSvg
        ]


{-| -}
primaryButton : msg -> String -> List (Root.Attribute msg) -> Html msg
primaryButton msg label focusableElementAttrs =
    Nri.Ui.styled button
        "modal__primary-button"
        [ buttonStyle, colorStyle PrimaryColors, sizeStyle ]
        (onClick msg :: List.map Html.Styled.Attributes.fromUnstyled focusableElementAttrs)
        [ text label ]


{-| -}
secondaryButton : msg -> String -> List (Root.Attribute msg) -> Html msg
secondaryButton msg label focusableElementAttrs =
    Nri.Ui.styled button
        "modal__secondary-button"
        [ buttonStyle
        , colorStyle SecondaryColors
        , Css.fontSize (Css.px 20)
        , Css.marginTop (Css.px 30)
        ]
        (onClick msg :: List.map Html.Styled.Attributes.fromUnstyled focusableElementAttrs)
        [ text label ]


{-| -}
dangerButton : msg -> String -> List (Root.Attribute msg) -> Html msg
dangerButton msg label focusableElementAttrs =
    Nri.Ui.styled button
        "modal__warning-button"
        [ buttonStyle, colorStyle DangerColors, sizeStyle ]
        (onClick msg :: List.map Html.Styled.Attributes.fromUnstyled focusableElementAttrs)
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
