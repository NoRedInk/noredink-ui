module Nri.Ui.Modal.V6 exposing
    ( Model, init
    , Msg, update, subscriptions
    , open, close
    , info, warning, FocusableElementAttrs
    , viewContent, viewFooter
    , closeButton
    )

{-| Changes from V5:

  - Removes button helpers, now that we can use Nri.Ui.Button.V9 directly

These changes have required major API changes. Be sure to wire up subscriptions!

    import Html.Styled exposing (..)
    import Nri.Ui.Button.V9 as Button
    import Nri.Ui.Modal.V6 as Modal

    type Msg
        = ModalMsg Modal.Msg
        | DoSomthing

    view : Modal.Model -> Html Msg
    view state =
        Modal.info
            { title = "Modal Header"
            , visibleTitle = True
            , wrapMsg = ModalMsg
            , content =
                \{ onlyFocusableElement } ->
                    div []
                        [ Modal.viewContent [ text "Content goes here!" ]
                        , Modal.viewFooter
                            [ Button.button "Continue"
                                [ Button.primary
                                , Button.onClick DoSomthing
                                , Button.custom onlyFocusableElement
                                ]
                            , text "`onlyFocusableElement` will trap the focus on the 'Continue' button."
                            ]
                        ]
            }
            state

    subscriptions : Modal.Model -> Sub Msg
    subscriptions state =
        Modal.subscriptions state

    view init
    --> text ""  -- a closed modal


## State and updates

@docs Model, init
@docs Msg, update, subscriptions

@docs open, close


## Views


### Modals

@docs info, warning, FocusableElementAttrs


### View containers

@docs viewContent, viewFooter


## X icon

@docs closeButton

-}

import Accessibility.Modal as Modal
import Accessibility.Style
import Accessibility.Styled as Html exposing (..)
import Accessibility.Styled.Style
import Accessibility.Styled.Widget as Widget
import Color
import Color.Transparent
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
close : Msg
close =
    Modal.close


{-| Pass the id of the element that focus should return to when the modal closes.
-}
open : String -> Msg
open =
    Modal.open


{-| -}
type alias FocusableElementAttrs msg =
    { onlyFocusableElement : List (Attribute msg)
    , firstFocusableElement : List (Attribute msg)
    , lastFocusableElement : List (Attribute msg)
    }


{-| -}
info :
    { visibleTitle : Bool
    , title : String
    , content : FocusableElementAttrs msg -> Html msg
    , wrapMsg : Msg -> msg
    }
    -> Model
    -> Html msg
info config model =
    view { overlayColor = Colors.navy, titleColor = Colors.navy } config model


{-| -}
warning :
    { visibleTitle : Bool
    , title : String
    , content : FocusableElementAttrs msg -> Html msg
    , wrapMsg : Msg -> msg
    }
    -> Model
    -> Html msg
warning config model =
    view { overlayColor = Colors.gray20, titleColor = Colors.red } config model


view :
    { overlayColor : Css.Color, titleColor : Css.Color }
    ->
        { visibleTitle : Bool
        , title : String
        , content : FocusableElementAttrs msg -> Html msg
        , wrapMsg : Msg -> msg
        }
    -> Model
    -> Html msg
view { overlayColor, titleColor } config model =
    Modal.view
        { overlayColor = toOverlayColor overlayColor
        , wrapMsg = config.wrapMsg
        , modalAttributes = modalStyles
        , title = viewTitle titleColor { title = config.title, visibleTitle = config.visibleTitle }
        , content =
            \{ onlyFocusableElement, firstFocusableElement, lastFocusableElement } ->
                { onlyFocusableElement = List.map Html.Styled.Attributes.fromUnstyled onlyFocusableElement
                , firstFocusableElement = List.map Html.Styled.Attributes.fromUnstyled firstFocusableElement
                , lastFocusableElement = List.map Html.Styled.Attributes.fromUnstyled lastFocusableElement
                }
                    |> config.content
                    |> toUnstyled
        }
        model
        |> fromUnstyled


toOverlayColor : Css.Color -> String
toOverlayColor color =
    color
        |> Nri.Ui.Colors.Extra.fromCssColor
        |> Color.Transparent.fromColor (Color.Transparent.customOpacity 0.9)
        |> Color.Transparent.toRGBAString


modalStyles : List (Root.Attribute Never)
modalStyles =
    [ style "width" "600px"
    , style "max-height" "calc(100vh - 100px)"
    , style "padding" "40px 0 40px 0"
    , style "margin" "75px auto"
    , style "background-color" ((Color.toRGBString << Nri.Ui.Colors.Extra.fromCssColor) Colors.white)
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
        , style "color" ((Color.toRGBString << Nri.Ui.Colors.Extra.fromCssColor) color)
        ]

      else
        Accessibility.Style.invisible
    )


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
closeButton : (Msg -> msg) -> List (Attribute msg) -> Html msg
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
            :: focusableElementAttrs
        )
        [ Nri.Ui.Svg.V1.toHtml Nri.Ui.SpriteSheet.xSvg
        ]
