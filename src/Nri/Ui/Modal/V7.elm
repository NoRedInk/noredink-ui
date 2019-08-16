module Nri.Ui.Modal.V7 exposing
    ( Model, init
    , Msg, update, subscriptions
    , open, close
    , info, warning
    , viewContent, viewFooter
    , Attribute
    , multipleFocusableElementView, onlyFocusableElementView
    , autofocusOnLastElement
    , closeButton
    )

{-| Changes from V6:

  - Modal starts a new stacking context, to prevent non-normal-flow elements from showing through the backdrop
  - Scrollable content shows a shadow

```
    import Html.Styled exposing (..)
    import Nri.Ui.Button.V9 as Button
    import Nri.Ui.Modal.V7 as Modal

    type Msg
        = ModalMsg Modal.Msg
        | DoSomthing

    view : Modal.Model -> Html Msg
    view state =
        Modal.info
            { title = "Modal Header"
            , visibleTitle = True
            , wrapMsg = ModalMsg
            }
            [ Modal.onlyFocusableElementView
                (\{ onlyFocusableElement } ->
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
                )
            ]
            state

    subscriptions : Modal.Model -> Sub Msg
    subscriptions state =
        Modal.subscriptions state

    view init
    --> text ""  -- a closed modal
```


## State and updates

@docs Model, init
@docs Msg, update, subscriptions

@docs open, close


## Views


### Modals

@docs info, warning


### View containers

@docs viewContent, viewFooter


### Attributes

@docs Attribute
@docs multipleFocusableElementView, onlyFocusableElementView
@docs autofocusOnLastElement


## X icon

@docs closeButton

-}

import Accessibility.Modal.Copy as Modal
import Accessibility.Style
import Accessibility.Styled as Html exposing (..)
import Accessibility.Styled.Widget as Widget
import Color
import Color.Transparent
import Css
import Css.Global
import Html as Root
import Html.Attributes exposing (style)
import Html.Styled.Attributes as Attributes exposing (css)
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
info :
    { visibleTitle : Bool
    , title : String
    , wrapMsg : Msg -> msg
    }
    -> List (Modal.Attribute msg)
    -> Model
    -> Html msg
info config model =
    view { overlayColor = Colors.navy, titleColor = Colors.navy } config model


{-| -}
warning :
    { visibleTitle : Bool
    , title : String
    , wrapMsg : Msg -> msg
    }
    -> List (Modal.Attribute msg)
    -> Model
    -> Html msg
warning config model =
    view { overlayColor = Colors.gray20, titleColor = Colors.red } config model


{-| -}
type alias Attribute msg =
    Modal.Attribute msg


{-| -}
autofocusOnLastElement : Modal.Attribute msg
autofocusOnLastElement =
    Modal.autofocusOnLastElement


{-| -}
multipleFocusableElementView :
    ({ firstFocusableElement : List (Html.Attribute msg)
     , lastFocusableElement : List (Html.Attribute msg)
     , autofocusElement : Html.Attribute msg
     }
     -> Html msg
    )
    -> Modal.Attribute msg
multipleFocusableElementView =
    Modal.multipleFocusableElementView


{-| -}
onlyFocusableElementView : (List (Html.Attribute msg) -> Html msg) -> Modal.Attribute msg
onlyFocusableElementView =
    Modal.onlyFocusableElementView


view :
    { overlayColor : Css.Color, titleColor : Css.Color }
    ->
        { visibleTitle : Bool
        , title : String
        , wrapMsg : Msg -> msg
        }
    -> List (Modal.Attribute msg)
    -> Model
    -> Html msg
view { overlayColor, titleColor } config attributes model =
    Modal.view config.wrapMsg
        config.title
        ([ Modal.overlayColor (Nri.Ui.Colors.Extra.withAlpha 0.9 overlayColor)
         , Modal.titleStyles
            (if config.visibleTitle then
                titleStyles titleColor

             else
                invisibleTitleStyles
            )
         , Modal.custom modalStyles
         ]
            ++ attributes
        )
        model
        |> List.singleton
        |> div [ css [ Css.position Css.relative, Css.zIndex (Css.int 1) ] ]


modalStyles : List Css.Style
modalStyles =
    [ Css.property "width" "600px"
    , Css.property "padding" "40px 0 40px 0"
    , Css.property "margin" "75px auto"
    , Css.property "background-color" ((Color.toRGBString << Nri.Ui.Colors.Extra.fromCssColor) Colors.white)
    , Css.property "border-radius" "20px"
    , Css.property "box-shadow" "0 1px 10px 0 rgba(0, 0, 0, 0.35)"
    , Css.property "position" "relative" -- required for closeButtonContainer
    ]


titleStyles : Css.Color -> List Css.Style
titleStyles color =
    [ Fonts.baseFont
    , Css.property "font-weight" "700"
    , Css.property "line-height" "27px"
    , Css.property "margin" "0 49px"
    , Css.property "font-size" "20px"
    , Css.property "text-align" "center"
    , Css.property "color" ((Color.toRGBString << Nri.Ui.Colors.Extra.fromCssColor) color)
    ]


invisibleTitleStyles : List Css.Style
invisibleTitleStyles =
    [ Css.property "property" "clip rect(1px, 1px, 1px, 1px)"
    , Css.property "position" "absolute"
    , Css.property "height" "1px"
    , Css.property "width" "1px"
    , Css.property "overflow" "hidden"
    , Css.property "margin" "-1px"
    , Css.property "padding" "0"
    , Css.property "border" "0"
    ]


{-| -}
viewContent : List (Html msg) -> Html msg
viewContent =
    Nri.Ui.styled div
        "modal-content"
        [ Css.overflowY Css.auto
        , Css.minHeight (Css.px 150)
        , Css.maxHeight (Css.calc (Css.vh 100) Css.minus (Css.px 360))
        , Css.padding2 (Css.px 30) (Css.px 40)
        , Css.width (Css.pct 100)
        , Css.boxSizing Css.borderBox

        -- Shadows for indicating that the content is scrollable
        , Css.property "background"
            """
            /* TOP shadow */

            top linear-gradient(to top, rgb(255, 255, 255), rgb(255, 255, 255)) local,
            top linear-gradient(to top, rgba(255, 255, 255, 0), rgba(0, 0, 0, 0.15)) scroll,

            /* BOTTOM shadow */

            bottom linear-gradient(to bottom, rgb(255, 255, 255), rgb(255, 255, 255)) local,
            bottom linear-gradient(to bottom, rgba(255, 255, 255, 0), rgba(0, 0, 0, 0.15)) scroll
            """
        , Css.backgroundSize2 (Css.pct 100) (Css.px 10)
        , Css.backgroundRepeat Css.noRepeat
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
closeButton : (Msg -> msg) -> List (Html.Attribute msg) -> Html msg
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
            :: Attributes.map wrapMsg (onClick Modal.close)
            :: focusableElementAttrs
        )
        [ Nri.Ui.Svg.V1.toHtml Nri.Ui.SpriteSheet.xSvg
        ]
