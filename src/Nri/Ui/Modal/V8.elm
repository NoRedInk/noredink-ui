module Nri.Ui.Modal.V8 exposing
    ( Model, init
    , Msg, update, subscriptions
    , open, close
    , info, warning
    , viewContent, viewFooterlessContent, viewFooter
    , Attribute
    , multipleFocusableElementView, onlyFocusableElementView
    , autofocusOnLastElement
    , closeButton
    )

{-| Changes from V7:

  - More customizable attributes

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
            , wrapMsg = ModalMsg
            }
            [ Modal.visibleTitle
            , Modal.onlyFocusableElementView
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
@docs view


### View containers

@docs viewContent, viewFooterlessContent, viewFooter


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
import Color.Transparent as Transparent
import Css
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
    { title : String
    , visibleTitle : Bool
    , wrapMsg : Msg -> msg
    }
    -> List (Attribute msg)
    -> Model
    -> Html msg
info config model =
    view Info config model


{-| -}
warning :
    { title : String
    , visibleTitle : Bool
    , wrapMsg : Msg -> msg
    }
    -> List (Attribute msg)
    -> Model
    -> Html msg
warning config model =
    view Warning config model


type Theme
    = Info
    | Warning


themeToOverlayColor : Theme -> Css.Color
themeToOverlayColor theme =
    case theme of
        Info ->
            Colors.navy

        Warning ->
            Colors.gray20


themeToTitleColor : Theme -> Css.Color
themeToTitleColor theme =
    case theme of
        Info ->
            Colors.navy

        Warning ->
            Colors.red


{-| -}
type Attribute msg
    = Attribute (Modal.Attribute msg)
    | WithTitleVisibility (Bool -> Modal.Attribute msg)


{-| -}
autofocusOnLastElement : Attribute msg
autofocusOnLastElement =
    Attribute Modal.autofocusOnLastElement


{-| -}
multipleFocusableElementView :
    ({ firstFocusableElement : List (Html.Attribute msg)
     , lastFocusableElement : List (Html.Attribute msg)
     , autofocusElement : Html.Attribute msg
     }
     -> Bool
     -> Html msg
    )
    -> Attribute msg
multipleFocusableElementView f =
    WithTitleVisibility (\visibleTitle -> Modal.multipleFocusableElementView (\attributes -> f attributes visibleTitle))


{-| -}
onlyFocusableElementView : (List (Html.Attribute msg) -> Bool -> Html msg) -> Attribute msg
onlyFocusableElementView f =
    WithTitleVisibility (\visibleTitle -> Modal.onlyFocusableElementView (\attributes -> f attributes visibleTitle))


view :
    Theme
    -> { title : String, visibleTitle : Bool, wrapMsg : Msg -> msg }
    -> List (Attribute msg)
    -> Model
    -> Html msg
view theme config attributes model =
    Modal.view
        config.wrapMsg
        config.title
        ([ Modal.overlayColor (Nri.Ui.Colors.Extra.withAlpha 0.9 (themeToOverlayColor theme))
         , Modal.custom
            [ Css.width (Css.px 600)
            , Css.margin2 (Css.px 50) Css.auto
            , Css.paddingTop (Css.px 40)
            , Css.borderRadius (Css.px 20)
            , Css.property "box-shadow" "0 1px 10px 0 rgba(0, 0, 0, 0.35)"
            , Css.backgroundColor Colors.white
            ]
         , if config.visibleTitle then
            Modal.titleStyles
                [ Fonts.baseFont
                , Css.fontWeight (Css.int 700)
                , Css.paddingBottom (Css.px 20)
                , Css.margin Css.zero
                , Css.fontSize (Css.px 20)
                , Css.textAlign Css.center
                , Css.color (themeToTitleColor theme)
                ]

           else
            Modal.titleStyles
                [ Css.property "property" "clip rect(1px, 1px, 1px, 1px)"
                , Css.property "position" "absolute"
                , Css.property "height" "1px"
                , Css.property "width" "1px"
                , Css.property "overflow" "hidden"
                , Css.property "margin" "-1px"
                , Css.property "padding" "0"
                , Css.property "border" "0"
                ]
         ]
            ++ List.map
                (\attribute ->
                    case attribute of
                        Attribute a ->
                            a

                        WithTitleVisibility f ->
                            f config.visibleTitle
                )
                attributes
        )
        model
        |> List.singleton
        |> div [ css [ Css.position Css.relative, Css.zIndex (Css.int 1) ] ]


{-| -}
viewContent : List (Html msg) -> Bool -> Html msg
viewContent children visibleTitle =
    let
        extraHeight =
            Css.px
                (180
                    -- footer height
                    + (if visibleTitle then
                        90

                       else
                        45
                       -- title height
                      )
                    + 100
                 -- desired surrounding whitespace
                )
    in
    div []
        [ Nri.Ui.styled div
            "modal-content"
            [ Css.overflowY Css.auto
            , Css.minHeight (Css.px 150)
            , Css.padding2 (Css.px 10) (Css.px 40)
            , Css.width (Css.pct 100)
            , Css.boxSizing Css.borderBox
            , shadow (Transparent.customOpacity 0.15)
            ]
            [ css [ Css.maxHeight (Css.calc (Css.vh 100) Css.minus extraHeight) ] ]
            children
        ]


{-| -}
viewFooterlessContent : List (Html msg) -> Bool -> Html msg
viewFooterlessContent children visibleTitle =
    let
        extraHeight =
            Css.px
                (0
                    -- footer height
                    + (if visibleTitle then
                        90

                       else
                        45
                       -- title height
                      )
                    + 100
                 -- desired surrounding whitespace
                )
    in
    div
        [ css
            [ Css.borderBottomLeftRadius (Css.px 20)
            , Css.borderBottomRightRadius (Css.px 20)
            , Css.overflowY Css.hidden
            ]
        ]
        [ Nri.Ui.styled div
            "footerless-modal-content"
            [ Css.overflowY Css.auto
            , Css.minHeight (Css.px 150)
            , Css.padding2 (Css.px 10) (Css.px 40)
            , Css.width (Css.pct 100)
            , Css.boxSizing Css.borderBox
            , shadow (Transparent.customOpacity 0.4)
            ]
            [ css [ Css.maxHeight (Css.calc (Css.vh 100) Css.minus extraHeight) ] ]
            children
        ]


shadow : Transparent.Opacity -> Css.Style
shadow opacity =
    let
        to =
            Transparent.fromRGBA { red = 0, green = 0, blue = 0, alpha = opacity }
                |> Transparent.toRGBAString
    in
    Css.batch
        [ -- Shadows for indicating that the content is scrollable
          [ "/* TOP shadow */"
          , "top linear-gradient(to top, rgb(255, 255, 255), rgb(255, 255, 255)) local,"
          , "top linear-gradient(to top, rgba(255, 255, 255, 0), rgba(0, 0, 0, 0.15)) scroll,"
          , ""
          , "/* BOTTOM shadow */"
          , "bottom linear-gradient(to bottom, rgb(255, 255, 255), rgb(255, 255, 255)) local,"
          , "bottom linear-gradient(to bottom, rgba(255, 255, 255, 0), " ++ to ++ ") scroll"
          ]
            |> String.join "\n"
            |> Css.property "background"
        , Css.backgroundSize2 (Css.pct 100) (Css.px 10)
        , Css.backgroundRepeat Css.noRepeat
        ]


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
        , Css.paddingBottom (Css.px 40)
        , Css.width (Css.pct 100)
        ]
        []



--BUTTONS


{-| -}
closeButton : (Msg -> msg) -> List (Html.Attribute msg) -> Html msg
closeButton wrapMsg focusableElementAttrs =
    Nri.Ui.styled button
        "close-button-container"
        [ -- in the upper-right corner of the modal
          Css.position Css.absolute
        , Css.top Css.zero
        , Css.right Css.zero

        -- make the hitspace extend all the way to the corner
        , Css.width (Css.px 40)
        , Css.height (Css.px 40)
        , Css.padding4 (Css.px 20) (Css.px 20) Css.zero Css.zero

        -- apply button styles
        , Css.borderWidth Css.zero
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
