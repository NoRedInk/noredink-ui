module Nri.Ui.Modal.V11 exposing
    ( view, closeButton
    , Model, init, open, close
    , Msg, update, subscriptions
    , Attribute
    , info, warning
    , showTitle, hideTitle
    , custom, css
    , isOpen
    , closeButtonId, firstFocusable, lastFocusable, onlyFocusable
    )

{-| Changes from V10:

  - remove `initOpen`
  - change `open`, `close` to return `(Model, Cmd Msg)` rather than `Msg`
  - make info and warning themes
  - adds `custom` helper for adding arbitrary html attributes (primarily useful to make limiting the scope of selectors in tests easier by adding ids to modals)
  - tab and tabback events stop propagation and prevent default

```
import Browser exposing (Program, element)
import Browser.Dom as Dom
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (id)
import Html.Styled.Events as Events
import Nri.Ui.Modal.V11 as Modal
import Task

main : Program flags model msg
main =
    { init = \_ -> init
    , view = view
    , update = update
    , subscriptions = \model -> Modal.subscriptions model.modalState
    }

type ModalKind
    = FirstKindOfModal
    | SecondKindOfModal

type alias Model =
    { modal : ModalKind
    , modalState : Modal.Model
    }

init : Model
init =
    let
        ( modalState, cmd ) =
            -- When we load the page with a modal already open, we should return
            -- the focus someplace sensible when the modal closes.
            -- [This article](https://developer.paciellogroup.com/blog/2018/06/the-current-state-of-modal-dialog-accessibility/) recommends
            -- focusing the main or body.
            Modal.open
                { startFocusOn = Modal.closeButtonId
                , returnFocusTo = "maincontent"
                }
    in
    ( { modal = FirstKindOfModal
      , modalState = modalState
      }
    , Cmd.map ModalMsg cmd
    )

type Msg
    = OpenModal ModalKind String
    | ModalMsg Modal.Msg
    | CloseModal
    | Focus String
    | Focused (Result Dom.Error ())

update : Msg -> Model -> ( Modal, Cmd Msg )
update msg model =
    case msg of
        OpenModal modalKind returnFocusTo ->
            let
                ( modalState, cmd ) =
                    Modal.open
                        { startFocusOn = Modal.closeButtonId
                        , returnFocusTo = returnFocusTo
                        }
            in
            ( { modal = modalKind
              , modalState = modalState
              }
            , Cmd.map ModalMsg cmd
            )

        ModalMsg modalMsg ->
            let
                ( modalState, cmd ) =
                    Modal.update
                        { dismissOnEscAndOverlayClick = True }
                        modalMsg
                        model.modalState
            in
            ( { model | modalState = modalState }
            , Cmd.map ModalMsg cmd
            )

        CloseModal ->
            let
                ( modalState, cmd ) =
                    Modal.close model.modalState
            in
            ( { model | modalState = modalState }
            , Cmd.map ModalMsg cmd
            )

        Focus id ->
            ( model, Task.attempt Focused (Dom.focus id) )

        Focused _ ->
            ( model, Cmd.none )

view : Model -> Html Msg
view model =
    main_ [ id "maincontent" ]
        [ button
            [ id "open-first-kind-of-modal-button"
            , Events.onClick (OpenModal FirstKindOfModal "open-first-kind-of-modal-button")
            ]
            [ text "Open FirstKindOfModal" ]
        , button
            [ id "open-second-kind-of-modal-button"
            , Events.onClick (OpenModal SecondKindOfModal "open-second-kind-of-modal-button")
            ]
            [ text "Open SecondKindOfModal" ]
        , case model.modal of
            FirstKindOfModal ->
                Modal.view
                    { title = "First kind of modal"
                    , wrapMsg = ModalMsg
                    , content =
                        [ Modal.closeButton ModalMsg <|
                            Modal.firstFocusable { focusLastId = Focus "last-element-id" }
                        , text "Modal Content"
                        ]
                    , footer =
                        [ button
                            (Events.onClick CloseModal
                                :: id "last-element-id"
                                :: Modal.lastFocusable { focusFirstElement = Focus Modal.closeButtonId }
                            )
                            [ text "Close" ]
                        ]
                    }
                    [ Modal.hideTitle
                    , Modal.css [ padding (px 10) ]
                    , Modal.custom [ id "first-modal" ]
                    ]
                    model.modalState

            SecondKindOfModal ->
                Modal.view
                    { title = "Second kind of modal"
                    , wrapMsg = ModalMsg
                    , content =
                        [ Modal.closeButton ModalMsg <|
                            Modal.onlyFocusable { focusSelf = Focus Modal.closeButtonId }
                        , text "Modal Content"
                        ]
                    , footer = []
                    }
                    [ Modal.warning
                    ]
                    model.modalState
        ]
```

@docs view, closeButton
@docs Model, init, open, close
@docs Msg, update, subscriptions


### Attributes

@docs Attribute
@docs info, warning
@docs showTitle, hideTitle
@docs custom, css


### State checks

@docs isOpen

-}

import Accessibility.Styled as Html exposing (..)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Widget as Widget
import Browser
import Browser.Dom as Dom
import Browser.Events
import Color.Transparent as Transparent
import Css exposing (..)
import Css.Transitions
import Html.Styled as Root
import Html.Styled.Attributes as Attrs exposing (id)
import Html.Styled.Events as Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.SpriteSheet
import Nri.Ui.Svg.V1
import Task


{-| -}
type Model
    = Opened String
    | Closed


{-| -}
init : Model
init =
    Closed


{-| Pass the id of the element that should receive focus when the modal closes.

> ...if a dialog was opened on page load, then focus could be placed on either the body or main element.
> If the trigger was removed from the DOM, then placing focus as close to the trigger’s DOM location would be ideal.

<https://developer.paciellogroup.com/blog/2018/06/the-current-state-of-modal-dialog-accessibility/>

-}
open : { startFocusOn : String, returnFocusTo : String } -> ( Model, Cmd Msg )
open { startFocusOn, returnFocusTo } =
    ( Opened returnFocusTo
    , Task.attempt Focused (Dom.focus startFocusOn)
    )


{-| -}
close : Model -> ( Model, Cmd Msg )
close model =
    case model of
        Opened returnFocusTo ->
            ( Closed, Task.attempt Focused (Dom.focus returnFocusTo) )

        Closed ->
            ( Closed, Cmd.none )


{-| -}
isOpen : Model -> Bool
isOpen model =
    case model of
        Opened _ ->
            True

        Closed ->
            False


type By
    = EscapeKey
    | OverlayClick
    | Other


{-| -}
type Msg
    = CloseModal By
    | Focus String
    | Focused (Result Dom.Error ())


{-| Include the subscription if you want the modal to dismiss on `Esc`.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Opened _ ->
            Browser.Events.onKeyDown (Key.escape (CloseModal EscapeKey))

        Closed ->
            Sub.none


{-| -}
update : { dismissOnEscAndOverlayClick : Bool } -> Msg -> Model -> ( Model, Cmd Msg )
update { dismissOnEscAndOverlayClick } msg model =
    case msg of
        CloseModal by ->
            case by of
                Other ->
                    close model

                _ ->
                    if dismissOnEscAndOverlayClick then
                        close model

                    else
                        ( model, Cmd.none )

        Focus id ->
            ( model, Task.attempt Focused (Dom.focus id) )

        Focused _ ->
            -- TODO: consider adding error handling when we didn't successfully
            -- fous an element
            ( model, Cmd.none )


type Autofocus
    = Default
    | Last



-- ATTRIBUTES


{-| This is the default theme.
-}
info : Attribute
info =
    Batch []


{-| -}
warning : Attribute
warning =
    Batch
        [ overlayColor (Nri.Ui.Colors.Extra.withAlpha 0.9 Colors.gray20)
        , titleColor Colors.red
        ]


{-| This is the default setting.
-}
showTitle : Attribute
showTitle =
    Attribute (\attrs -> { attrs | visibleTitle = True })


{-| -}
hideTitle : Attribute
hideTitle =
    Attribute (\attrs -> { attrs | visibleTitle = False })


{-| Do NOT use this function for attaching styles -- use the `css` helper instead.

    import Html.Styled.Attribute exposing (id)

    Modal.view
        { title = "Some Great Modal"
        , wrapMsg = ModalMsg
        , content = []
        , footer = []
        }
        [ Modal.custom [ id "my-modal" ]]
        modalState

-}
custom : List (Html.Attribute Never) -> Attribute
custom customAttributes =
    Attribute
        (\attrs ->
            { attrs
                | customAttributes =
                    List.append attrs.customAttributes customAttributes
            }
        )


{-| -}
css : List Style -> Attribute
css styles =
    Attribute
        (\attrs ->
            { attrs
                | customStyles =
                    List.append attrs.customStyles styles
            }
        )


{-| -}
type Attribute
    = Attribute (Attributes -> Attributes)
    | Batch (List Attribute)



-- ATTRIBUTES (INTERNAL)


type alias Attributes =
    { overlayColor : Color
    , titleColor : Color
    , visibleTitle : Bool
    , customStyles : List Style
    , customAttributes : List (Html.Attribute Never)
    }


defaultAttributes : Attributes
defaultAttributes =
    { overlayColor = Nri.Ui.Colors.Extra.withAlpha 0.9 Colors.navy
    , titleColor = Colors.navy
    , visibleTitle = True
    , customStyles = []
    , customAttributes = []
    }


titleColor : Color -> Attribute
titleColor color =
    Attribute (\attrs -> { attrs | titleColor = color })


overlayColor : Color -> Attribute
overlayColor color =
    Attribute (\attrs -> { attrs | overlayColor = color })


buildAttributes : List Attribute -> Attributes
buildAttributes attrs =
    let
        applyAttrs attribute acc =
            case attribute of
                Attribute fun ->
                    fun acc

                Batch functions ->
                    List.foldl applyAttrs acc functions
    in
    List.foldl applyAttrs defaultAttributes attrs


modalStyles : List Style
modalStyles =
    [ position relative

    -- Border
    , borderRadius (px 20)
    , boxShadow5 zero (px 1) (px 10) zero (rgba 0 0 0 0.35)

    -- Spacing
    , margin2 (px 50) auto

    -- Size
    , minHeight (vh 40)
    , width (px 600)
    , backgroundColor Colors.white

    -- the modal should grow up to the viewport minus a 50px margin
    , maxHeight (calc (pct 100) minus (px 100))
    ]


titleStyles : Color -> Bool -> List Style
titleStyles color visibleTitle =
    if visibleTitle then
        [ Fonts.baseFont
        , Css.fontWeight (Css.int 700)
        , Css.paddingTop (Css.px 40)
        , Css.paddingBottom (Css.px 20)
        , Css.margin Css.zero
        , Css.fontSize (Css.px 20)
        , Css.textAlign Css.center
        , Css.color color
        ]

    else
        [ -- https://snook.ca/archives/html_and_css/hiding-content-for-accessibility
          Css.property "clip" "rect(1px, 1px, 1px, 1px)"
        , Css.position Css.absolute
        , Css.height (Css.px 1)
        , Css.width (Css.px 1)
        , Css.overflow Css.hidden
        , Css.margin (Css.px -1)
        , Css.padding Css.zero
        , Css.border Css.zero
        ]



-- VIEW


{-| -}
view :
    { title : String
    , wrapMsg : Msg -> msg
    , content : List (Html msg)
    , footer : List (Html msg)
    }
    -> List Attribute
    -> Model
    -> Html msg
view config attrsList model =
    let
        attrs =
            buildAttributes attrsList
    in
    case model of
        Opened _ ->
            div
                [ Attrs.css
                    [ position fixed
                    , top zero
                    , left zero
                    , width (pct 100)
                    , height (pct 100)
                    , displayFlex
                    , alignItems center
                    ]
                ]
                [ viewBackdrop config.wrapMsg attrs.overlayColor
                , div [ Attrs.css (List.append modalStyles attrs.customStyles) ]
                    [ viewModal
                        { title = config.title
                        , titleColor = attrs.titleColor
                        , visibleTitle = attrs.visibleTitle
                        , customAttributes = attrs.customAttributes
                        , content = config.content
                        , footer = config.footer
                        }
                    ]
                , Root.node "style" [] [ Root.text "body {overflow: hidden;} " ]
                ]
                |> List.singleton
                |> div [ Attrs.css [ Css.position Css.relative, Css.zIndex (Css.int 1) ] ]

        Closed ->
            text ""


viewBackdrop : (Msg -> msg) -> Color -> Html msg
viewBackdrop wrapMsg color =
    Root.div
        -- We use Root html here in order to allow clicking to exit out of
        -- the overlay. This behavior is available to non-mouse users as
        -- well via the ESC key, so imo it's fine to have this div
        -- be clickable but not focusable.
        [ Attrs.css
            [ position absolute
            , width (pct 100)
            , height (pct 100)
            , backgroundColor color
            ]
        , onClick (wrapMsg (CloseModal OverlayClick))
        ]
        []


modalTitleId : String
modalTitleId =
    "modal__title"


viewModal :
    { title : String
    , titleColor : Color
    , visibleTitle : Bool
    , customAttributes : List (Html.Attribute Never)
    , content : List (Html msg)
    , footer : List (Html msg)
    }
    -> Html msg
viewModal config =
    section
        ([ Role.dialog
         , Widget.modal True
         , Aria.labeledBy modalTitleId
         ]
            ++ config.customAttributes
        )
        [ h1
            [ id modalTitleId
            , Attrs.css (titleStyles config.titleColor config.visibleTitle)
            ]
            [ text config.title ]
        , div
            []
            [ viewInnerContent config.content config.visibleTitle (not (List.isEmpty config.footer))
            , viewFooter config.footer
            ]
        ]


{-| -}
onlyFocusable : { focusSelf : msg } -> List (Html.Attribute msg)
onlyFocusable { focusSelf } =
    [ onKeyDownPreventDefault
        [ Key.tab focusSelf
        , Key.tabBack focusSelf
        ]
    , Attrs.class "modal__only-focusable-element"
    ]


{-| -}
firstFocusable : { focusLastId : msg } -> List (Html.Attribute msg)
firstFocusable { focusLastId } =
    [ onKeyDownPreventDefault [ Key.tabBack focusLastId ]
    , Attrs.class "modal__first-focusable-element"
    ]


{-| -}
lastFocusable : { focusFirstId : msg } -> List (Html.Attribute msg)
lastFocusable { focusFirstId } =
    [ onKeyDownPreventDefault [ Key.tab focusFirstId ]
    , Attrs.class "modal__last-focusable-element"
    ]


onKeyDownPreventDefault : List (Decoder msg) -> Html.Attribute msg
onKeyDownPreventDefault decoders =
    Events.preventDefaultOn "keydown"
        (Decode.oneOf (List.map (Decode.map (\msg -> ( msg, True ))) decoders))


{-| -}
viewInnerContent : List (Html msg) -> Bool -> Bool -> Html msg
viewInnerContent children visibleTitle visibleFooter =
    let
        titleHeight =
            if visibleTitle then
                45

            else
                0

        footerHeight =
            if visibleFooter then
                180

            else
                0

        modalTitleStyles =
            if visibleTitle then
                []

            else
                [ Css.borderTopLeftRadius (Css.px 20)
                , Css.borderTopRightRadius (Css.px 20)
                , Css.overflowY Css.hidden
                ]

        modalFooterStyles =
            if visibleFooter then
                []

            else
                [ Css.borderBottomLeftRadius (Css.px 20)
                , Css.borderBottomRightRadius (Css.px 20)
                , Css.overflowY Css.hidden
                ]
    in
    div
        [ Attrs.css (modalTitleStyles ++ modalFooterStyles)
        ]
        [ div
            [ Attrs.css
                [ Css.overflowY Css.auto
                , Css.overflowX Css.hidden
                , Css.minHeight (Css.px 150)
                , Css.maxHeight
                    (Css.calc (Css.vh 100)
                        Css.minus
                        (Css.px (footerHeight + titleHeight + 145))
                    )
                , Css.width (Css.pct 100)
                , Css.boxSizing Css.borderBox
                , Css.paddingLeft (Css.px 40)
                , Css.paddingRight (Css.px 40)
                , if visibleTitle then
                    Css.paddingTop Css.zero

                  else
                    Css.paddingTop (Css.px 40)
                , if visibleFooter then
                    Css.paddingBottom Css.zero

                  else
                    Css.paddingBottom (Css.px 40)
                , if visibleFooter then
                    shadow (Transparent.customOpacity 0.15) (Css.px 16)

                  else
                    shadow (Transparent.customOpacity 0.4) (Css.px 30)
                ]
            ]
            children
        ]


shadow : Transparent.Opacity -> Css.Px -> Css.Style
shadow opacity bottomShadowHeight =
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
        , Css.backgroundSize2 (Css.pct 100) bottomShadowHeight
        , Css.backgroundRepeat Css.noRepeat
        ]


{-| -}
viewFooter : List (Html msg) -> Html msg
viewFooter children =
    if List.isEmpty children then
        Html.text ""

    else
        div
            [ Attrs.css
                [ Css.alignItems Css.center
                , Css.displayFlex
                , Css.flexDirection Css.column
                , Css.flexGrow (Css.int 2)
                , Css.flexWrap Css.noWrap
                , Css.margin4 (Css.px 20) Css.zero Css.zero Css.zero
                , Css.paddingBottom (Css.px 40)
                , Css.width (Css.pct 100)
                ]
            ]
            children



--BUTTONS


{-| -}
closeButtonId : String
closeButtonId =
    "modal__close-button-x"


{-| -}
closeButton : (Msg -> msg) -> List (Html.Attribute msg) -> Html msg
closeButton wrapMsg attrs =
    button
        (Widget.label "Close modal"
            :: Attrs.map wrapMsg (onClick (CloseModal Other))
            :: Attrs.css
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
                , Css.Transitions.transition [ Css.Transitions.color 0.1 ]
                ]
            :: Attrs.id closeButtonId
            :: attrs
        )
        [ Nri.Ui.Svg.V1.toHtml Nri.Ui.SpriteSheet.xSvg
        ]
