module Nri.Ui.Modal.V11 exposing
    ( view, closeButton, closeButtonId
    , Model, init, open, close
    , Msg, update, subscriptions
    , Attribute
    , info, warning
    , showTitle, hideTitle
    , testId, css, custom
    , isOpen
    )

{-|


# Patch changes:

  - adds `testId` helper
  - adds data-nri-descriptions to the header, content, and footer
  - use `Shadows`


# Changes from V10:

  - remove `initOpen`
  - change `open`, `close` to return `(Model, Cmd Msg)` rather than `Msg`
  - make info and warning themes
  - adds `custom` helper for adding arbitrary html attributes (primarily useful to make limiting the scope of selectors in tests easier by adding ids to modals)
  - tab and tabback events stop propagation and prevent default

```
import Browser exposing (element)
import Browser.Dom as Dom
import Css exposing (padding, px)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (id)
import Html.Styled.Events as Events
import Nri.Ui.FocusTrap.V1 as FocusTrap exposing (FocusTrap)
import Nri.Ui.Modal.V11 as Modal
import Task

main : Program () Modal.Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = toUnstyled << view
        , update = update
        , subscriptions = \model -> Sub.map ModalMsg (Modal.subscriptions model)
        }

init : ( Modal.Model, Cmd Msg )
init =
    let
        ( model, cmd ) =
            -- When we load the page with a modal already open, we should return
            -- the focus someplace sensible when the modal closes.
            -- [This article](https://developer.paciellogroup.com/blog/2018/06/the-current-state-of-modal-dialog-accessibility/) recommends
            -- focusing the main or body.
            Modal.open
                { startFocusOn = Modal.closeButtonId
                , returnFocusTo = "maincontent"
                }
    in
    ( model, Cmd.map ModalMsg cmd )

type Msg
    = OpenModal String
    | ModalMsg Modal.Msg
    | CloseModal
    | Focus String
    | Focused (Result Dom.Error ())

update : Msg -> Modal.Model -> ( Modal.Model, Cmd Msg )
update msg model =
    case msg of
        OpenModal returnFocusTo ->
            let
                ( newModel, cmd ) =
                    Modal.open
                        { startFocusOn = Modal.closeButtonId
                        , returnFocusTo = returnFocusTo
                        }
            in
            ( newModel, Cmd.map ModalMsg cmd )

        ModalMsg modalMsg ->
            let
                ( newModel, cmd ) =
                    Modal.update
                        { dismissOnEscAndOverlayClick = True }
                        modalMsg
                        model
            in
            ( newModel, Cmd.map ModalMsg cmd )

        CloseModal ->
            let
                ( newModel, cmd ) =
                    Modal.close model
            in
            ( newModel, Cmd.map ModalMsg cmd )

        Focus id ->
            ( model, Task.attempt Focused (Dom.focus id) )

        Focused _ ->
            ( model, Cmd.none )

view : Modal.Model -> Html Msg
view model =
    main_ [ id "maincontent" ]
        [ button
            [ id "open-modal"
            , Events.onClick (OpenModal "open-modal")
            ]
            [ text "Open Modal" ]
        , Modal.view
            { title = "First kind of modal"
            , wrapMsg = ModalMsg
            , content = [ text "Modal Content" ]
            , footer =
                [ button
                    [ Events.onClick CloseModal
                    , id "last-element-id"
                    ]
                    [ text "Close" ]
                ]
            , focusTrap =
                { focus = Focus
                , firstId = Modal.closeButtonId
                , lastId = "last-element-id"
                }
            }
            [ Modal.hideTitle
            , Modal.css [ padding (px 10) ]
            , Modal.custom [ id "first-modal" ]
            , Modal.closeButton
            ]
            model
        ]
```

@docs view, closeButton, closeButtonId
@docs Model, init, open, close
@docs Msg, update, subscriptions


### Attributes

@docs Attribute
@docs info, warning
@docs showTitle, hideTitle
@docs testId, css, custom


### State checks

@docs isOpen

-}

import Accessibility.Styled as Html exposing (..)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
import Browser.Dom as Dom
import Browser.Events
import Css exposing (..)
import Css.Media
import Css.Transitions
import Html.Styled as Root
import Html.Styled.Attributes as Attrs exposing (id)
import Html.Styled.Events exposing (onClick)
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusTrap.V1 as FocusTrap exposing (FocusTrap)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.MediaQuery.V1 exposing (mobile)
import Nri.Ui.Shadows.V1 as Shadows
import Nri.Ui.UiIcon.V1 as UiIcon
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


{-| -}
type Msg
    = CloseButtonClicked
    | EscOrOverlayClicked
    | Focused (Result Dom.Error ())


{-| Include the subscription if you want the modal to dismiss on `Esc`.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Opened _ ->
            Browser.Events.onKeyDown (Key.escape EscOrOverlayClicked)

        Closed ->
            Sub.none


{-| -}
update : { dismissOnEscAndOverlayClick : Bool } -> Msg -> Model -> ( Model, Cmd Msg )
update { dismissOnEscAndOverlayClick } msg model =
    case msg of
        CloseButtonClicked ->
            close model

        EscOrOverlayClicked ->
            if dismissOnEscAndOverlayClick then
                close model

            else
                ( model, Cmd.none )

        Focused _ ->
            -- TODO: consider adding error handling when we didn't successfully
            -- fous an element
            ( model, Cmd.none )



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


{-| Include the close button.
-}
closeButton : Attribute
closeButton =
    Attribute (\attrs -> { attrs | closeButton = True })


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
testId : String -> Attribute
testId id_ =
    Attribute
        (\attrs ->
            { attrs
                | customAttributes =
                    ExtraAttributes.testId id_ :: attrs.customAttributes
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
    , closeButton : Bool
    }


defaultAttributes : Attributes
defaultAttributes =
    { overlayColor = Nri.Ui.Colors.Extra.withAlpha 0.9 Colors.navy
    , titleColor = Colors.navy
    , visibleTitle = True
    , customStyles = []
    , customAttributes = []
    , closeButton = False
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
    , Shadows.high
    , Css.Media.withMedia [ mobile ]
        [ borderRadius zero
        ]

    -- Spacing
    , margin2 (px 20) auto

    -- Size
    , width (px 600)
    , backgroundColor Colors.white
    ]


titleStyles : Color -> Bool -> List Style
titleStyles color visibleTitle =
    if visibleTitle then
        [ Fonts.baseFont
        , Css.fontWeight (Css.int 700)
        , Css.padding3 (Css.px 40) (Css.px 40) (Css.px 20)
        , Css.margin Css.zero
        , Css.fontSize (Css.px 20)
        , Css.textAlign Css.center
        , Css.color color
        , Css.Media.withMedia [ mobile ]
            [ Css.padding3 (Css.px 20) (Css.px 20) Css.zero
            ]
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


{-| `FocusTrap` comes from `Nri.Ui.FocusTrap.V1`.
-}
view :
    { title : String
    , wrapMsg : Msg -> msg
    , focusTrap : FocusTrap msg
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
                        , closeButton =
                            if attrs.closeButton then
                                Just (config.wrapMsg CloseButtonClicked)

                            else
                                Nothing
                        , content = config.content
                        , footer = config.footer
                        }
                    ]
                , Root.node "style" [] [ Root.text "body {overflow: hidden;} " ]
                ]
                |> List.singleton
                |> Root.div
                    (List.concat
                        [ [ FocusTrap.toAttribute config.focusTrap ]
                        , [ Attrs.css [ Css.position Css.relative, Css.zIndex (Css.int 100) ] ]
                        ]
                    )

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
        , onClick (wrapMsg EscOrOverlayClicked)
        , ExtraAttributes.nriDescription "modal-backdrop"
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
    , closeButton : Maybe msg
    , content : List (Html msg)
    , footer : List (Html msg)
    }
    -> Html msg
viewModal config =
    section
        ([ Role.dialog
         , Aria.modal True
         , Aria.labeledBy modalTitleId
         ]
            ++ config.customAttributes
        )
        [ h1
            [ id modalTitleId
            , Attrs.css (titleStyles config.titleColor config.visibleTitle)
            , ExtraAttributes.nriDescription "modal-title"
            ]
            [ text config.title ]
        , div
            []
            [ viewInnerContent config
            , viewFooter config.footer
            ]
        ]


{-| -}
viewInnerContent :
    { config
        | content : List (Html msg)
        , visibleTitle : Bool
        , closeButton : Maybe msg
        , footer : List (Html msg)
    }
    -> Html msg
viewInnerContent ({ visibleTitle } as config) =
    let
        children =
            case config.closeButton of
                Just msg ->
                    viewCloseButton msg :: config.content

                Nothing ->
                    config.content

        visibleFooter =
            not (List.isEmpty config.footer)

        ( titleDesktopHeight, titleMobileHeight ) =
            if visibleTitle then
                ( 85, 45 )

            else
                ( 0, 0 )

        ( footerDesktopHeight, footerMobileHeight ) =
            if visibleFooter then
                ( 160, 137 )

            else
                ( 0, 0 )

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
        , ExtraAttributes.nriDescription "modal-content"
        ]
        [ div
            [ Attrs.css
                [ Css.overflowY Css.auto
                , Css.overflowX Css.hidden
                , Css.minHeight (Css.px 50)
                , Css.maxHeight
                    (Css.calc (Css.vh 100)
                        Css.minus
                        (Css.px (footerDesktopHeight + titleDesktopHeight + 40))
                    )
                , Css.width (Css.pct 100)
                , Css.boxSizing Css.borderBox
                , Css.paddingLeft (Css.px 40)
                , Css.paddingRight (Css.px 40)
                , Css.Media.withMedia [ mobile ]
                    [ Css.padding (Css.px 20)
                    , Css.maxHeight
                        (Css.calc (Css.vh 100)
                            Css.minus
                            (Css.px (footerMobileHeight + titleMobileHeight + 40))
                        )
                    ]
                , if visibleTitle then
                    Css.paddingTop Css.zero

                  else
                    Css.paddingTop (Css.px 40)
                , if visibleFooter then
                    Css.paddingBottom (Css.px 30)

                  else
                    Css.paddingBottom (Css.px 40)
                ]
            ]
            children
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
                , Css.padding2 (Css.px 30) Css.zero
                , Css.backgroundColor Colors.gray96
                , Css.borderTop3 (Css.px 1) Css.solid Colors.gray92
                , Css.borderRadius4 Css.zero Css.zero (Css.px 20) (Css.px 20)
                , Css.Media.withMedia [ mobile ]
                    [ Css.padding (Css.px 20)
                    ]
                ]
            , ExtraAttributes.nriDescription "modal-footer"
            ]
            children



--BUTTONS


{-| -}
closeButtonId : String
closeButtonId =
    "modal__close-button-x"


{-| -}
viewCloseButton : msg -> Html msg
viewCloseButton closeModal =
    ClickableSvg.button "Close modal"
        UiIcon.x
        [ ClickableSvg.id closeButtonId
        , ClickableSvg.onClick closeModal
        , -- TODO: trim down the unnecessary styles
          ClickableSvg.css
            [ -- in the upper-right corner of the modal
              Css.position Css.absolute
            , Css.top Css.zero
            , Css.right Css.zero

            -- make appear above lesson content
            , Css.backgroundColor (rgba 255 255 255 0.5)
            , Css.borderRadius (pct 50)

            -- make the hitspace extend all the way around x
            , Css.width (Css.px 60)
            , Css.height (Css.px 60)
            , Css.padding (Css.px 20)

            -- apply button styles
            , Css.borderWidth Css.zero
            , Css.cursor Css.pointer
            , Css.color Colors.azure
            , Css.hover [ Css.color Colors.azureDark ]
            , Css.Transitions.transition [ Css.Transitions.color 0.1 ]
            ]
        ]
