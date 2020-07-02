module Nri.Ui.Modal.V9 exposing
    ( Model, init
    , Msg, update, subscriptions
    , open, close
    , info, warning
    , multipleFocusableElementView, onlyFocusableElementView
    )

{-| Changes from V8:

  - enable checks against the state (e.g., is the modal open or not?)


## State and updates

@docs Model, init
@docs Msg, update, subscriptions

@docs open, close


## Views

@docs info, warning


### Focusable

@docs multipleFocusableElementView, onlyFocusableElementView

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
import Html.Styled.Attributes as Attributes exposing (css, id)
import Html.Styled.Events exposing (onClick)
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


type By
    = EscapeKey
    | OverlayClick
    | Other


{-| -}
type Msg
    = OpenModal String
    | CloseModal By
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
        OpenModal returnFocusTo ->
            ( Opened returnFocusTo
            , Dom.focus autofocusId
                |> Task.onError (\_ -> Dom.focus firstId)
                |> Task.attempt Focused
            )

        CloseModal by ->
            let
                closeModal returnFocusTo =
                    ( Closed, Task.attempt Focused (Dom.focus returnFocusTo) )
            in
            case ( model, by, dismissOnEscAndOverlayClick ) of
                ( Opened returnFocusTo, _, True ) ->
                    closeModal returnFocusTo

                ( Opened returnFocusTo, Other, False ) ->
                    closeModal returnFocusTo

                _ ->
                    ( model, Cmd.none )

        Focus id ->
            ( model, Task.attempt Focused (Dom.focus id) )

        Focused _ ->
            ( model, Cmd.none )


type Autofocus
    = Default
    | Last


{-| Pass the id of the element that should receive focus when the modal closes.
-}
open : String -> Msg
open =
    OpenModal


{-| -}
close : Msg
close =
    CloseModal Other


{-| -}
info :
    { visibleTitle : Bool
    , title : String
    , wrapMsg : Msg -> msg
    }
    ->
        ({ viewContent : { content : List (Html msg), footer : List (Html msg) } -> Html msg
         , closeButton : List (Html.Attribute msg) -> Html msg
         }
         -> Attribute msg
        )
    -> Model
    -> Html msg
info config getFocusable model =
    view Info config getFocusable model


{-| -}
warning :
    { visibleTitle : Bool
    , title : String
    , wrapMsg : Msg -> msg
    }
    ->
        ({ viewContent : { content : List (Html msg), footer : List (Html msg) } -> Html msg
         , closeButton : List (Html.Attribute msg) -> Html msg
         }
         -> Attribute msg
        )
    -> Model
    -> Html msg
warning config getFocusable model =
    view Warning config getFocusable model


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



-- ATTRIBUTES


type alias Config msg =
    { overlayColor : Color
    , wrapMsg : Msg -> msg
    , modalStyle : Style
    , titleString : String
    , titleStyles : List Style
    , autofocusOn : Autofocus
    , content :
        { onlyFocusableElement : List (Html.Attribute msg)
        , firstFocusableElement : List (Html.Attribute msg)
        , lastFocusableElement : List (Html.Attribute msg)
        , autofocusOn : Html.Attribute msg
        }
        -> Html msg
    }


defaults : (Msg -> msg) -> String -> Config msg
defaults wrapMsg t =
    { overlayColor = rgba 128 0 70 0.7
    , wrapMsg = wrapMsg
    , modalStyle =
        batch
            [ backgroundColor (rgb 255 255 255)
            , borderRadius (px 8)
            , border3 (px 2) solid (rgb 127 0 127)
            , margin2 (px 80) auto
            , padding (px 20)
            , maxWidth (px 600)
            , minHeight (vh 40)
            ]
    , titleString = t
    , titleStyles = []
    , autofocusOn = Default
    , content = \_ -> text ""
    }


{-| -}
type Attribute msg
    = Attribute (Config msg -> Config msg)


{-| -}
overlayColor : Color -> Attribute msg
overlayColor color =
    Attribute (\config -> { config | overlayColor = color })


{-| -}
title : String -> Attribute msg
title t =
    Attribute (\config -> { config | titleString = t })


{-| -}
titleStyles : List Style -> Attribute msg
titleStyles styles =
    Attribute (\config -> { config | titleStyles = styles })


{-| -}
custom : List Style -> Attribute msg
custom styles =
    Attribute (\config -> { config | modalStyle = batch styles })


{-| -}
multipleFocusableElementView :
    ({ firstFocusableElement : List (Html.Attribute msg)
     , lastFocusableElement : List (Html.Attribute msg)
     , autofocusElement : Html.Attribute msg
     }
     -> Html msg
    )
    -> Attribute msg
multipleFocusableElementView f =
    Attribute
        (\config ->
            { config
                | content =
                    \{ firstFocusableElement, lastFocusableElement, autofocusOn } ->
                        f
                            { firstFocusableElement = firstFocusableElement
                            , lastFocusableElement = lastFocusableElement
                            , autofocusElement = autofocusOn
                            }
            }
        )


{-| -}
onlyFocusableElementView :
    ({ onlyFocusableElement : List (Html.Attribute msg)
     }
     -> Html msg
    )
    -> Attribute msg
onlyFocusableElementView f =
    Attribute
        (\config ->
            { config
                | content =
                    \{ onlyFocusableElement } ->
                        f
                            { onlyFocusableElement = onlyFocusableElement
                            }
                , autofocusOn = Last
            }
        )



-- VIEW


view :
    Theme
    ->
        { visibleTitle : Bool
        , title : String
        , wrapMsg : Msg -> msg
        }
    ->
        ({ viewContent : { content : List (Html msg), footer : List (Html msg) } -> Html msg
         , closeButton : List (Html.Attribute msg) -> Html msg
         }
         -> Attribute msg
        )
    -> Model
    -> Html msg
view theme config getFocusable model =
    let
        viewFuncs =
            { viewContent = viewContent config.visibleTitle
            , closeButton = closeButton config.wrapMsg
            }

        focusables : Attribute msg
        focusables =
            getFocusable viewFuncs

        size : Attribute msg
        size =
            custom
                [ Css.width (Css.px 600)
                , Css.margin2 (Css.px 50) Css.auto
                , Css.borderRadius (Css.px 20)
                , Css.boxShadow5 Css.zero (Css.px 1) (Css.px 10) Css.zero (Css.rgba 0 0 0 0.35)
                , Css.backgroundColor Colors.white

                -- the modal should grow up to the viewport minus a 50px margin
                , Css.maxHeight (Css.calc (Css.pct 100) Css.minus (Css.px 100))
                ]

        titleAttribute : Attribute msg
        titleAttribute =
            if config.visibleTitle then
                titleStyles
                    [ Fonts.baseFont
                    , Css.fontWeight (Css.int 700)
                    , Css.paddingTop (Css.px 40)
                    , Css.paddingBottom (Css.px 20)
                    , Css.margin Css.zero
                    , Css.fontSize (Css.px 20)
                    , Css.textAlign Css.center
                    , Css.color (themeToTitleColor theme)
                    ]

            else
                titleStyles
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
    in
    view_
        config.wrapMsg
        config.title
        ([ overlayColor (Nri.Ui.Colors.Extra.withAlpha 0.9 (themeToOverlayColor theme))
         , size
         , titleAttribute
         ]
            ++ [ focusables ]
        )
        model
        |> List.singleton
        |> div [ css [ Css.position Css.relative, Css.zIndex (Css.int 1) ] ]


view_ :
    (Msg -> msg)
    -> String
    -> List (Attribute msg)
    -> Model
    -> Html msg
view_ wrapMsg ti attributes model =
    let
        config =
            List.foldl (\(Attribute f) acc -> f acc) (defaults wrapMsg ti) attributes
    in
    case model of
        Opened _ ->
            div
                [ css
                    [ position fixed
                    , top zero
                    , left zero
                    , width (pct 100)
                    , height (pct 100)
                    , displayFlex
                    , alignItems center
                    ]
                ]
                [ viewBackdrop config
                , div
                    [ css [ position relative, config.modalStyle ] ]
                    [ viewModal config ]
                , Root.node "style" [] [ Root.text "body {overflow: hidden;} " ]
                ]

        Closed ->
            text ""


viewBackdrop :
    { a | wrapMsg : Msg -> msg, overlayColor : Color }
    -> Html msg
viewBackdrop config =
    Root.div
        -- We use Root html here in order to allow clicking to exit out of
        -- the overlay. This behavior is available to non-mouse users as
        -- well via the ESC key, so imo it's fine to have this div
        -- be clickable but not focusable.
        [ css
            [ position absolute
            , width (pct 100)
            , height (pct 100)
            , backgroundColor config.overlayColor
            ]
        , onClick (config.wrapMsg (CloseModal OverlayClick))
        ]
        []


viewModal : Config msg -> Html msg
viewModal config =
    section
        [ Role.dialog
        , Aria.labeledBy modalTitleId
        ]
        [ h1 [ id modalTitleId, css config.titleStyles ] [ text config.titleString ]
        , config.content
            (case config.autofocusOn of
                Last ->
                    { onlyFocusableElement =
                        [ Key.onKeyDown
                            [ Key.tabBack (Focus firstId)
                            , Key.tab (Focus firstId)
                            ]
                        , id firstId
                        ]
                            |> List.map (Attributes.map config.wrapMsg)
                    , firstFocusableElement =
                        [ Key.onKeyDown [ Key.tabBack (Focus autofocusId) ]
                        , id firstId
                        ]
                            |> List.map (Attributes.map config.wrapMsg)
                    , lastFocusableElement =
                        [ Key.onKeyDown [ Key.tab (Focus firstId) ]
                        , id autofocusId
                        ]
                            |> List.map (Attributes.map config.wrapMsg)
                    , autofocusOn =
                        id autofocusId
                            |> Attributes.map config.wrapMsg
                    }

                _ ->
                    { onlyFocusableElement =
                        [ Key.onKeyDown
                            [ Key.tabBack (Focus firstId)
                            , Key.tab (Focus firstId)
                            ]
                        , id firstId
                        ]
                            |> List.map (Attributes.map config.wrapMsg)
                    , firstFocusableElement =
                        [ Key.onKeyDown [ Key.tabBack (Focus lastId) ]
                        , id firstId
                        ]
                            |> List.map (Attributes.map config.wrapMsg)
                    , lastFocusableElement =
                        [ Key.onKeyDown [ Key.tab (Focus firstId) ]
                        , id lastId
                        ]
                            |> List.map (Attributes.map config.wrapMsg)
                    , autofocusOn =
                        id autofocusId
                            |> Attributes.map config.wrapMsg
                    }
            )
        ]


{-| -}
viewContent : Bool -> { content : List (Html msg), footer : List (Html msg) } -> Html msg
viewContent visibleTitle { content, footer } =
    div []
        [ viewInnerContent content visibleTitle (not (List.isEmpty footer))
        , viewFooter footer
        ]


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
        [ css (modalTitleStyles ++ modalFooterStyles)
        ]
        [ div
            [ css
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
            [ css
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
closeButton : (Msg -> msg) -> List (Html.Attribute msg) -> Html msg
closeButton wrapMsg focusableElementAttrs =
    button
        (Widget.label "Close modal"
            :: Attributes.map wrapMsg (onClick close)
            :: css
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
            :: focusableElementAttrs
        )
        [ Nri.Ui.Svg.V1.toHtml Nri.Ui.SpriteSheet.xSvg
        ]



-- IDS


modalTitleId : String
modalTitleId =
    "modal__title"


firstId : String
firstId =
    "modal__first-focusable-element"


lastId : String
lastId =
    "modal__last-focusable-element"


autofocusId : String
autofocusId =
    "modal__autofocus-element"
