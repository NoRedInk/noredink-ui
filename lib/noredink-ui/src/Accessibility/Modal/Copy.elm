module Accessibility.Modal.Copy exposing
    ( Model, init, subscriptions
    , update, Msg, close, open
    , view
    , Attribute
    , multipleFocusableElementView, onlyFocusableElementView
    , autofocusOnLastElement
    , overlayColor, custom, titleStyles
    )

{-| COPIED from <https://package.elm-lang.org/packages/tesk9/modal/latest/> for
clean monolith upgrades. Remove this and go back to using tesk9/modal when possible!

    import Accessibility.Modal as Modal
    import Html.Styled exposing (..)
    import Html.Styled.Events exposing (onClick)

    view : Html Modal.Msg
    view =
        Modal.view identity
            "Example modal"
            [ Modal.onlyFocusableElementView
                (\onlyFocusableElementAttributes ->
                    div []
                        [ text "Welcome to this modal! I'm so happy to have you here with me."
                        , button
                            (onClick Modal.close :: onlyFocusableElementAttributes)
                            [ text "Close Modal" ]
                        ]
                )
            ]
            modal

@docs Model, init, subscriptions
@docs update, Msg, close, open
@docs view

@docs Attribute
@docs multipleFocusableElementView, onlyFocusableElementView
@docs autofocusOnLastElement
@docs overlayColor, custom, titleStyles

-}

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
import Browser
import Browser.Dom as Dom
import Browser.Events
import Css exposing (..)
import Html.Styled as Root
import Html.Styled.Attributes as Attributes exposing (css, id)
import Html.Styled.Events exposing (onClick)
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


type alias Config msg =
    { overlayColor : Color
    , wrapMsg : Msg -> msg
    , modalStyle : Style
    , titleString : String
    , titleStyles : List Style
    , autofocusOn : Autofocus
    , content :
        { onlyFocusableElement : List (Accessibility.Styled.Attribute msg)
        , firstFocusableElement : List (Accessibility.Styled.Attribute msg)
        , lastFocusableElement : List (Accessibility.Styled.Attribute msg)
        , autofocusOn : Accessibility.Styled.Attribute msg
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
autofocusOnLastElement : Attribute msg
autofocusOnLastElement =
    Attribute (\config -> { config | autofocusOn = Last })


{-| -}
onlyFocusableElementView : (List (Accessibility.Styled.Attribute msg) -> Html msg) -> Attribute msg
onlyFocusableElementView v =
    Attribute (\config -> { config | content = \{ onlyFocusableElement } -> v onlyFocusableElement })


{-| -}
multipleFocusableElementView :
    ({ firstFocusableElement : List (Accessibility.Styled.Attribute msg)
     , lastFocusableElement : List (Accessibility.Styled.Attribute msg)
     , autofocusElement : Accessibility.Styled.Attribute msg
     }
     -> Html msg
    )
    -> Attribute msg
multipleFocusableElementView v =
    Attribute
        (\config ->
            { config
                | content =
                    \{ firstFocusableElement, lastFocusableElement, autofocusOn } ->
                        v
                            { firstFocusableElement = firstFocusableElement
                            , lastFocusableElement = lastFocusableElement
                            , autofocusElement = autofocusOn
                            }
            }
        )


{-| -}
view :
    (Msg -> msg)
    -> String
    -> List (Attribute msg)
    -> Model
    -> Html msg
view wrapMsg ti attributes model =
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
subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Opened _ ->
            Browser.Events.onKeyDown (Key.escape (CloseModal EscapeKey))

        Closed ->
            Sub.none
