module Examples.Modal exposing (Msg, State, example, init, update, subscriptions)

{-|

@docs Msg, State, example, init, update, subscriptions

-}

import Accessibility.Styled as Html exposing (Html, div, h3, h4, p, text)
import Css exposing (..)
import Css.Global
import Html as Root
import Html.Styled.Attributes exposing (css)
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Button.V9 as Button
import Nri.Ui.Checkbox.V5 as Checkbox
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Modal.V7 as Modal


{-| -}
type alias State =
    { infoModal : Modal.Model
    , warningModal : Modal.Model
    , visibleTitle : Bool
    , showX : Bool
    , showContinue : Bool
    , showSecondary : Bool
    , dismissOnEscAndOverlayClick : Bool
    }


{-| -}
init : State
init =
    { infoModal = Modal.init
    , warningModal = Modal.init
    , visibleTitle = True
    , showX = True
    , showContinue = True
    , showSecondary = False
    , dismissOnEscAndOverlayClick = True
    }


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { name = "Nri.Ui.Modal.V7"
    , category = Modals
    , content =
        [ Button.button "Launch Info Modal"
            [ Button.onClick (InfoModalMsg (Modal.open "launch-info-modal"))
            , Button.custom
                [ Html.Styled.Attributes.id "launch-info-modal"
                , css [ Css.marginRight (Css.px 16) ]
                ]
            , Button.secondary
            , Button.medium
            ]
        , Button.button "Launch Warning Modal"
            [ Button.onClick (WarningModalMsg (Modal.open "launch-warning-modal"))
            , Button.custom [ Html.Styled.Attributes.id "launch-warning-modal" ]
            , Button.secondary
            , Button.medium
            ]
        , Modal.info
            { title = "Modal.info"
            , visibleTitle = state.visibleTitle
            , wrapMsg = InfoModalMsg
            , content =
                viewContent state
                    InfoModalMsg
                    Button.primary
                    Button.secondary
            }
            state.infoModal
        , Modal.warning
            { title = "Modal.warning"
            , visibleTitle = state.visibleTitle
            , wrapMsg = WarningModalMsg
            , content =
                viewContent state
                    WarningModalMsg
                    Button.danger
                    Button.secondary
            }
            state.warningModal
        ]
            |> List.map (Html.map parentMessage)
    }


viewContent :
    State
    -> (Modal.Msg -> Msg)
    -> Button.Attribute Msg
    -> Button.Attribute Msg
    -> Modal.FocusableElementAttrs Msg
    -> Html Msg
viewContent state wrapMsg firstButtonStyle secondButtonStyle focusableElementAttrs =
    case ( state.showX, state.showContinue, state.showSecondary ) of
        ( True, True, True ) ->
            div []
                [ Modal.closeButton wrapMsg focusableElementAttrs.firstFocusableElement
                , Modal.viewContent [ viewSettings state ]
                , Modal.viewFooter
                    [ Button.button "Continue"
                        [ firstButtonStyle
                        , Button.onClick ForceClose
                        ]
                    , Button.button "Close"
                        [ secondButtonStyle
                        , Button.onClick ForceClose
                        , Button.custom focusableElementAttrs.lastFocusableElement
                        ]
                    ]
                ]

        ( True, False, True ) ->
            div []
                [ Modal.closeButton wrapMsg focusableElementAttrs.firstFocusableElement
                , Modal.viewContent [ viewSettings state ]
                , Modal.viewFooter
                    [ Button.button "Close"
                        [ secondButtonStyle
                        , Button.onClick ForceClose
                        , Button.custom focusableElementAttrs.lastFocusableElement
                        ]
                    ]
                ]

        ( True, False, False ) ->
            div []
                [ Modal.closeButton wrapMsg focusableElementAttrs.firstFocusableElement
                , Modal.viewContent [ viewSettings state ]
                ]

        ( True, True, False ) ->
            div []
                [ Modal.closeButton wrapMsg focusableElementAttrs.firstFocusableElement
                , Modal.viewContent [ viewSettings state ]
                , Modal.viewFooter
                    [ Button.button "Continue"
                        [ firstButtonStyle
                        , Button.onClick ForceClose
                        , Button.custom focusableElementAttrs.lastFocusableElement
                        ]
                    ]
                ]

        ( False, True, True ) ->
            div []
                [ Modal.viewContent [ viewSettings state ]
                , Modal.viewFooter
                    [ Button.button "Continue"
                        [ firstButtonStyle
                        , Button.onClick ForceClose
                        , Button.custom focusableElementAttrs.firstFocusableElement
                        ]
                    , Button.button "Close"
                        [ secondButtonStyle
                        , Button.onClick ForceClose
                        , Button.custom focusableElementAttrs.lastFocusableElement
                        ]
                    ]
                ]

        ( False, False, True ) ->
            div []
                [ Modal.viewContent [ viewSettings state ]
                , Modal.viewFooter
                    [ Button.button "Close"
                        [ secondButtonStyle
                        , Button.onClick ForceClose
                        , Button.custom focusableElementAttrs.lastFocusableElement
                        ]
                    ]
                ]

        ( False, True, False ) ->
            div []
                [ Modal.viewContent [ viewSettings state ]
                , Modal.viewFooter
                    [ Button.button "Continue"
                        [ firstButtonStyle
                        , Button.onClick ForceClose
                        , Button.custom focusableElementAttrs.lastFocusableElement
                        ]
                    ]
                ]

        ( False, False, False ) ->
            div []
                [ Modal.viewContent [ viewSettings state ]
                ]


viewSettings : State -> Html Msg
viewSettings state =
    div []
        [ Checkbox.viewWithLabel
            { identifier = "visible-title"
            , label = "Visible title"
            , selected = Checkbox.selectedFromBool state.visibleTitle
            , setterMsg = SetVisibleTitle
            , disabled = False
            , theme = Checkbox.Square
            }
        , Checkbox.viewWithLabel
            { identifier = "show-x"
            , label = "Show X button"
            , selected = Checkbox.selectedFromBool state.showX
            , setterMsg = SetShowX
            , disabled = False
            , theme = Checkbox.Square
            }
        , Checkbox.viewWithLabel
            { identifier = "show-continue"
            , label = "Show main button"
            , selected = Checkbox.selectedFromBool state.showContinue
            , setterMsg = SetShowContinue
            , disabled = False
            , theme = Checkbox.Square
            }
        , Checkbox.viewWithLabel
            { identifier = "show-secondary"
            , label = "Show secondary button"
            , selected = Checkbox.selectedFromBool state.showSecondary
            , setterMsg = SetShowSecondary
            , disabled = False
            , theme = Checkbox.Square
            }
        , Checkbox.viewWithLabel
            { identifier = "dismiss-on-click"
            , label = "Dismiss on ESC and on backdrop click"
            , selected = Checkbox.selectedFromBool state.dismissOnEscAndOverlayClick
            , setterMsg = SetDismissOnEscAndOverlayClick
            , disabled = False
            , theme = Checkbox.Square
            }
        ]


{-| -}
type Msg
    = InfoModalMsg Modal.Msg
    | WarningModalMsg Modal.Msg
    | ForceClose
    | SetVisibleTitle Bool
    | SetShowX Bool
    | SetShowContinue Bool
    | SetShowSecondary Bool
    | SetDismissOnEscAndOverlayClick Bool


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    let
        updateConfig =
            { dismissOnEscAndOverlayClick = state.dismissOnEscAndOverlayClick }
    in
    case msg of
        InfoModalMsg modalMsg ->
            case Modal.update updateConfig modalMsg state.infoModal of
                ( newState, cmds ) ->
                    ( { state | infoModal = newState }
                    , Cmd.map InfoModalMsg cmds
                    )

        WarningModalMsg modalMsg ->
            case Modal.update updateConfig modalMsg state.warningModal of
                ( newState, cmds ) ->
                    ( { state | warningModal = newState }
                    , Cmd.map WarningModalMsg cmds
                    )

        ForceClose ->
            ( { state
                | infoModal = Modal.init
                , warningModal = Modal.init
              }
            , Cmd.none
            )

        SetVisibleTitle value ->
            ( { state | visibleTitle = value }, Cmd.none )

        SetShowX value ->
            ( { state | showX = value }, Cmd.none )

        SetShowContinue value ->
            ( { state | showContinue = value }, Cmd.none )

        SetShowSecondary value ->
            ( { state | showSecondary = value }, Cmd.none )

        SetDismissOnEscAndOverlayClick value ->
            ( { state | dismissOnEscAndOverlayClick = value }, Cmd.none )


{-| -}
subscriptions : State -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map InfoModalMsg (Modal.subscriptions model.infoModal)
        , Sub.map WarningModalMsg (Modal.subscriptions model.warningModal)
        ]
