module Examples.Modal exposing (Msg, State, example, init, update, subscriptions)

{-|

@docs Msg, State, example, init, update, subscriptions

-}

import Accessibility.Styled as Html exposing (Html, div, h3, h4, p, span, text)
import Css exposing (..)
import Css.Global
import Html as Root
import Html.Styled.Attributes exposing (css)
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Button.V9 as Button
import Nri.Ui.Checkbox.V5 as Checkbox
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Modal.V8 as Modal
import Nri.Ui.Text.V4 as Text


{-| -}
type alias State =
    { infoModal : Modal.Model
    , warningModal : Modal.Model
    , visibleTitle : Bool
    , showX : Bool
    , showContinue : Bool
    , showSecondary : Bool
    , dismissOnEscAndOverlayClick : Bool
    , longContent : Bool
    }


{-| -}
init : State
init =
    { infoModal = Modal.init
    , warningModal = Modal.init
    , visibleTitle = True
    , showX = True
    , showContinue = True
    , showSecondary = True
    , dismissOnEscAndOverlayClick = True
    , longContent = True
    }


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { name = "Nri.Ui.Modal.V8"
    , category = Modals
    , content =
        [ viewSettings state
        , Button.button "Launch Info Modal"
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
        , let
            ( options, attribs ) =
                configAndAttributes state InfoModalMsg Button.primary
          in
          Modal.info { title = "Modal.info", wrapMsg = InfoModalMsg }
            options
            attribs
            state.infoModal
        , let
            ( options, attribs ) =
                configAndAttributes state WarningModalMsg Button.danger
          in
          Modal.warning { title = "Modal.warning", wrapMsg = WarningModalMsg }
            options
            attribs
            state.warningModal
        ]
            |> List.map (Html.map parentMessage)
    }


configAndAttributes :
    State
    -> (Modal.Msg -> Msg)
    -> Button.Attribute Msg
    -> ( List (Modal.OptionalConfig Msg), List (Modal.Focusable Msg) )
configAndAttributes state wrapMsg firstButtonStyle =
    let
        defaultOptions =
            if state.visibleTitle then
                []

            else
                [ Modal.invisibleTitle ]
    in
    case ( state.showX, state.showContinue, state.showSecondary ) of
        ( True, True, True ) ->
            ( defaultOptions
            , [ Modal.multipleFocusableElementView
                    (\focusableElementAttrs ->
                        div []
                            [ Modal.closeButton wrapMsg focusableElementAttrs.firstFocusableElement
                            , Modal.viewContent
                                { content = [ viewModalContent state.longContent ]
                                , footer =
                                    [ Button.button "Continue"
                                        [ firstButtonStyle
                                        , Button.onClick ForceClose
                                        , Button.large
                                        , Button.custom [ focusableElementAttrs.autofocusElement ]
                                        ]
                                    , ClickableText.button "Close"
                                        [ ClickableText.onClick ForceClose
                                        , ClickableText.large
                                        , ClickableText.custom
                                            (css [ Css.marginTop (Css.px 12) ]
                                                :: focusableElementAttrs.lastFocusableElement
                                            )
                                        ]
                                    ]
                                }
                                state.visibleTitle
                            ]
                    )
              ]
            )

        ( True, False, True ) ->
            ( defaultOptions
            , [ Modal.multipleFocusableElementView
                    (\focusableElementAttrs ->
                        div []
                            [ Modal.closeButton wrapMsg focusableElementAttrs.firstFocusableElement
                            , Modal.viewContent
                                { content = [ viewModalContent state.longContent ]
                                , footer =
                                    [ ClickableText.button "Close"
                                        [ ClickableText.onClick ForceClose
                                        , ClickableText.large
                                        , ClickableText.custom
                                            (css [ Css.marginTop (Css.px 12) ]
                                                :: focusableElementAttrs.lastFocusableElement
                                            )
                                        ]
                                    ]
                                }
                                state.visibleTitle
                            ]
                    )
              ]
            )

        ( True, False, False ) ->
            ( defaultOptions
            , [ Modal.multipleFocusableElementView
                    (\focusableElementAttrs ->
                        div []
                            [ Modal.closeButton wrapMsg focusableElementAttrs.firstFocusableElement
                            , Modal.viewContent
                                { content = [ viewModalContent state.longContent ]
                                , footer = []
                                }
                                state.visibleTitle
                            ]
                    )
              ]
            )

        ( True, True, False ) ->
            ( Modal.autofocusOnLastElement :: defaultOptions
            , [ Modal.multipleFocusableElementView
                    (\focusableElementAttrs ->
                        div []
                            [ Modal.closeButton wrapMsg focusableElementAttrs.firstFocusableElement
                            , Modal.viewContent
                                { content = [ viewModalContent state.longContent ]
                                , footer =
                                    [ Button.button "Continue"
                                        [ firstButtonStyle
                                        , Button.onClick ForceClose
                                        , Button.custom focusableElementAttrs.lastFocusableElement
                                        , Button.large
                                        ]
                                    ]
                                }
                                state.visibleTitle
                            ]
                    )
              ]
            )

        ( False, True, True ) ->
            ( defaultOptions
            , [ Modal.multipleFocusableElementView
                    (\focusableElementAttrs ->
                        div []
                            [ Modal.viewContent
                                { content = [ viewModalContent state.longContent ]
                                , footer =
                                    [ Button.button "Continue"
                                        [ firstButtonStyle
                                        , Button.onClick ForceClose
                                        , Button.custom focusableElementAttrs.firstFocusableElement
                                        , Button.large
                                        ]
                                    , ClickableText.button "Close"
                                        [ ClickableText.onClick ForceClose
                                        , ClickableText.large
                                        , ClickableText.custom
                                            (css [ Css.marginTop (Css.px 12) ]
                                                :: focusableElementAttrs.lastFocusableElement
                                            )
                                        ]
                                    ]
                                }
                                state.visibleTitle
                            ]
                    )
              ]
            )

        ( False, False, True ) ->
            ( Modal.autofocusOnLastElement :: defaultOptions
            , [ Modal.multipleFocusableElementView
                    (\focusableElementAttrs ->
                        div []
                            [ Modal.viewContent
                                { content = [ viewModalContent state.longContent ]
                                , footer =
                                    [ ClickableText.button "Close"
                                        [ ClickableText.onClick ForceClose
                                        , ClickableText.large
                                        , ClickableText.custom
                                            (css [ Css.marginTop (Css.px 12) ]
                                                :: focusableElementAttrs.lastFocusableElement
                                            )
                                        ]
                                    ]
                                }
                                state.visibleTitle
                            ]
                    )
              ]
            )

        ( False, True, False ) ->
            ( Modal.autofocusOnLastElement :: defaultOptions
            , [ Modal.multipleFocusableElementView
                    (\focusableElementAttrs ->
                        div []
                            [ Modal.viewContent
                                { content = [ viewModalContent state.longContent ]
                                , footer =
                                    [ Button.button "Continue"
                                        [ firstButtonStyle
                                        , Button.onClick ForceClose
                                        , Button.custom [ focusableElementAttrs.autofocusElement ]
                                        , Button.large
                                        ]
                                    ]
                                }
                                state.visibleTitle
                            ]
                    )
              ]
            )

        ( False, False, False ) ->
            ( defaultOptions
            , [ Modal.multipleFocusableElementView
                    (\focusableElementAttrs ->
                        div []
                            [ Modal.viewContent
                                { content = [ viewModalContent state.longContent ]
                                , footer = []
                                }
                                state.visibleTitle
                            ]
                    )
              ]
            )


viewModalContent : Bool -> Html msg
viewModalContent longContent =
    Text.mediumBody
        [ span [ css [ whiteSpace preLine ] ]
            [ if longContent then
                """Soufflé pastry chocolate cake danish muffin. Candy wafer pastry ice cream cheesecake toffee cookie cake carrot cake. Macaroon pie jujubes gummies cookie pie. Gummi bears brownie pastry carrot cake cotton candy. Jelly-o sweet roll biscuit cake soufflé lemon drops tiramisu marshmallow macaroon. Chocolate jelly halvah marzipan macaroon cupcake sweet cheesecake carrot cake.

                    Sesame snaps pastry muffin cookie. Powder powder sweet roll toffee cake icing. Chocolate cake sweet roll gingerbread icing chupa chups sweet roll sesame snaps. Chocolate croissant chupa chups jelly beans toffee. Jujubes sweet wafer marshmallow halvah jelly. Liquorice sesame snaps sweet.

                    Tootsie roll icing jelly danish ice cream tiramisu sweet roll. Fruitcake ice cream dragée. Bear claw sugar plum sweet jelly beans bonbon dragée tart. Gingerbread chocolate sweet. Apple pie danish toffee sugar plum jelly beans donut. Chocolate cake croissant caramels chocolate bar. Jelly beans caramels toffee chocolate cake liquorice. Toffee pie sugar plum cookie toffee muffin. Marzipan marshmallow marzipan liquorice tiramisu."""
                    |> text

              else
                "Ice cream tootsie roll donut sweet cookie liquorice sweet donut. Sugar plum danish apple pie sesame snaps chocolate bar biscuit. Caramels macaroon jelly gummies sweet tootsie roll tiramisu apple pie. Dessert chocolate bar lemon drops dragée jelly powder cheesecake chocolate."
                    |> text
            ]
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
        , Checkbox.viewWithLabel
            { identifier = "long-content"
            , label = "Display longer content"
            , selected = Checkbox.selectedFromBool state.longContent
            , setterMsg = SetLongContent
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
    | SetLongContent Bool


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

        SetLongContent value ->
            ( { state | longContent = value }, Cmd.none )


{-| -}
subscriptions : State -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map InfoModalMsg (Modal.subscriptions model.infoModal)
        , Sub.map WarningModalMsg (Modal.subscriptions model.warningModal)
        ]
