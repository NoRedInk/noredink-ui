module Examples.Modal exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled as Html exposing (Html, div, h3, h4, p, span, text)
import AtomicDesignType exposing (AtomicDesignType(..))
import Category exposing (Category(..))
import Css exposing (..)
import Css.Global
import Example exposing (Example)
import Html as Root
import Html.Styled.Attributes as Attributes
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Checkbox.V5 as Checkbox
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Modal.V9 as Modal
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
example : Example State Msg
example =
    { name = "Nri.Ui.Modal.V8"
    , categories = [ Modals ]
    , atomicDesignType = Organism
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = subscriptions
    , view =
        \state ->
            [ viewSettings state
            , Button.button "Launch Info Modal"
                [ Button.onClick (InfoModalMsg (Modal.open "launch-info-modal"))
                , Button.custom [ Attributes.id "launch-info-modal" ]
                , Button.css [ Css.marginRight (Css.px 16) ]
                , Button.secondary
                , Button.medium
                ]
            , Button.button "Launch Warning Modal"
                [ Button.onClick (WarningModalMsg (Modal.open "launch-warning-modal"))
                , Button.custom [ Attributes.id "launch-warning-modal" ]
                , Button.secondary
                , Button.medium
                ]
            , let
                params =
                    ( state, InfoModalMsg, Button.primary )
              in
              Modal.info { title = "Modal.info", wrapMsg = InfoModalMsg, visibleTitle = state.visibleTitle }
                (getFocusable params)
                state.infoModal
            , let
                params =
                    ( state, WarningModalMsg, Button.danger )
              in
              Modal.warning { title = "Modal.warning", wrapMsg = WarningModalMsg, visibleTitle = state.visibleTitle }
                (getFocusable params)
                state.warningModal
            ]
    }


getFocusable :
    ( State, Modal.Msg -> Msg, Button.Attribute Msg )
    ->
        { viewContent : { content : List (Html Msg), footer : List (Html Msg) } -> Html Msg
        , closeButton : List (Html.Attribute Msg) -> Html Msg
        }
    -> Modal.Focusable Msg
getFocusable ( state, wrapMsg, firstButtonStyle ) { viewContent, closeButton } =
    case ( state.showX, state.showContinue, state.showSecondary ) of
        ( True, True, True ) ->
            Modal.multipleFocusableElementView
                (\{ firstFocusableElement, autofocusElement, lastFocusableElement } ->
                    div []
                        [ closeButton firstFocusableElement
                        , viewContent
                            { content = [ viewModalContent state.longContent ]
                            , footer =
                                [ Button.button "Continue"
                                    [ firstButtonStyle
                                    , Button.onClick ForceClose
                                    , Button.large
                                    , Button.custom [ autofocusElement ]
                                    ]
                                , ClickableText.button "Close"
                                    [ ClickableText.onClick ForceClose
                                    , ClickableText.large
                                    , ClickableText.custom lastFocusableElement
                                    , ClickableText.css [ Css.marginTop (Css.px 12) ]
                                    ]
                                ]
                            }
                        ]
                )

        ( True, False, True ) ->
            Modal.multipleFocusableElementView
                (\{ firstFocusableElement, autofocusElement, lastFocusableElement } ->
                    div []
                        [ closeButton firstFocusableElement
                        , viewContent
                            { content = [ viewModalContent state.longContent ]
                            , footer =
                                [ ClickableText.button "Close"
                                    [ ClickableText.onClick ForceClose
                                    , ClickableText.large
                                    , ClickableText.custom (autofocusElement :: lastFocusableElement)
                                    , ClickableText.css [ Css.marginTop (Css.px 12) ]
                                    ]
                                ]
                            }
                        ]
                )

        ( True, False, False ) ->
            Modal.onlyFocusableElementView
                (\{ onlyFocusableElement } ->
                    div []
                        [ closeButton onlyFocusableElement
                        , viewContent
                            { content = [ viewModalContent state.longContent ]
                            , footer = []
                            }
                        ]
                )

        ( True, True, False ) ->
            Modal.multipleFocusableElementView
                (\{ firstFocusableElement, autofocusElement, lastFocusableElement } ->
                    div []
                        [ closeButton firstFocusableElement
                        , viewContent
                            { content = [ viewModalContent state.longContent ]
                            , footer =
                                [ Button.button "Continue"
                                    [ firstButtonStyle
                                    , Button.onClick ForceClose
                                    , Button.custom (autofocusElement :: lastFocusableElement)
                                    , Button.large
                                    ]
                                ]
                            }
                        ]
                )

        ( False, True, True ) ->
            Modal.multipleFocusableElementView
                (\{ firstFocusableElement, autofocusElement, lastFocusableElement } ->
                    div []
                        [ viewContent
                            { content = [ viewModalContent state.longContent ]
                            , footer =
                                [ Button.button "Continue"
                                    [ firstButtonStyle
                                    , Button.onClick ForceClose
                                    , Button.custom (autofocusElement :: firstFocusableElement)
                                    , Button.large
                                    ]
                                , ClickableText.button "Close"
                                    [ ClickableText.onClick ForceClose
                                    , ClickableText.large
                                    , ClickableText.custom lastFocusableElement
                                    , ClickableText.css [ Css.marginTop (Css.px 12) ]
                                    ]
                                ]
                            }
                        ]
                )

        ( False, False, True ) ->
            Modal.onlyFocusableElementView
                (\{ onlyFocusableElement } ->
                    div []
                        [ viewContent
                            { content = [ viewModalContent state.longContent ]
                            , footer =
                                [ ClickableText.button "Close"
                                    [ ClickableText.onClick ForceClose
                                    , ClickableText.large
                                    , ClickableText.custom onlyFocusableElement
                                    , ClickableText.css [ Css.marginTop (Css.px 12) ]
                                    ]
                                ]
                            }
                        ]
                )

        ( False, True, False ) ->
            Modal.onlyFocusableElementView
                (\{ onlyFocusableElement } ->
                    div []
                        [ viewContent
                            { content = [ viewModalContent state.longContent ]
                            , footer =
                                [ Button.button "Continue"
                                    [ firstButtonStyle
                                    , Button.onClick ForceClose
                                    , Button.custom onlyFocusableElement
                                    , Button.large
                                    ]
                                ]
                            }
                        ]
                )

        ( False, False, False ) ->
            Modal.onlyFocusableElementView
                (\_ ->
                    div []
                        [ viewContent
                            { content = [ viewModalContent state.longContent ]
                            , footer = []
                            }
                        ]
                )


viewModalContent : Bool -> Html msg
viewModalContent longContent =
    Text.mediumBody
        [ span [ Attributes.css [ whiteSpace preLine ] ]
            [ if longContent then
                """Soufflé pastry chocolate cake danish muffin. Candy wafer pastry ice cream cheesecake toffee cookie cake carrot cake. Macaroon pie jujubes gummies cookie pie. Gummi bears brownie pastry carrot cake cotton candy. Jelly-o sweet roll biscuit cake soufflé lemon drops tiramisu marshmallow macaroon. Chocolate jelly halvah marzipan macaroon cupcake sweet cheesecake carrot cake.

                    Sesame snaps pastry muffin cookie. Powder powder sweet roll toffee cake icing. Chocolate cake sweet roll gingerbread icing chupa chups sweet roll sesame snaps. Chocolate croissant chupa chups jelly beans toffee. Jujubes sweet wafer marshmallow halvah jelly. Liquorice sesame snaps sweet.

                    Tootsie roll icing jelly danish ice cream tiramisu sweet roll. Fruitcake ice cream dragée. Bear claw sugar plum sweet jelly beans bonbon dragée tart. Gingerbread chocolate sweet. Apple pie danish toffee sugar plum jelly beans donut. Chocolate cake croissant caramels chocolate bar. Jelly beans caramels toffee chocolate cake liquorice. Toffee pie sugar plum cookie toffee muffin. Marzipan marshmallow marzipan liquorice tiramisu.

                    Brownie ice cream halvah danish candy ice cream sweet roll jujubes chocolate cake. Chocolate bar sesame snaps bear claw gummies. Dragée cookie brownie cake sugar plum chocolate cake fruitcake toffee. Tiramisu tiramisu cookie cake. Lemon drops pie toffee icing powder biscuit cotton candy gummies. Caramels lemon drops cupcake. Lemon drops toffee macaroon liquorice chocolate bar candy bonbon. Cupcake biscuit cupcake chupa chups candy. Chocolate cake sweet toffee bonbon danish biscuit pudding. Tootsie roll brownie jelly tootsie roll. Jujubes jujubes marshmallow gummi bears bear claw sugar plum. Cupcake bonbon soufflé carrot cake powder fruitcake sugar plum brownie.

                    Danish sesame snaps tiramisu chocolate cake powder cotton candy powder. Liquorice cupcake macaroon sweet soufflé jujubes. Jelly-o oat cake caramels sweet roll. Sweet roll sugar plum gummies cheesecake sesame snaps. Gummies pastry tootsie roll marzipan lollipop muffin sweet cake. Wafer carrot cake halvah bear claw jelly beans apple pie cookie halvah. Brownie sugar plum macaroon halvah croissant pastry. Marzipan muffin carrot cake chocolate jelly beans dragée jelly beans dragée tiramisu. Sweet roll powder apple pie icing halvah marshmallow pastry. Pastry marzipan chocolate cake jelly beans sugar plum carrot cake lollipop croissant. Cotton candy chocolate croissant gummies muffin. Dragée jelly beans oat cake pastry muffin pie. Donut marzipan dessert wafer gingerbread tiramisu macaroon. Cotton candy macaroon gummies oat cake cake gingerbread cotton candy sweet roll pie."""
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
