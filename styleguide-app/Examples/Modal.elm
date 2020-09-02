module Examples.Modal exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled as Html exposing (Html, div, h3, h4, p, span, text)
import AtomicDesignType exposing (AtomicDesignType(..))
import Category exposing (Category(..))
import Css exposing (..)
import Debug.Control as Control exposing (Control)
import Example exposing (Example)
import Html as Root
import Html.Styled.Attributes as Attributes
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Checkbox.V5 as Checkbox
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Modal.V11 as Modal
import Nri.Ui.Text.V4 as Text


{-| -}
type alias State =
    { state : Modal.Model
    , settings : Control Settings
    }


{-| -}
init : State
init =
    { state = Modal.init
    , settings = initModalSettings
    }


type alias Settings =
    { visibleTitle : Bool
    , showX : Bool
    , showContinue : Bool
    , showSecondary : Bool
    , dismissOnEscAndOverlayClick : Bool
    , longContent : Bool
    , customStyling : Bool
    , theme : Modal.Attribute
    }


initModalSettings : Control Settings
initModalSettings =
    Control.record Settings
        |> Control.field "visibleTitle" (Control.bool True)
        |> Control.field "showX" (Control.bool True)
        |> Control.field "showContinue" (Control.bool True)
        |> Control.field "showSecondary" (Control.bool True)
        |> Control.field "dismissOnEscAndOverlayClick" (Control.bool True)
        |> Control.field "longContent" (Control.bool True)
        |> Control.field "customStyling" (Control.bool False)
        |> Control.field "theme" controlTheme


controlTheme : Control Modal.Attribute
controlTheme =
    Control.choice
        [ ( "info", Control.value Modal.info )
        , ( "warning", Control.value Modal.warning )
        ]


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.Modal.V11"
    , categories = [ Modals ]
    , atomicDesignType = Organism
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = subscriptions
    , view =
        \state ->
            let
                settings =
                    Control.currentValue state.settings

                titleAttrs =
                    if settings.visibleTitle then
                        []

                    else
                        [ Modal.hideTitle ]

                stylingAttrs =
                    if settings.customStyling then
                        [ Modal.css
                            [ Css.borderRadius Css.zero
                            , Css.width (Css.px 800)
                            ]
                        ]

                    else
                        []

                attrs =
                    settings.theme :: titleAttrs ++ stylingAttrs
            in
            [ Control.view UpdateSettings state.settings
                |> Html.fromUnstyled
            , Button.button "Launch Modal"
                [ Button.onClick (OpenModal "launch-modal")
                , Button.custom [ Attributes.id "launch-modal" ]
                , Button.secondary
                , Button.medium
                ]
            , Modal.view
                { title = "Modal Title"
                , wrapMsg = ModalMsg
                , focusManager = makeFocusManager settings
                }
                attrs
                state.state
            ]
    }


makeFocusManager : Settings -> Modal.FocusManager Msg
makeFocusManager settings =
    case ( settings.showX, settings.showContinue, settings.showSecondary ) of
        ( True, True, True ) ->
            Modal.MultipleFocusableElements <|
                \modalOptions ->
                    { content =
                        [ modalOptions.closeButton modalOptions.firstFocusableElement
                        , viewModalContent settings.longContent
                        ]
                    , footer =
                        [ Button.button "Continue"
                            [ Button.premium
                            , Button.onClick ForceClose
                            , Button.large
                            , Button.custom [ modalOptions.autofocusElement ]
                            ]
                        , ClickableText.button "Close"
                            [ ClickableText.onClick ForceClose
                            , ClickableText.large
                            , ClickableText.custom modalOptions.lastFocusableElement
                            , ClickableText.css [ Css.marginTop (Css.px 12) ]
                            ]
                        ]
                    }

        ( True, False, True ) ->
            Modal.MultipleFocusableElements <|
                \modalOptions ->
                    { content =
                        [ modalOptions.closeButton modalOptions.firstFocusableElement
                        , viewModalContent settings.longContent
                        ]
                    , footer =
                        [ ClickableText.button "Close"
                            [ ClickableText.onClick ForceClose
                            , ClickableText.large
                            , ClickableText.custom (modalOptions.autofocusElement :: modalOptions.lastFocusableElement)
                            , ClickableText.css [ Css.marginTop (Css.px 12) ]
                            ]
                        ]
                    }

        ( True, False, False ) ->
            Modal.OneFocusableElement
                (\{ onlyFocusableElement, closeButton } ->
                    { content =
                        [ closeButton onlyFocusableElement
                        , viewModalContent settings.longContent
                        ]
                    , footer = []
                    }
                )

        ( True, True, False ) ->
            Modal.MultipleFocusableElements <|
                \modalOptions ->
                    { content =
                        [ modalOptions.closeButton modalOptions.firstFocusableElement
                        , viewModalContent settings.longContent
                        ]
                    , footer =
                        [ Button.button "Continue"
                            [ Button.premium
                            , Button.onClick ForceClose
                            , Button.custom (modalOptions.autofocusElement :: modalOptions.lastFocusableElement)
                            , Button.large
                            ]
                        ]
                    }

        ( False, True, True ) ->
            Modal.MultipleFocusableElements <|
                \modalOptions ->
                    { content = [ viewModalContent settings.longContent ]
                    , footer =
                        [ Button.button "Continue"
                            [ Button.premium
                            , Button.onClick ForceClose
                            , Button.custom (modalOptions.autofocusElement :: modalOptions.firstFocusableElement)
                            , Button.large
                            ]
                        , ClickableText.button "Close"
                            [ ClickableText.onClick ForceClose
                            , ClickableText.large
                            , ClickableText.custom modalOptions.lastFocusableElement
                            , ClickableText.css [ Css.marginTop (Css.px 12) ]
                            ]
                        ]
                    }

        ( False, False, True ) ->
            Modal.OneFocusableElement
                (\{ onlyFocusableElement } ->
                    { content = [ viewModalContent settings.longContent ]
                    , footer =
                        [ ClickableText.button "Close"
                            [ ClickableText.onClick ForceClose
                            , ClickableText.large
                            , ClickableText.custom onlyFocusableElement
                            , ClickableText.css [ Css.marginTop (Css.px 12) ]
                            ]
                        ]
                    }
                )

        ( False, True, False ) ->
            Modal.OneFocusableElement
                (\{ onlyFocusableElement } ->
                    { content = [ viewModalContent settings.longContent ]
                    , footer =
                        [ Button.button "Continue"
                            [ Button.premium
                            , Button.onClick ForceClose
                            , Button.custom onlyFocusableElement
                            , Button.large
                            ]
                        ]
                    }
                )

        ( False, False, False ) ->
            Modal.OneFocusableElement
                (\_ ->
                    { content = [ viewModalContent settings.longContent ]
                    , footer = []
                    }
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
                "Generally, you'll want to pair the Modal.warning theme with the Button.danger theme and the Modal.info theme with the Button.primary theme."
                    |> text
            ]
        ]


{-| -}
type Msg
    = OpenModal String
    | ModalMsg Modal.Msg
    | ForceClose
    | UpdateSettings (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    let
        settings =
            state.settings

        updateConfig =
            { dismissOnEscAndOverlayClick =
                (Control.currentValue settings).dismissOnEscAndOverlayClick
            }
    in
    case msg of
        OpenModal returnFocusTo ->
            let
                ( newState, cmd ) =
                    Modal.open returnFocusTo
            in
            ( { state | state = newState }
            , Cmd.map ModalMsg cmd
            )

        ModalMsg modalMsg ->
            case Modal.update updateConfig modalMsg state.state of
                ( newState, cmd ) ->
                    ( { state | state = newState }
                    , Cmd.map ModalMsg cmd
                    )

        ForceClose ->
            let
                ( newState, cmd ) =
                    Modal.close state.state
            in
            ( { state | state = newState }
            , Cmd.map ModalMsg cmd
            )

        UpdateSettings value ->
            ( { state | settings = value }, Cmd.none )


{-| -}
subscriptions : State -> Sub Msg
subscriptions model =
    Sub.map ModalMsg (Modal.subscriptions model.state)
