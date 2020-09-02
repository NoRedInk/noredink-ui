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
    { titleVisibility : Modal.Attribute
    , title : String
    , showX : Bool
    , showContinue : Bool
    , showSecondary : Bool
    , dismissOnEscAndOverlayClick : Bool
    , content : String
    , customStyling : Bool
    , theme : Modal.Attribute
    }


initModalSettings : Control Settings
initModalSettings =
    Control.record Settings
        |> Control.field "titleVisibility" controlTitleVisibility
        |> Control.field "title" (Control.string "Modal Title")
        |> Control.field "showX" (Control.bool True)
        |> Control.field "showContinue" (Control.bool True)
        |> Control.field "showSecondary" (Control.bool True)
        |> Control.field "dismissOnEscAndOverlayClick" (Control.bool True)
        |> Control.field "content"
            (Control.stringTextarea <|
                String.join "\n\n"
                    [ "Generally, you'll want to pair the Modal.warning theme with the Button.danger theme and the Modal.info theme with the Button.primary theme."
                    , "Muffin liquorice powder liquorice jujubes biscuit cookie candy canes lemon drops. Liquorice powder carrot cake dragée icing tootsie roll apple pie lemon drops lemon drops. Jujubes danish bear claw cotton candy. Dragée apple pie tiramisu. Sugar plum dessert pastry marzipan chocolate cake dragée sesame snaps. Marshmallow gingerbread lemon drops. Brownie chocolate fruitcake pastry. Powder jelly beans jujubes. Croissant macaroon dessert cookie candy canes jelly jujubes. Muffin liquorice ice cream wafer donut danish soufflé dragée chocolate bar. Candy croissant candy wafer toffee lemon drops croissant danish sugar plum. Cookie cake candy canes. Pastry powder muffin soufflé tootsie roll sweet cookie tiramisu."
                    , "Candy cake danish gingerbread. Caramels toffee cupcake toffee sweet. Gummi bears candy cheesecake sweet. Pie gingerbread sugar plum halvah muffin icing marzipan wafer icing. Candy fruitcake gummies icing marzipan. Halvah jelly beans candy candy canes biscuit bonbon sesame snaps. Biscuit carrot cake croissant cake chocolate lollipop candy biscuit croissant. Topping jujubes apple pie croissant chocolate cake. Liquorice cookie dragée gummies cotton candy fruitcake lemon drops candy canes. Apple pie lemon drops gummies cake chocolate bar cake jelly-o tiramisu. Chocolate bar icing pudding marshmallow cake soufflé soufflé muffin. Powder lemon drops biscuit sugar plum cupcake carrot cake powder cake dragée. Bear claw gummi bears liquorice sweet roll."
                    ]
            )
        |> Control.field "customStyling" (Control.bool False)
        |> Control.field "theme" controlTheme


controlTitleVisibility : Control Modal.Attribute
controlTitleVisibility =
    Control.choice
        [ ( "showTitle", Control.value Modal.showTitle )
        , ( "hideTitle", Control.value Modal.hideTitle )
        ]


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
                    settings.theme :: settings.titleVisibility :: stylingAttrs
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
                { title = settings.title
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
                        , viewModalContent settings.content
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
                        , viewModalContent settings.content
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
                        , viewModalContent settings.content
                        ]
                    , footer = []
                    }
                )

        ( True, True, False ) ->
            Modal.MultipleFocusableElements <|
                \modalOptions ->
                    { content =
                        [ modalOptions.closeButton modalOptions.firstFocusableElement
                        , viewModalContent settings.content
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
                    { content = [ viewModalContent settings.content ]
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
                    { content = [ viewModalContent settings.content ]
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
                    { content = [ viewModalContent settings.content ]
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
                    { content = [ viewModalContent settings.content ]
                    , footer = []
                    }
                )


viewModalContent : String -> Html msg
viewModalContent content =
    Text.mediumBody
        [ span
            [ Attributes.css [ whiteSpace preLine ] ]
            [ text content ]
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
