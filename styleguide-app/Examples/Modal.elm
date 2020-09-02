module Examples.Modal exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled as Html exposing (Html, div, h3, h4, p, span, text)
import AtomicDesignType exposing (AtomicDesignType(..))
import Browser.Dom as Dom
import Category exposing (Category(..))
import Css exposing (..)
import Debug.Control as Control exposing (Control)
import Example exposing (Example)
import Html as Root
import Html.Styled.Attributes as Attributes exposing (css)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Checkbox.V5 as Checkbox
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusTrap.V1 as FocusTrap
import Nri.Ui.Modal.V11 as Modal
import Nri.Ui.Text.V4 as Text
import Task


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
    { title : String
    , showX : Bool
    , showContinue : Bool
    , showSecondary : Bool
    , dismissOnEscAndOverlayClick : Bool
    , content : String
    , attributes : List Modal.Attribute
    }


initModalSettings : Control Settings
initModalSettings =
    Control.record Settings
        |> Control.field "Modal title" (Control.string "Modal Title")
        |> Control.field "X button" (Control.bool True)
        |> Control.field "Continue button" (Control.bool True)
        |> Control.field "Close button" (Control.bool True)
        |> Control.field "dismissOnEscAndOverlayClick" (Control.bool True)
        |> Control.field "Content"
            (Control.stringTextarea <|
                String.join "\n\n"
                    [ "Generally, you'll want to pair the Modal.warning theme with the Button.danger theme and the Modal.info theme with the Button.primary theme."
                    , "Muffin liquorice powder liquorice jujubes biscuit cookie candy canes lemon drops. Liquorice powder carrot cake dragée icing tootsie roll apple pie lemon drops lemon drops. Jujubes danish bear claw cotton candy. Dragée apple pie tiramisu. Sugar plum dessert pastry marzipan chocolate cake dragée sesame snaps. Marshmallow gingerbread lemon drops. Brownie chocolate fruitcake pastry. Powder jelly beans jujubes. Croissant macaroon dessert cookie candy canes jelly jujubes. Muffin liquorice ice cream wafer donut danish soufflé dragée chocolate bar. Candy croissant candy wafer toffee lemon drops croissant danish sugar plum. Cookie cake candy canes. Pastry powder muffin soufflé tootsie roll sweet cookie tiramisu."
                    , "Candy cake danish gingerbread. Caramels toffee cupcake toffee sweet. Gummi bears candy cheesecake sweet. Pie gingerbread sugar plum halvah muffin icing marzipan wafer icing. Candy fruitcake gummies icing marzipan. Halvah jelly beans candy candy canes biscuit bonbon sesame snaps. Biscuit carrot cake croissant cake chocolate lollipop candy biscuit croissant. Topping jujubes apple pie croissant chocolate cake. Liquorice cookie dragée gummies cotton candy fruitcake lemon drops candy canes. Apple pie lemon drops gummies cake chocolate bar cake jelly-o tiramisu. Chocolate bar icing pudding marshmallow cake soufflé soufflé muffin. Powder lemon drops biscuit sugar plum cupcake carrot cake powder cake dragée. Bear claw gummi bears liquorice sweet roll."
                    ]
            )
        |> Control.field "Modal Attributes" controlAttributes


controlAttributes : Control (List Modal.Attribute)
controlAttributes =
    Control.record (\a b c -> a :: b :: c :: [])
        |> Control.field "Theme" controlTheme
        |> Control.field "Title visibility" controlTitleVisibility
        |> Control.field "Custom css" controlCss


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


controlCss : Control Modal.Attribute
controlCss =
    Control.map Modal.css <|
        Control.choice
            [ ( "[]", Control.value [] )
            , ( "[ Css.borderRadius Css.zero ]", Control.value [ Css.borderRadius Css.zero ] )
            , ( "[ Css.width (Css.px 900) ]", Control.value [ Css.width (Css.px 900) ] )
            ]


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.Modal.V11"
    , categories = [ Modals ]
    , atomicDesignType = Organism
    , keyboardSupport =
        [ { keys = [ KeyboardSupport.Tab ]
          , result = "Moves focus to the next button within the modal or wraps back to the first element within the modal."
          }
        , { keys = [ KeyboardSupport.Tab, KeyboardSupport.Shift ]
          , result = "Moves focus to the previous button within the modal or wraps back to the last element within the modal."
          }
        , { keys = [ KeyboardSupport.Esc ]
          , result = "If 'dismissOnEscAndOverlayClick' is set to true, closes the modal. Else, does nothing."
          }
        ]
    , state = init
    , update = update
    , subscriptions = subscriptions
    , view =
        \state ->
            let
                settings =
                    Control.currentValue state.settings
            in
            [ Control.view UpdateSettings state.settings
                |> Html.fromUnstyled
            , launchModalButton settings
            , Modal.view (modalSettings settings) settings.attributes state.state
            ]
    }


launchModalButton : Settings -> Html Msg
launchModalButton settings =
    let
        launchId =
            "launch-modal"

        startFocusId =
            if settings.showContinue then
                Just continueButtonId

            else if settings.showX then
                Just Modal.closeButtonId

            else if settings.showSecondary then
                Just closeClickableTextId

            else
                Nothing
    in
    Button.button "Launch Modal"
        [ case startFocusId of
            Just autofocusElementId ->
                Button.onClick
                    (OpenModal
                        { startFocusOn = autofocusElementId
                        , returnFocusTo = launchId
                        }
                    )

            Nothing ->
                Button.disabled
        , Button.custom [ Attributes.id launchId ]
        , Button.secondary
        ]


modalSettings :
    Settings
    ->
        { title : String
        , wrapMsg : Modal.Msg -> Msg
        , content : List (Html Msg)
        , footer : List (Html Msg)
        }
modalSettings settings =
    let
        default =
            { title = settings.title
            , wrapMsg = ModalMsg
            , content = []
            , footer = []
            }
    in
    case ( settings.showX, settings.showContinue, settings.showSecondary ) of
        ( True, True, True ) ->
            { default
                | content =
                    [ Modal.closeButton CloseModal <|
                        FocusTrap.first { focusLastId = Focus closeClickableTextId }
                    , viewModalContent settings.content
                    ]
                , footer =
                    [ continueButton []
                    , closeClickableText <|
                        FocusTrap.last { focusFirstId = Focus Modal.closeButtonId }
                    ]
            }

        ( True, False, True ) ->
            { default
                | content =
                    [ Modal.closeButton CloseModal <|
                        FocusTrap.first { focusLastId = Focus closeClickableTextId }
                    , viewModalContent settings.content
                    ]
                , footer =
                    [ closeClickableText <|
                        FocusTrap.last { focusFirstId = Focus Modal.closeButtonId }
                    ]
            }

        ( True, False, False ) ->
            { default
                | content =
                    [ Modal.closeButton CloseModal <|
                        FocusTrap.only { focusSelf = Focus closeClickableTextId }
                    , viewModalContent settings.content
                    ]
            }

        ( True, True, False ) ->
            { default
                | content =
                    [ Modal.closeButton CloseModal <|
                        FocusTrap.first { focusLastId = Focus closeClickableTextId }
                    , viewModalContent settings.content
                    ]
                , footer =
                    [ continueButton <|
                        FocusTrap.last { focusFirstId = Focus Modal.closeButtonId }
                    ]
            }

        ( False, True, True ) ->
            { default
                | content = [ viewModalContent settings.content ]
                , footer =
                    [ continueButton <|
                        FocusTrap.first { focusLastId = Focus closeClickableTextId }
                    , closeClickableText <|
                        FocusTrap.last { focusFirstId = Focus continueButtonId }
                    ]
            }

        ( False, False, True ) ->
            { default
                | content = [ viewModalContent settings.content ]
                , footer =
                    [ closeClickableText <|
                        FocusTrap.only { focusSelf = Focus closeClickableTextId }
                    ]
            }

        ( False, True, False ) ->
            { default
                | content = [ viewModalContent settings.content ]
                , footer =
                    [ continueButton <|
                        FocusTrap.only { focusSelf = Focus continueButtonId }
                    ]
            }

        ( False, False, False ) ->
            { default
                | content = [ viewModalContent settings.content ]
            }


viewModalContent : String -> Html msg
viewModalContent content =
    Text.mediumBody [ span [ css [ whiteSpace preLine ] ] [ text content ] ]


continueButtonId : String
continueButtonId =
    "continue-button-id"


continueButton : List (Html.Attribute Msg) -> Html Msg
continueButton attributes =
    Button.button "Continue"
        [ Button.premium
        , Button.onClick CloseModal
        , Button.custom (Attributes.id continueButtonId :: attributes)
        , Button.large
        ]


closeClickableTextId : String
closeClickableTextId =
    "continue-button-id"


closeClickableText : List (Html.Attribute Msg) -> Html Msg
closeClickableText attributes =
    ClickableText.button "Close"
        [ ClickableText.onClick CloseModal
        , ClickableText.large
        , ClickableText.custom (Attributes.id closeClickableTextId :: attributes)
        , ClickableText.css [ Css.marginTop (Css.px 15) ]
        ]


{-| -}
type Msg
    = OpenModal { startFocusOn : String, returnFocusTo : String }
    | ModalMsg Modal.Msg
    | CloseModal
    | UpdateSettings (Control Settings)
    | Focus String
    | Focused (Result Dom.Error ())


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
        OpenModal config ->
            let
                ( newState, cmd ) =
                    Modal.open config
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

        CloseModal ->
            let
                ( newState, cmd ) =
                    Modal.close state.state
            in
            ( { state | state = newState }
            , Cmd.map ModalMsg cmd
            )

        UpdateSettings value ->
            ( { state | settings = value }, Cmd.none )

        Focus id ->
            ( state, Task.attempt Focused (Dom.focus id) )

        Focused _ ->
            ( state, Cmd.none )


{-| -}
subscriptions : State -> Sub Msg
subscriptions model =
    Sub.map ModalMsg (Modal.subscriptions model.state)
