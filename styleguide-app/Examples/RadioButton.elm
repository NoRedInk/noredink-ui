module Examples.RadioButton exposing
    ( example
    , State, Msg
    )

{-|

@docs example
@docs State, Msg

-}

import Browser.Dom as Dom
import Category exposing (Category(..))
import Css exposing (..)
import Debug.Control as Control exposing (Control)
import Dict exposing (Dict)
import Example exposing (Example)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Data.PremiumLevel as PremiumLevel exposing (PremiumLevel)
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Modal.V11 as Modal
import Nri.Ui.RadioButton.V3 as RadioButton
import Nri.Ui.Text.V6 as Text
import Task


{-| -}
example : Example State Msg
example =
    { name = "RadioButton"
    , version = 3
    , state = init
    , update = update
    , subscriptions = subscriptions
    , preview = []
    , view = view
    , categories = [ Inputs ]
    , keyboardSupport =
        [ { keys = [ Arrow Left ]
          , result = "Move the focus & select the radio button to the left"
          }
        , { keys = [ Arrow Right ]
          , result = "Move the focus & select the radio button to the right"
          }
        , { keys = [ Space ]
          , result = "Select the current radio button"
          }
        ]
    }


{-| -}
view : State -> List (Html Msg)
view model =
    [ Heading.h3 [] [ Html.text "RadioButton" ]
    , Heading.h4 [] [ Html.text "view" ]
    , viewVanilla model
    , Heading.h4 [] [ Html.text "premium" ]
    , viewPremium model
    , Modal.view
        { title = "Go Premium!"
        , wrapMsg = ModalMsg
        , content = [ Text.mediumBody [ Text.plaintext "Often, we'll launch a modal showing the benefits of premium when a locked radio button is clicked." ] ]
        , footer =
            [ Button.button "Okay"
                [ Button.modal
                , Button.onClick CloseModal
                , Button.id "close-premium-modal"
                ]
            ]
        , focusTrap =
            { focus = Focus
            , firstId = Modal.closeButtonId
            , lastId = "close-premium-modal"
            }
        }
        [ Modal.closeButton ]
        model.modal
    ]


viewVanilla : State -> Html Msg
viewVanilla state =
    div []
        [ RadioButton.view
            { label = "Cats"
            , name = "radio-button-examples"
            , selectedValue = state.selectedValue
            , onSelect = Debug.log "selected" >> Select
            , valueToString = identity
            }
            [ RadioButton.enabled
            , RadioButton.value "Felines"
            , RadioButton.name "radio-button-examples"
            ]
        , RadioButton.view
            { label = "Dogs"
            , name = "radio-button-examples"
            , selectedValue = state.selectedValue
            , onSelect = Debug.log "selected" >> Select
            , valueToString = identity
            }
            [ RadioButton.enabled
            , RadioButton.value "Canines"
            ]
        ]


viewPremium : State -> Html Msg
viewPremium state =
    let
        premiumConfig =
            Control.currentValue state.premiumControl
    in
    div []
        [ Heading.h4 [] [ Html.text "Premium Radio Buttons" ]
        , Html.div [ css [ Css.margin (Css.px 8) ] ]
            [ Control.view SetPremiumControl state.premiumControl
                |> Html.fromUnstyled
            ]
        , RadioButton.premium
            { label = "Hedgehog (Free)"
            , value = "Hedgehogs"
            , name = "radio-button-examples"
            , selectedValue = state.selectedValue
            , teacherPremiumLevel = premiumConfig.teacherPremiumLevel
            , contentPremiumLevel = PremiumLevel.Free
            , onSelect = Select

            -- TODO:
            -- the next version of the RadioComponent will handle focus correctly,
            -- including re-capturing the focus when the modal closes.
            -- While we could change premiumMsg to be String -> msg now,
            -- and use the correct id, there's not much point in doing
            -- so yet since the radio doesn't handle focus correctly.
            , premiumMsg = OpenModal "hedgehogs-free"
            , valueToString = identity
            , showPennant = premiumConfig.showPennant
            , isDisabled = False
            }
        , RadioButton.premium
            { label = "Hedgehodge (Premium)"
            , value = "Hedgehodges"
            , name = "radio-button-examples"
            , selectedValue = state.selectedValue
            , teacherPremiumLevel = premiumConfig.teacherPremiumLevel
            , contentPremiumLevel = PremiumLevel.PremiumWithWriting
            , onSelect = Select

            -- TODO:
            -- the next version of the RadioComponent will handle focus correctly,
            -- including re-capturing the focus when the modal closes.
            -- While we could change premiumMsg to be String -> msg now,
            -- and use the correct id, there's not much point in doing
            -- so yet since the radio doesn't handle focus correctly.
            , premiumMsg = OpenModal "hedgehogs-premium"
            , valueToString = identity
            , showPennant = premiumConfig.showPennant
            , isDisabled = False
            }
        , RadioButton.premium
            { label = "Disabled"
            , value = "Disabled"
            , name = "radio-button-examples"
            , selectedValue = state.selectedValue
            , teacherPremiumLevel = premiumConfig.teacherPremiumLevel
            , contentPremiumLevel = PremiumLevel.PremiumWithWriting
            , onSelect = Select

            -- TODO:
            -- the next version of the RadioComponent will handle focus correctly,
            -- including re-capturing the focus when the modal closes.
            -- While we could change premiumMsg to be String -> msg now,
            -- and use the correct id, there's not much point in doing
            -- so yet since the radio doesn't handle focus correctly.
            , premiumMsg = OpenModal "hedgehogs-premium"
            , valueToString = identity
            , showPennant = premiumConfig.showPennant
            , isDisabled = True
            }
        ]


{-| -}
type alias State =
    { selectedValue : Maybe String
    , modal : Modal.Model
    , premiumControl : Control PremiumConfig
    }


{-| -}
init : State
init =
    { selectedValue = Nothing
    , modal = Modal.init
    , premiumControl = initPremiumControls
    }


type alias PremiumConfig =
    { teacherPremiumLevel : PremiumLevel
    , showPennant : Bool
    }


initPremiumControls : Control PremiumConfig
initPremiumControls =
    Control.record PremiumConfig
        |> Control.field "teacherPremiumLevel"
            (Control.choice
                [ ( "Free", Control.value PremiumLevel.Free )
                , ( "Premium", Control.value PremiumLevel.PremiumWithWriting )
                ]
            )
        |> Control.field "showPennant" (Control.bool False)


type Msg
    = OpenModal String
    | ModalMsg Modal.Msg
    | CloseModal
    | Select String
    | SetPremiumControl (Control PremiumConfig)
    | Focus String
    | Focused (Result Dom.Error ())


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        OpenModal returnFocusTo ->
            let
                ( modal, cmd ) =
                    Modal.open
                        { startFocusOn = Modal.closeButtonId
                        , returnFocusTo = returnFocusTo
                        }
            in
            ( { model | modal = modal }, Cmd.map ModalMsg cmd )

        ModalMsg modalMsg ->
            let
                ( modal, cmd ) =
                    Modal.update { dismissOnEscAndOverlayClick = True }
                        modalMsg
                        model.modal
            in
            ( { model | modal = modal }, Cmd.map ModalMsg cmd )

        CloseModal ->
            let
                ( modal, cmd ) =
                    Modal.close model.modal
            in
            ( { model | modal = modal }, Cmd.map ModalMsg cmd )

        Select value ->
            ( { model | selectedValue = Just value }, Cmd.none )

        SetPremiumControl premiumControl ->
            ( { model | premiumControl = premiumControl }, Cmd.none )

        Focus focus ->
            ( model, Task.attempt Focused (Dom.focus focus) )

        Focused _ ->
            ( model, Cmd.none )


subscriptions : State -> Sub Msg
subscriptions { modal } =
    Sub.map ModalMsg (Modal.subscriptions modal)
