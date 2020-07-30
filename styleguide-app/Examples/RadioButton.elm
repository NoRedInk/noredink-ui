module Examples.RadioButton exposing
    ( example
    , State, Msg
    )

{-|

@docs example
@docs State, Msg

-}

import AtomicDesignType exposing (AtomicDesignType(..))
import Category exposing (Category(..))
import Css exposing (..)
import Debug.Control as Control exposing (Control)
import Dict exposing (Dict)
import Example exposing (Example)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Data.PremiumLevel as PremiumLevel exposing (PremiumLevel)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.RadioButton.V1 as RadioButton
import Nri.Ui.Text.V4 as Text


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.RadioButton.V1"
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    , categories = [ Layout ]
    , atomicDesignType = Atom
    , keyboardSupport = []
    }


{-| -}
view : State -> List (Html Msg)
view model =
    [ Heading.h3 [] [ Html.text "RadioButton" ]
    , Heading.h4 [] [ Html.text "view" ]
    , viewVanilla model
    , viewInvisibleLabel model
    , Heading.h4 [] [ Html.text "premium" ]
    , viewPremium model
    ]


viewVanilla : State -> Html Msg
viewVanilla state =
    div [ css [ Css.margin (Css.px 8) ] ]
        [ RadioButton.view
            { label = "Cats"
            , showLabel = True
            , value = "Cats"
            , name = "radio-button-examples"
            , selectedValue = state.selectedValue
            , onSelect = Select
            , noOpMsg = NoOp
            , valueToString = identity
            }
        , RadioButton.view
            { label = "Dogs"
            , showLabel = True
            , value = "Dogs"
            , name = "radio-button-examples"
            , selectedValue = state.selectedValue
            , onSelect = Select
            , noOpMsg = NoOp
            , valueToString = identity
            }
        ]


viewInvisibleLabel : State -> Html Msg
viewInvisibleLabel state =
    div [ css [ Css.margin (Css.px 8) ] ]
        [ Heading.h4 [] [ Html.text "Invisible Label" ]
        , RadioButton.view
            { label = "Shh"
            , showLabel = False
            , value = "I'm a secret... but not to screen readers"
            , name = "Secret"
            , selectedValue = state.selectedValue
            , onSelect = Select
            , noOpMsg = NoOp
            , valueToString = identity
            }
        ]


viewPremium : State -> Html Msg
viewPremium state =
    let
        premiumConfig =
            Control.currentValue state.premiumControl
    in
    div [ css [ Css.margin (Css.px 8) ] ]
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
            , premiumMsg = ShowModal True
            , noOpMsg = NoOp
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
            , premiumMsg = ShowModal True
            , noOpMsg = NoOp
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
            , premiumMsg = ShowModal True
            , noOpMsg = NoOp
            , valueToString = identity
            , showPennant = premiumConfig.showPennant
            , isDisabled = True
            }
        ]


{-| -}
type alias State =
    { selectedValue : Maybe String
    , isModalShowing : Bool
    , premiumControl : Control PremiumConfig
    }


{-| -}
init : State
init =
    { selectedValue = Nothing
    , isModalShowing = False
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
    = ShowModal Bool
    | Select String
    | SetPremiumControl (Control PremiumConfig)
    | NoOp


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        ShowModal isModalShowing ->
            ( { model | isModalShowing = isModalShowing }, Cmd.none )

        Select value ->
            ( { model | selectedValue = Just value }, Cmd.none )

        SetPremiumControl premiumControl ->
            ( { model | premiumControl = premiumControl }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )
