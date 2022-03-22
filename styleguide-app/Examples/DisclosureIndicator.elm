module Examples.DisclosureIndicator exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Css exposing (Style)
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.DisclosureIndicator.V2 as DisclosureIndicator
import Nri.Ui.Text.V6 as Text


{-| -}
example : Example State Msg
example =
    { name = "DisclosureIndicator"
    , version = 2
    , categories = [ Icons ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ DisclosureIndicator.medium [] False
        , DisclosureIndicator.medium [] True
        , DisclosureIndicator.large [] False
        , DisclosureIndicator.large [] True
        ]
    , view =
        \state ->
            [ Text.smallBodyGray [ Text.plaintext "The disclosure indicator is only the caret. It is NOT a button -- you must create a button or clickabletext yourself!" ]
            , ControlView.view
                { update = UpdateSettings
                , settings = state.settings
                , toExampleCode =
                    \settings ->
                        let
                            toCode viewName =
                                "DisclosureIndicator."
                                    ++ viewName
                                    ++ " "
                                    ++ Tuple.first settings.css
                                    ++ " "
                                    ++ Tuple.first settings.isOpen
                        in
                        [ { sectionName = "Large"
                          , code = toCode "large"
                          }
                        , { sectionName = "medium"
                          , code = toCode "medium"
                          }
                        ]
                }
            , Html.div [ css [ Css.displayFlex, Css.padding (Css.px 8) ] ]
                [ Button.button "Toggle large indicator"
                    [ Button.onClick ToggleLarge, Button.small, Button.secondary ]
                , Button.button "Toggle medium indicator"
                    [ Button.onClick ToggleMedium, Button.small, Button.secondary ]
                ]
            , Html.div [ css [ Css.displayFlex, Css.alignItems Css.center, Css.marginBottom (Css.px 8) ] ]
                [ DisclosureIndicator.large [ Css.marginRight (Css.px 10) ] state.largeState
                , Html.text "I'm a 17px caret icon."
                ]
            , Html.div [ css [ Css.displayFlex, Css.alignItems Css.center, Css.marginBottom (Css.px 8) ] ]
                [ DisclosureIndicator.medium [ Css.paddingRight (Css.px 8) ] state.mediumState
                , Html.text "I'm a 15px caret icon."
                ]
            ]
    }


{-| -}
type alias State =
    { settings : Control Settings
    , largeState : Bool
    , mediumState : Bool
    }


{-| -}
init : State
init =
    { settings = initSettings
    , largeState = False
    , mediumState = False
    }


type alias Settings =
    { css : ( String, List Style )
    , isOpen : ( String, Bool )
    }


initSettings : Control Settings
initSettings =
    Control.record Settings
        |> Control.field "css" (Control.value ( "[]", [] ))
        |> Control.field "isOpen" (Control.value ( "False", False ))


{-| -}
type Msg
    = UpdateSettings (Control Settings)
    | ToggleLarge
    | ToggleMedium


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateSettings settings ->
            ( { state | settings = settings }, Cmd.none )

        ToggleLarge ->
            ( { state | largeState = not state.largeState }, Cmd.none )

        ToggleMedium ->
            ( { state | mediumState = not state.mediumState }, Cmd.none )
