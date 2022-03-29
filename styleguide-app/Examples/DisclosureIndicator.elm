module Examples.DisclosureIndicator exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Css exposing (Style)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import Nri.Ui.DisclosureIndicator.V2 as DisclosureIndicator
import Nri.Ui.Text.V6 as Text


moduleName : String
moduleName =
    "DisclosureIndicator"


version : Int
version =
    2


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
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
        \ellieLinkConfig state ->
            let
                attributes =
                    Control.currentValue state.settings
            in
            [ Text.smallBodyGray [ Text.plaintext "The disclosure indicator is only the caret. It is NOT a button -- you must create a button or clickabletext yourself!" ]
            , ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateSettings
                , settings = state.settings
                , toExampleCode =
                    \settings ->
                        let
                            toCode viewName =
                                moduleName
                                    ++ "."
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
            , Html.div [ css [ Css.displayFlex, Css.alignItems Css.center, Css.marginBottom (Css.px 8) ] ]
                [ DisclosureIndicator.large
                    (Tuple.second attributes.css)
                    (Tuple.second attributes.isOpen)
                , Html.text "large is a 17px caret icon."
                ]
            , Html.div [ css [ Css.displayFlex, Css.alignItems Css.center, Css.marginBottom (Css.px 8) ] ]
                [ DisclosureIndicator.medium
                    (Tuple.second attributes.css)
                    (Tuple.second attributes.isOpen)
                , Html.text "medium is a 15px caret icon."
                ]
            ]
    }


{-| -}
type alias State =
    { settings : Control Settings
    }


{-| -}
init : State
init =
    { settings = initSettings
    }


type alias Settings =
    { css : ( String, List Style )
    , isOpen : ( String, Bool )
    }


initSettings : Control Settings
initSettings =
    Control.record Settings
        |> Control.field "css"
            (Control.choice
                [ ( "[ Css.marginRight (Css.px 8) ]"
                  , Control.value
                        ( "[ Css.marginRight (Css.px 8) ]"
                        , [ Css.marginRight (Css.px 8) ]
                        )
                  )
                , ( "[]", Control.value ( "[]", [] ) )
                ]
            )
        |> Control.field "isOpen" (ControlExtra.bool False)


{-| -}
type Msg
    = UpdateSettings (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateSettings settings ->
            ( { state | settings = settings }, Cmd.none )
