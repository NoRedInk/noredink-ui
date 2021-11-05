module Examples.Switch exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category
import Example exposing (Example)
import Html.Styled as Html
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Switch.V1 as Switch
import Nri.Ui.Text.V6 as Text


{-| -}
type alias State =
    Bool


{-| -}
type Msg
    = Switch Bool


example : Example State Msg
example =
    { name = "Switch"
    , version = 1
    , state = True
    , update = \(Switch new) _ -> ( new, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , preview = []
    , view =
        \interactiveIsOn ->
            [ Heading.h3 [] [ Html.text "Interactive" ]
            , Text.mediumBody
                [ Text.html
                    [ Switch.view
                        [ Switch.onSwitch Switch
                        , Switch.id "switch-interactive"
                        , Switch.label
                            (if interactiveIsOn then
                                Html.text "On"

                             else
                                Html.text "Off"
                            )
                        ]
                        interactiveIsOn
                    ]
                ]
            , Heading.h3 [] [ Html.text "Disabled" ]
            , Text.mediumBody
                [ Text.html
                    [ Switch.view
                        [ Switch.disabled
                        , Switch.id "switch-disabled-on"
                        , Switch.label (Html.text "Permanently on")
                        ]
                        True
                    ]
                ]
            , Text.mediumBody
                [ Text.html
                    [ Switch.view
                        [ Switch.disabled
                        , Switch.id "switch-disabled-off"
                        , Switch.label (Html.text "Permanently off")
                        ]
                        False
                    ]
                ]
            ]
    , categories = [ Category.Inputs ]
    , keyboardSupport = [{- TODO -}]
    }
