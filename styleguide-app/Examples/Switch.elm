module Examples.Switch exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled.Key as Key
import Category
import Example exposing (Example)
import Html.Styled as Html
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Switch.V1 as Switch


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
    , preview =
        [ Switch.view
            [ Switch.label (Html.text "Toggle On")
            , Switch.custom [ Key.tabbable False ]
            ]
            False
        , Switch.view
            [ Switch.label (Html.text "Toggle Off")
            , Switch.custom [ Key.tabbable False ]
            ]
            True
        ]
    , view =
        \ellieLinkConfig interactiveIsOn ->
            [ Heading.h3 [] [ Html.text "Interactive" ]
            , Switch.view
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
            , Heading.h3 [] [ Html.text "Disabled (On)" ]
            , Switch.view
                [ Switch.disabled
                , Switch.id "switch-disabled-on"
                , Switch.label (Html.text "Permanently on")
                ]
                True
            , Heading.h3 [] [ Html.text "Disabled (Off)" ]
            , Switch.view
                [ Switch.disabled
                , Switch.id "switch-disabled-off"
                , Switch.label (Html.text "Permanently off")
                ]
                False
            ]
    , categories = [ Category.Inputs ]
    , keyboardSupport = [{- TODO -}]
    }
